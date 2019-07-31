{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Estuary.Render.Renderer where

import Data.Time.Clock
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Loops
import Data.Functor (void)
import Data.List (intercalate,zipWith4)
import Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Either
import qualified Data.Map as Map
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Bifunctor

import Sound.MusicW.AudioContext
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.Evaluation as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.Types as Punctual
import qualified Sound.Punctual.Parser as Punctual

import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Tidal.Types
import Estuary.Tidal.ParamPatternable
import Estuary.Types.Live
import Estuary.Languages.TidalParsers
import Estuary.WebDirt.SampleEngine
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Types.Tempo
import Estuary.Types.MovingAverage
import Estuary.Render.AudioContext

type Renderer = StateT RenderState IO ()

renderPeriod :: NominalDiffTime
renderPeriod = 0.032

-- flush events for SuperDirt and WebDirt
flushEvents :: Context -> Renderer
flushEvents c = do
  events <- gets dirtEvents
  when (webDirtOn c) $ do
    let events' = fmap (first (utcTimeToAudioSeconds $ t0 c)) events
    liftIO $ sendSoundsAudio (webDirt c) events'
  when (superDirtOn c) $ liftIO $ sendSoundsUtc (superDirt c) events
  modify' $ \x -> x { dirtEvents = [] }
  return ()

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ControlMap)]
renderTidalPattern start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (at t)) * cps t + beat t -- start time in cycles since beginning of tempo
    end = realToFrac range * cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.queryArc p (Tidal.Arc (toRational start') (toRational end)) -- events with t in cycles
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - beat t)/cps t)) (at t)
        w1 = Tidal.start $ Tidal.whole e

sequenceToControlPattern :: (Text,[Bool]) -> Tidal.ControlPattern
sequenceToControlPattern (sampleName,pat) = Tidal.s $ parseBP' $ intercalate " " $ fmap f pat
  where f False = "~"
        f True = T.unpack sampleName

render :: Context -> Renderer
render c = do
  t1 <- liftIO $ getCurrentTime
  s <- get
  when (canvasElement c /= cachedCanvasElement s) $ do
    liftIO $ putStrLn "render: canvasElement new/changed"
    traverseWithKey (canvasChanged c) (zones $ ensemble $ ensembleC c)
    modify' $ \x -> x { cachedCanvasElement = canvasElement c }
  traverseWithKey (renderZone c) (zones $ ensemble $ ensembleC c)
  flushEvents c
  t2 <- liftIO $ getCurrentTime
  modify' $ \x -> x { renderStartTime = t1, renderEndTime = t2 }
  calculateRenderTimes
  scheduleNextRender

canvasChanged :: Context -> Int -> Definition -> Renderer
canvasChanged c z (TextProgram x) = canvasChangedTextProgram c z $ forRendering x
canvasChanged _ _ _ = return ()

canvasChangedTextProgram :: Context -> Int -> (TextNotation,Text) -> Renderer
canvasChangedTextProgram c z (Punctual,x) = do
  webGLs <- gets punctualWebGLs
  let prevWebGL = IntMap.findWithDefault Punctual.emptyPunctualWebGL z webGLs
  liftIO $ T.putStrLn "about to Punctual.updateRenderingContext"
  newWebGL <- liftIO $ Punctual.updateRenderingContext prevWebGL (canvasElement c)
  modify' $ \x -> x { punctualWebGLs = insert z newWebGL webGLs }
canvasChangedTextProgram _ _ _ = return ()

renderZone :: Context -> Int -> Definition -> Renderer
renderZone c z d = do
  t1 <- liftIO $ getCurrentTime
  s <- get
  let prevDef = IntMap.lookup z $ cachedDefs s
  let d' = definitionForRendering d
  when (prevDef /= (Just d')) $ do
    renderZoneChanged c z d'
    modify' $ \x -> x { cachedDefs = insert z d' (cachedDefs s) }
  renderZoneAlways c z d'
  t2 <- liftIO $ getCurrentTime
  let prevZoneRenderTimes = findWithDefault (newAverage 20) z $ zoneRenderTimes s
  let newZoneRenderTimes = updateAverage prevZoneRenderTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneRenderTimes = insert z newZoneRenderTimes (zoneRenderTimes s) }

renderAnimation :: Context -> Renderer
renderAnimation c = do
  tNow <- liftAudioIO $ audioTime
  traverseWithKey (renderZoneAnimation tNow c) (zones $ ensemble $ ensembleC c)
  return ()

renderZoneAnimation :: Double -> Context -> Int -> Definition -> Renderer
renderZoneAnimation tNow c z (TextProgram x) = do
  s <- get
  t1 <- liftIO $ getCurrentTime
  renderZoneAnimationTextProgram tNow c z $ forRendering x
  t2 <- liftIO $ getCurrentTime
  let prevZoneAnimationTimes = findWithDefault (newAverage 20) z $ zoneAnimationTimes s
  let newZoneAnimationTimes = updateAverage prevZoneAnimationTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneAnimationTimes = insert z newZoneAnimationTimes (zoneAnimationTimes s) }
  return ()
renderZoneAnimation _ _ _ _ = return ()

renderZoneAnimationTextProgram :: Double -> Context -> Int -> (TextNotation,Text) -> Renderer
renderZoneAnimationTextProgram tNow c z (Punctual,x) = do
  webGLs <- gets punctualWebGLs
  let webGL = findWithDefault Punctual.emptyPunctualWebGL z webGLs
  liftIO $ Punctual.drawFrame tNow webGL
renderZoneAnimationTextProgram _ _ _ _ = return ()

renderZoneChanged :: Context -> Int -> Definition -> Renderer
renderZoneChanged c z (Structure x) = do
  let newParamPattern = toParamPattern x
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged c z (TextProgram x) = do
  liftIO $ putStrLn $ "zone " ++ show z ++ " TextProgram changed, type = " ++ show x
  renderTextProgramChanged c z $ forRendering x
renderZoneChanged c z (Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged _ _ _ = return ()

renderZoneAlways :: Context -> Int -> Definition -> Renderer
renderZoneAlways c z (Structure _) = renderControlPattern c z
renderZoneAlways c z (TextProgram x) = renderTextProgramAlways c z $ forRendering x
renderZoneAlways c z (Sequence _) = renderControlPattern c z
renderZoneAlways _ _ _ = return ()


renderTextProgramChanged :: Context -> Int -> (TextNotation,Text) -> Renderer

renderTextProgramChanged c z (TidalTextNotation x,y) = do
  s <- get
  let parseResult = tidalParser x y -- :: Either ParseError ControlPattern
  let newParamPatterns = either (const $ paramPatterns s) (\p -> insert z p (paramPatterns s)) parseResult
  liftIO $ either (putStrLn) (const $ return ()) parseResult -- print new errors to console
  let newErrors = either (\e -> insert z (T.pack e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { paramPatterns = newParamPatterns, info = (info s) { errors = newErrors} }

renderTextProgramChanged c z (Punctual,x) = do
  s <- get
  ac <- liftAudioIO $ audioContext
  let parseResult = Punctual.runPunctualParser x
  if isLeft parseResult then return () else do
    -- A. update PunctualW (audio state) in response to new, syntactically correct program
    let exprs = either (const []) id parseResult
    let t = logicalTime s
    let eval = (exprs,t)
    let (mainBusIn,_,_,_) = mainBus c
    let prevPunctualW = findWithDefault (Punctual.emptyPunctualW ac mainBusIn 2 t) z (punctuals s)
    let tempo' = tempo $ ensemble $ ensembleC c
    let beat0 = beatZero tempo'
    let cps' = cps tempo'
    newPunctualW <- liftAudioIO $ Punctual.updatePunctualW prevPunctualW (beat0,realToFrac cps') eval
    modify' $ \x -> x { punctuals = insert z newPunctualW (punctuals s)}
    -- B. update Punctual WebGL state in response to new, syntactically correct program
    webGLs <- gets punctualWebGLs
    let prevWebGL = IntMap.lookup z webGLs
    prevWebGL' <- if isJust prevWebGL then return (fromJust prevWebGL) else
      liftIO $ Punctual.updateRenderingContext Punctual.emptyPunctualWebGL (canvasElement c)
    newWebGL <- liftIO $ Punctual.evaluatePunctualWebGL prevWebGL' (beat0,realToFrac cps') eval
    modify' $ \x -> x { punctualWebGLs = insert z newWebGL webGLs }
  let newErrors = either (\e -> insert z (T.pack $ show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { info = (info s) { errors = newErrors }}

renderTextProgramChanged c z (CineCer0,x) = do
  s <- get
  let parseResult :: Either String CineCer0.CineCer0Spec = CineCer0.cineCer0 $ T.unpack x -- Either String CineCer0Spec
  let maybeTheDiv = videoDivElement c
  when (isJust maybeTheDiv && isRight parseResult) $ do
    let spec :: CineCer0.CineCer0Spec = fromRight (IntMap.empty) parseResult
    let theDiv = fromJust maybeTheDiv
    let prevState = IntMap.findWithDefault (CineCer0.emptyCineCer0State theDiv) z $ cineCer0States s
    liftIO $ putStrLn $ show parseResult
    let t = tempo $ ensemble $ ensembleC c
    let now = logicalTime s
    newState <- liftIO $ CineCer0.updateCineCer0State t now spec prevState
    modify' $ \x -> x { cineCer0States = insert z newState (cineCer0States s) }
  when (isLeft parseResult) $ do
    let errs = either (\e -> insert z (T.pack $ show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
    modify' $ \x -> x { info = (info s) { errors = errs }}

renderTextProgramChanged _ _ _ = return ()

renderTextProgramAlways :: Context -> Int -> (TextNotation,Text) -> Renderer
renderTextProgramAlways c z (TidalTextNotation _,_) = renderControlPattern c z
renderTextProgramAlways _ _ _ = return ()

renderControlPattern :: Context -> Int -> Renderer
renderControlPattern c z = when (webDirtOn c || superDirtOn c) $ do
  s <- get
  let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
  let lt = logicalTime s
  let tempo' = tempo $ ensemble $ ensembleC c
  let events = maybe [] id $ fmap (renderTidalPattern lt renderPeriod tempo') controlPattern
  modify' $ \x -> x { dirtEvents = (dirtEvents s) ++ events }

calculateRenderTimes :: Renderer
calculateRenderTimes = do
  s <- get
  --
  let mostRecentRenderTime = diffUTCTime (renderEndTime s) (renderStartTime s)
  let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
  let newAvgRenderLoad = ceiling (getAverage newRenderTime * 100 / realToFrac renderPeriod)
  let newPeakRenderLoad = ceiling (getPeak newRenderTime * 100 / realToFrac renderPeriod)
  modify' $ \x -> x { renderTime = newRenderTime }
  modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
  traverseWithKey calculateZoneRenderTimes $ zoneRenderTimes s
  traverseWithKey calculateZoneAnimationTimes $ zoneAnimationTimes s
  return ()

calculateZoneRenderTimes :: Int -> MovingAverage -> Renderer
calculateZoneRenderTimes z zrt = do
  s <- get
  let newAvgMap = insert z (getAverage zrt) (avgZoneRenderTime $ info s)
  modify' $ \x -> x { info = (info x) { avgZoneRenderTime = newAvgMap }}

calculateZoneAnimationTimes :: Int -> MovingAverage -> Renderer
calculateZoneAnimationTimes z zat = do
  s <- get
  let newAvgMap = insert z (getAverage zat) (avgZoneAnimationTime $ info s)
  modify' $ \x -> x { info = (info x) { avgZoneAnimationTime = newAvgMap }}

scheduleNextRender :: Renderer
scheduleNextRender = do
  s <- get
  let next = addUTCTime renderPeriod (logicalTime s)
  tNow <- liftIO $ getCurrentTime
  let diff = diffUTCTime next tNow
  -- if next logical time is more than 0.2 seconds in the past or future
  -- fast-forward or rewind by half of the difference
  let adjustment = if diff >= (-0.2) && diff <= 0.2 then 0 else (diff*(-1))
  when (diff < (-0.2)) $ liftIO $ putStrLn $ "fast forwarding by " ++ show adjustment
  when (diff > 0.2) $ liftIO $ putStrLn $ "rewinding by " ++ show adjustment
  let next' = addUTCTime adjustment next
  put $ s { logicalTime = next' }

-- if the (potentially adjusted) next logical time is more than a half period from now
-- sleep (threadDelay) so that next logical time is approximately a half period from then
sleepIfNecessary :: Renderer
sleepIfNecessary = do
  next <- gets logicalTime
  tNow <- liftIO $ getCurrentTime
  let halfPeriod = renderPeriod / 2
  let diff = diffUTCTime next tNow
  when (diff > halfPeriod) $ do
    let wakeTime = addUTCTime (0-halfPeriod) next
    let delay = diffUTCTime wakeTime tNow
    liftIO $ threadDelay $ floor $ realToFrac $ delay * 1000000

forkRenderThreads :: MVar Context -> MVar RenderInfo -> IO ()
forkRenderThreads ctxM riM = do
  renderStart <- getCurrentTime
  irs <- initialRenderState renderStart
  rsM <- newMVar irs
  void $ forkIO $ mainRenderThread ctxM riM rsM
  void $ forkIO $ animationThread ctxM riM rsM

mainRenderThread :: MVar Context -> MVar RenderInfo -> MVar RenderState -> IO ()
mainRenderThread ctxM riM rsM = do
  ctx <- readMVar ctxM
  rs <- readMVar rsM
  rs' <- execStateT (render ctx) rs
  swapMVar rsM rs'
  swapMVar riM (info rs') -- copy RenderInfo from state into MVar for instant reading elsewhere
  execStateT sleepIfNecessary rs'
  mainRenderThread ctxM riM rsM

animationThread :: MVar Context -> MVar RenderInfo -> MVar RenderState -> IO ()
animationThread ctxM riM rsM = void $ inAnimationFrame ThrowWouldBlock $ \_ -> do
  ctx <- readMVar ctxM
  when (canvasOn ctx) $ do
    rs <- readMVar rsM
    _ <- execStateT (renderAnimation ctx) rs
    -- putMVar rsM rs'
    return ()
  animationThread ctxM riM rsM
