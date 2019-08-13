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
import qualified Data.Map.Strict as Map
import JavaScript.Web.AnimationFrame
import GHCJS.Concurrent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Bifunctor
import TextShow
import Sound.OSC.Datum
import Text.Parsec

import Sound.MusicW.AudioContext
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.Evaluation as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.Types as Punctual
import qualified Sound.Punctual.Parser as Punctual
import qualified Sound.TimeNot.MapEstuary as TimeNot

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
import Estuary.Languages.TextReplacement
import Estuary.WebDirt.SampleEngine
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Types.Tempo
import Estuary.Types.MovingAverage

type Renderer = StateT RenderState IO ()

clockRatioThreshold :: Double
clockRatioThreshold = 0.6

maxRenderLatency :: NominalDiffTime
maxRenderLatency = 0.240

maxRenderPeriod :: NominalDiffTime
maxRenderPeriod = 0.240

minRenderLatency :: NominalDiffTime
minRenderLatency = 0.120

minRenderPeriod :: NominalDiffTime
minRenderPeriod = 0.120

-- should be somewhat larger than maxRenderLatency
waitThreshold :: NominalDiffTime
waitThreshold = 0.260

rewindThreshold :: NominalDiffTime
rewindThreshold = 1.0

earlyWakeUp :: NominalDiffTime
earlyWakeUp = 0.002


getRenderTime :: Context -> StateT RenderState IO UTCTime
getRenderTime c = do
  tAudio <- liftAudioIO $ audioTime
  return $ audioSecondsToUTC (clockDiff c) tAudio

mapTextDatumToControlMap :: Map.Map Text Datum -> Tidal.ControlMap
mapTextDatumToControlMap m = Map.mapKeys T.unpack $ Map.mapMaybe datumToValue m

datumToValue :: Datum -> Maybe Tidal.Value
datumToValue (Int32 x) = Just $ Tidal.VI $ fromIntegral x
datumToValue (Int64 x) = Just $ Tidal.VI $ fromIntegral x
datumToValue (Float x) = Just $ Tidal.VF $ realToFrac x
datumToValue (Double x) = Just $ Tidal.VF $ x
datumToValue (ASCII_String x) = Just $ Tidal.VS $ ascii_to_string x
datumToValue _ = Nothing

-- flush events for SuperDirt and WebDirt
flushEvents :: Context -> Renderer
flushEvents c = do
  events <- gets dirtEvents
  when (webDirtOn c) $ do
    let events' = fmap (first (utcTimeToAudioSeconds $ clockDiff c)) events
    liftIO $ sendSoundsAudio (webDirt c) events'
  when (superDirtOn c) $ liftIO $ sendSoundsPOSIX (superDirt c) events
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
  s <- get
  -- check if audio clock has advanced same amount as system clock
  t1System <- liftIO $ getCurrentTime
  t1Audio <- liftAudioIO $ audioTime
  let elapsedSystem = (realToFrac $ diffUTCTime t1System $ wakeTimeSystem s) :: Double
  let elapsedAudio = t1Audio - wakeTimeAudio s
  let cr = elapsedAudio / elapsedSystem
  let crProblem = cr < clockRatioThreshold && cr > 0
  modify' $ \x -> x {
    wakeTimeSystem = t1System,
    wakeTimeAudio = t1Audio,
    info = (info x) { clockRatio = cr, clockRatioProblem = crProblem }
  }
  when crProblem $ liftIO $ T.putStrLn $ "audio clock SLOW, ratio=" <> showt cr

  -- four possible timing scenarios to account for...
  let diff = diffUTCTime (renderEnd s) t1System
  -- 1. Fast Forward
  when (diff < minRenderLatency) $ do
    liftIO $ T.putStrLn "FAST-FORWARD"
    modify' $ \x -> x {
      renderStart = addUTCTime minRenderLatency t1System,
      renderPeriod = minRenderPeriod,
      renderEnd = addUTCTime (minRenderPeriod+minRenderLatency) t1System
    }

  -- 2. Normal Advance
  when (diff >= minRenderLatency && diff <= waitThreshold) $ do
    let ratio0 = (diff - minRenderLatency) / (maxRenderLatency - minRenderLatency)
    let ratio = min 1 ratio0
    let adaptivePeriod = ratio * (maxRenderPeriod - minRenderPeriod) + minRenderPeriod
    -- liftIO $ T.putStrLn $ "NORMAL " <> showt (realToFrac ratio :: Double) <> " " <> showt (realToFrac adaptivePeriod :: Double)
    modify' $ \x -> x {
      renderStart = renderEnd s,
      renderPeriod = adaptivePeriod,
      renderEnd = addUTCTime adaptivePeriod (renderEnd s)
    }

  -- 3. Wait
  let wait = (diff > waitThreshold && diff < rewindThreshold)
  when wait $ liftIO $ T.putStrLn $ "WAIT " <> showt (realToFrac diff :: Double)

  -- 4. Rewind
  let rewind = (diff >= rewindThreshold)
  when rewind $ do
    liftIO $ T.putStrLn $ "REWIND"
    modify' $ \x -> x {
      renderStart = addUTCTime minRenderLatency t1System,
      renderPeriod = minRenderPeriod,
      renderEnd = addUTCTime (minRenderPeriod+minRenderLatency) t1System
    }

  -- if there is no reason not to traverse/render zones, then do so
  -- using renderStart and renderEnd from the state as the window to render
  when (not crProblem && not wait && not rewind) $ do
    when (canvasElement c /= cachedCanvasElement s) $ do
      liftIO $ putStrLn "render: canvasElement new/changed"
      traverseWithKey (canvasChanged c) (zones $ ensemble $ ensembleC c)
      modify' $ \x -> x { cachedCanvasElement = canvasElement c }
    traverseWithKey (renderZone c) (zones $ ensemble $ ensembleC c)
    flushEvents c
    -- calculate how much time this render cycle took and update load measurements
    t2System <- liftIO $ getCurrentTime
    t2Audio <- liftAudioIO $ audioTime
    let mostRecentRenderTime = diffUTCTime t2System t1System
    let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
    let newAvgRenderLoad = ceiling (getAverage newRenderTime * 100 / realToFrac maxRenderPeriod)
    modify' $ \x -> x { renderTime = newRenderTime }
    modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
    traverseWithKey calculateZoneRenderTimes $ zoneRenderTimes s -- *** SHOULDN'T BE HERE
    traverseWithKey calculateZoneAnimationTimes $ zoneAnimationTimes s -- *** SHOULDN'T BE HERE
    return ()
  sleepIfNecessary


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

setZoneError :: Int -> Text -> Renderer
setZoneError z t = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = insert z t oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

clearZoneError :: Int -> Renderer
clearZoneError z = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = delete z oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

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
  tNow <- getRenderTime c
  traverseWithKey (renderZoneAnimation tNow c) (zones $ ensemble $ ensembleC c)
  return ()

renderZoneAnimation :: UTCTime -> Context -> Int -> Definition -> Renderer
renderZoneAnimation tNow c z (TextProgram x) = do
  s <- get
  t1 <- getRenderTime c
  renderZoneAnimationTextProgram tNow c z $ forRendering x
  t2 <- getRenderTime c
  let prevZoneAnimationTimes = findWithDefault (newAverage 20) z $ zoneAnimationTimes s
  let newZoneAnimationTimes = updateAverage prevZoneAnimationTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneAnimationTimes = insert z newZoneAnimationTimes (zoneAnimationTimes s) }
  return ()
renderZoneAnimation _ _ _ _ = return ()

renderZoneAnimationTextProgram :: UTCTime -> Context -> Int -> (TextNotation,Text) -> Renderer
renderZoneAnimationTextProgram tNow c z (Punctual,x) = do
  webGLs <- gets punctualWebGLs
  let webGL = findWithDefault Punctual.emptyPunctualWebGL z webGLs
  let tNow' = utcTimeToAudioSeconds (clockDiff c) tNow
  liftIO $ Punctual.drawFrame tNow' webGL
renderZoneAnimationTextProgram _ _ _ _ = return ()

renderZoneChanged :: Context -> Int -> Definition -> Renderer
renderZoneChanged c z (Structure x) = do
  let newParamPattern = toParamPattern x
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged c z (TextProgram x) = do
  -- liftIO $ putStrLn $ "zone " ++ show z ++ " TextProgram changed, type = " ++ show x
  renderTextProgramChanged c z $ forRendering x
renderZoneChanged c z (Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  s <- get
  modify' $ \x -> x { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged _ _ _ = return ()

renderZoneAlways :: Context -> Int -> Definition -> Renderer
renderZoneAlways c z (Structure _) = renderControlPattern c z
renderZoneAlways c z (TextProgram x) = renderTextProgramAlways c z
renderZoneAlways c z (Sequence _) = renderControlPattern c z
renderZoneAlways _ _ _ = return ()

renderTextProgramChanged :: Context -> Int -> (TextNotation,Text) -> Renderer
renderTextProgramChanged c z prog = do
  let x = applyTextReplacement prog
  renderBaseProgramChanged c z x
  when (isRight x) $ do
    let (n,t) = fromRight (Punctual,"") x
    oldBaseNotations <- gets baseNotations
    modify' $ \y -> y { baseNotations = insert z n oldBaseNotations }

renderBaseProgramChanged :: Context -> Int -> Either ParseError (TextNotation,Text) -> Renderer

renderBaseProgramChanged c z (Left e) = setZoneError z (T.pack $ show e)

renderBaseProgramChanged c z (Right (TidalTextNotation x,y)) = do
  s <- get
  let parseResult = tidalParser x y -- :: Either ParseError ControlPattern
  let newParamPatterns = either (const $ paramPatterns s) (\p -> insert z p (paramPatterns s)) parseResult
  liftIO $ either (putStrLn) (const $ return ()) parseResult -- print new errors to console
  let newErrors = either (\e -> insert z (T.pack e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { paramPatterns = newParamPatterns, info = (info s) { errors = newErrors} }

renderBaseProgramChanged c z (Right (Punctual,x)) = parsePunctualNotation c z Punctual.runPunctualParser x

-- note: but really the line below is probably obsolete because of new text replacement approach?
renderBaseProgramChanged c z (Right (Experiment,x)) = parsePunctualNotation c z Punctual.runPunctualParser x

renderBaseProgramChanged c z (Right (CineCer0,x)) = do
  s <- get
  let parseResult :: Either String CineCer0.CineCer0Spec = CineCer0.cineCer0 $ T.unpack x -- Either String CineCer0Spec
  let maybeTheDiv = videoDivElement c
  when (isJust maybeTheDiv && isRight parseResult) $ do
    let spec :: CineCer0.CineCer0Spec = fromRight (IntMap.empty) parseResult
    let theDiv = fromJust maybeTheDiv
    let prevState = IntMap.findWithDefault (CineCer0.emptyCineCer0State theDiv) z $ cineCer0States s
    liftIO $ putStrLn $ show parseResult
    let t = tempo $ ensemble $ ensembleC c
    let now = renderStart s
    newState <- liftIO $ CineCer0.updateCineCer0State t now spec prevState
    modify' $ \x -> x { cineCer0States = insert z newState (cineCer0States s) }
  when (isLeft parseResult) $ do
    let errs = either (\e -> insert z (T.pack $ show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
    modify' $ \x -> x { info = (info s) { errors = errs }}

renderBaseProgramChanged c z (Right (TimeNot,x)) = do
  s <- get
  let parseResult = TimeNot.timeNot (renderStart s) x -- :: Either Text [(UTCTime, Map Text Datum)]
  when (isRight parseResult) $ do
    let evs = fromRight [] parseResult -- :: [(UTCTime, Map Text Datum)]
    let evs' = fmap (second (mapTextDatumToControlMap)) evs -- :: [(UTCTime,Tidal.ControlMap)]
    modify' $ \x -> x { dirtEvents = dirtEvents x ++ evs' }
    -- liftIO $ T.putStrLn $ T.pack $ show evs
  when (isLeft parseResult) $ do
    let errs = either (\e -> insert z e (errors (info s))) (const $ delete z (errors (info s))) parseResult
    modify' $ \x -> x { info = (info s) { errors = errs }}

renderBaseProgramChanged c z _ = setZoneError z "renderBaseProgramChanged: no match for base language"


parsePunctualNotation :: Context -> Int -> (Text -> Either ParseError [Punctual.Expression]) -> Text -> Renderer
parsePunctualNotation c z p t = do
  s <- get
  let parseResult = p t
  when (isRight parseResult) $ do
    let exprs = fromRight [] parseResult -- :: [Expression]
    let evalTime = utcTimeToAudioSeconds (clockDiff c) $ renderStart s -- :: AudioTime/Double
    let eval = (exprs,evalTime) -- :: Punctual.Evaluation
    punctualProgramChanged c z eval
  let newErrors = either (\e -> insert z (T.pack $ show e) (errors (info s))) (const $ delete z (errors (info s))) parseResult
  modify' $ \x -> x { info = (info s) { errors = newErrors }}

punctualProgramChanged :: Context -> Int -> Punctual.Evaluation -> Renderer
punctualProgramChanged c z e = do
  s <- get
  -- A. update PunctualW (audio state) in response to new, syntactically correct program
  let (mainBusIn,_,_,_) = mainBus c
  ac <- liftAudioIO $ audioContext
  t <- liftAudioIO $ audioTime
  let prevPunctualW = findWithDefault (Punctual.emptyPunctualW ac mainBusIn 2 t) z (punctuals s)
  let tempo' = tempo $ ensemble $ ensembleC c
  let beat0 = utcTimeToAudioSeconds (clockDiff c) $ beatZero tempo'
  let cps' = cps tempo'
  newPunctualW <- liftAudioIO $ Punctual.updatePunctualW prevPunctualW (beat0,realToFrac cps') e
  modify' $ \x -> x { punctuals = insert z newPunctualW (punctuals s)}
  -- B. update Punctual WebGL state in response to new, syntactically correct program
  webGLs <- gets punctualWebGLs
  let prevWebGL = IntMap.lookup z webGLs
  prevWebGL' <- if isJust prevWebGL then return (fromJust prevWebGL) else
    liftIO $ Punctual.updateRenderingContext Punctual.emptyPunctualWebGL (canvasElement c)
  newWebGL <- liftIO $ Punctual.evaluatePunctualWebGL prevWebGL' (beat0,realToFrac cps') e
  modify' $ \x -> x { punctualWebGLs = insert z newWebGL webGLs }


renderTextProgramAlways :: Context -> Int -> Renderer
renderTextProgramAlways c z = do
  s <- get
  let baseNotation = IntMap.lookup z $ baseNotations s
  renderBaseProgramAlways c z $ baseNotation

renderBaseProgramAlways :: Context -> Int -> Maybe TextNotation -> Renderer
renderBaseProgramAlways c z (Just (TidalTextNotation _)) = renderControlPattern c z
renderBaseProgramAlways _ _ _ = return ()

renderControlPattern :: Context -> Int -> Renderer
renderControlPattern c z = when (webDirtOn c || superDirtOn c) $ do
  s <- get
  let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
  let lt = renderStart s
  let rp = renderPeriod s
  let tempo' = tempo $ ensemble $ ensembleC c
  let events = maybe [] id $ fmap (renderTidalPattern lt rp tempo') controlPattern
  modify' $ \x -> x { dirtEvents = (dirtEvents s) ++ events }

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

sleepIfNecessary :: Renderer
sleepIfNecessary = do
  s <- get
  let targetTime = addUTCTime (maxRenderLatency * (-1) - earlyWakeUp) (renderEnd s)
  tNow <- liftIO $ getCurrentTime
  let diff = diffUTCTime targetTime tNow
  when (diff > 0) $ do
    -- liftIO $ T.putStrLn $ "sleeping " <> showt (realToFrac diff :: Double)
    liftIO $ threadDelay $ floor $ realToFrac $ diff * 1000000
  -- when (diff <= 0) $ liftIO $ T.putStrLn "not sleeping"

forkRenderThreads :: MVar Context -> MVar RenderInfo -> IO ()
forkRenderThreads ctxM riM = do
  ctx <- readMVar ctxM
  t0Audio <- liftAudioIO $ audioTime
  let t0System = audioSecondsToUTC (clockDiff ctx) t0Audio
  irs <- initialRenderState t0System t0Audio
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
