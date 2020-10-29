{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Estuary.Render.Renderer where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (evaluate,catch,SomeException,try)
import Control.DeepSeq
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
import Text.Parsec (ParseError)
import qualified Data.ByteString as B
import GHCJS.DOM.Types (HTMLCanvasElement)
import Data.Witherable

import Sound.MusicW.AudioContext
import qualified Sound.Punctual.Program as Punctual
import qualified Sound.Punctual.PunctualW as Punctual
import qualified Sound.Punctual.GL as Punctual
import qualified Sound.Punctual.WebGL as Punctual
import qualified Sound.Punctual.AsyncProgram as Punctual
import qualified Sound.Punctual.Parser as Punctual
import qualified Sound.Punctual.Resolution as Punctual
import qualified Sound.TimeNot.AST as TimeNot
import qualified Sound.TimeNot.Parsers as TimeNot
import qualified Sound.TimeNot.Render as TimeNot

import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Tidal.Types
import Estuary.Tidal.ParamPatternable
import Estuary.Types.Live
import qualified Estuary.Render.WebDirt as WebDirt
import qualified Estuary.Render.SuperDirt as SuperDirt
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Types.RenderEnvironment
import Estuary.Types.Tempo
import Estuary.Types.MovingAverage
import Estuary.Render.DynamicsMode
import Estuary.Render.R

import qualified Estuary.Languages.Hydra.Types as Hydra
import qualified Estuary.Languages.Hydra.Parser as Hydra
import qualified Estuary.Languages.Hydra.Render as Hydra
import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0
import Estuary.Languages.TidalParsers
import Estuary.Languages.TextReplacement

clockRatioThreshold :: Double
clockRatioThreshold = 0.8

maxRenderLatency :: NominalDiffTime
maxRenderLatency = 0.360

maxRenderPeriod :: NominalDiffTime
maxRenderPeriod = 0.360

minRenderLatency :: NominalDiffTime
minRenderLatency = 0.070

minRenderPeriod :: NominalDiffTime
minRenderPeriod = 0.120

-- should be somewhat larger than maxRenderLatency
waitThreshold :: NominalDiffTime
waitThreshold = 0.380

rewindThreshold :: NominalDiffTime
rewindThreshold = 1.0

earlyWakeUp :: NominalDiffTime
earlyWakeUp = 0.002

processEnsembleEvents :: R ()
processEnsembleEvents ir = do
  eevsM <- ensembleEventsM <$> ask
  eevs <- takeMVar eevsM
  putMVar eevsM []
  mapM_ processEnsembleEvent eevs

processEnsembleEvent :: EnsembleEvent -> R ()
processEnsembleEvent (TempoEvent t) = modify' $ \s -> s { tempo = t }
processEnsembleEvent (ZoneEvent z d) = do
  let d' = definitionForRendering d
  prevDef <- (IntMap.lookup z . cachedDefs) <$> get
  case prevDef of
    Nothing -> do
      renderZoneChanged z d'
      modify' $ \x -> x { cachedDefs = insert z d' (cachedDefs s) }
    Just dPrev -> do
      case definitionChanged dPrev d' of
        True -> do
          when (definitionNotationChanged dPrev d') $ clearZone z dPrev
          renderZoneChanged z d'
          modify' $ \x -> x { cachedDefs = insert z d' (cachedDefs s) }
        False -> return ()
processEnsembleEvent ClearZones = clearZones
processEnsembleEvent LeaveEvent = clearZones
processEnsembleEvent _ = return ()


parseZone :: Int -> Definition -> R ()
parseZone z (TidalStructure x) = modify' $ \y -> y { paramPatterns = insert z (toParamPattern x) (paramPatterns y) }
parseZone z (TextProgram x) = parseTextProgram z $ forRendering x
parseZone z (Sequence xs) = do
  let y = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  modify' $ \x -> x { paramPatterns = insert z y (paramPatterns x) }
parseZone _ _ = return ()

parseTextProgram :: Int -> TextProgram -> R ()
parseTextProgram z prog = do
  let x = applyTextReplacement prog
  parseBaseProgram z x
  case x of
    (Right x') -> do
      let (n,_,eTime) = x'
      s <- get
      let oldBaseNotations = baseNotations s
      let oldEvaluationTimes = evaluationTimes s
      modify' $ \y -> y { baseNotations = insert z n oldBaseNotations, evaluationTimes = insert z eTime oldEvaluationTimes }
    (Left _) -> return ()

parseBaseProgram :: Int -> Either ParseError TextProgram -> R ()
parseBaseProgram z (Left e) = setZoneError z (T.pack $ show e)
parseBaseProgram z (Right (TidalTextNotation p,txt,_)) = parseTidalTextNotation z notation txt
parseBaseProgram z (Right (Punctual,txt,eTime)) = parsePunctual z txt eTime
parseBaseProgram z (Right (CineCer0,txt,eTime)) = parseCineCer0 z txt eTime
parseBaseProgram z (Right (TimeNot,txt,eTime)) = parseTimeNot z txt eTime
parseBaseProgram z (Right (Seis8s,txt,eTime)) = parseSeis8s z txt eTime
parseBaseProgram z (Right (Hydra,txt,_)) = parseHydra z txt
parseBaseProgram z _ = setZoneError z "*parseBaseProgram: no match for base language*"


clearZones :: R ()
clearZones = do
  s <- get
  IntMap.traverseWithKey clearZone $ cachedDefs s
  modify' $ \x -> x { cachedDefs = IntMap.empty }

clearZone :: Int -> Definition -> R ()
clearZone z (TextProgram x) = do
  clearTextProgram z $ forRendering x
  modify' $ \s -> s { cachedDefs = IntMap.delete z (cachedDefs s) }
clearZone z (Sequence _) = do
  modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }
  modify' $ \s -> s { cachedDefs = IntMap.delete z (cachedDefs s) }
clearZone z (TidalStructure _) = do
  modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }
  modify' $ \s -> s { cachedDefs = IntMap.delete z (cachedDefs s) }
clearZone z _ = modify' $ \s -> s { cachedDefs = IntMap.delete z (cachedDefs s) }

clearTextProgram :: Int -> TextProgram -> R ()
clearTextProgram z (TidalTextNotation _,_,_) = modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }
clearTextProgram z (Punctual,_,_) = *** ???
clearTextProgram z (CineCer0,_,_) = *** ???
clearTextProgram z (TimeNot,_,_) = modify' $ \s -> s { timeNots = IntMap.delete z (timeNots s) }
clearTextProgram z (Seis8s,_,_) = modify' $ \s -> s { seis8ses = IntMap.delete z (seis8ses s) }
clearTextProgram z (Hydra,_,_) = *** ???

-- *** ^--- TODO: above here, need to implement clear zones of visual languages properly




-- flush events for SuperDirt and WebDirt
flushEvents :: R ()
flushEvents = do
  stgs <- askSettings
  am <- askAudioMap
  st <- get
  wd <- webDirt <$> ask
  sd <- superDirt <$> ask
  when (webDirtOn stgs) $ liftIO $ do
    let cDiff = (wakeTimeSystem st,wakeTimeAudio st)
    noteEvents' <- witherM (WebDirt.noteEventToWebDirtJSVal am cDiff) $ noteEvents st
    tidalEvents' <- witherM (WebDirt.tidalEventToWebDirtJSVal am cDiff) $ tidalEvents st
    mapM_ (WebDirt.playSample wd) $ noteEvents' ++ tidalEvents'
  when (superDirtOn stgs) $ liftIO $ do
    noteEvents' <- mapM (SuperDirt.noteEventToSuperDirtJSVal am) $ noteEvents st
    tidalEvents' <- mapM (SuperDirt.tidalEventToSuperDirtJSVal am) $ tidalEvents st
    mapM_ (SuperDirt.playSample (superDirt irc)) $ noteEvents' ++ tidalEvents'
  modify' $ \x -> x { noteEvents = [], tidalEvents = [] }
  return ()


render :: R ()
render = do
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
  when crProblem $ liftIO $ T.putStrLn $ "audio clock slower, ratio=" <> showt cr

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
  when (not wait && not rewind) $ do
    processEnsembleEvents
    traverseWithKey renderZone (cachedDefs s)
    flushEvents
    updatePunctualResolutionAndBrightness
    -- calculate how much time this render cycle took and update load measurements
    t2System <- liftIO $ getCurrentTime
    let mostRecentRenderTime = diffUTCTime t2System t1System
    let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
    let newAvgRenderLoad = ceiling (getAverage newRenderTime * 100 / realToFrac maxRenderPeriod)
    modify' $ \x -> x { renderTime = newRenderTime }
    modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
    return ()


renderZone :: Int -> Definition -> R ()
renderZone z d = do
  t1 <- liftIO $ getCurrentTime
  case d of
    TidalStructure _ -> renderControlPattern z
    TextProgram x -> renderTextProgram z evalTime
    Sequence _ -> returnControlPattern z
    _ -> return ()
  t2 <- liftIO $ getCurrentTime
  let prevZoneRenderTimes = findWithDefault (newAverage 20) z $ zoneRenderTimes s
  let newZoneRenderTimes = updateAverage prevZoneRenderTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneRenderTimes = insert z newZoneRenderTimes (zoneRenderTimes s) }

renderTextProgram :: Int -> UTCTime -> R ()
renderTextProgram z eTime = do
  s <- get
  renderBaseProgram z eTime $ IntMap.lookup z $ baseNotations s

renderBaseProgram :: Int -> Maybe TextNotation -> Renderer
renderBaseProgram z (Just (TidalTextNotation _)) = renderControlPattern irc c z
renderBaseProgram z (Just TimeNot) = renderTimeNot z
renderBaseProgram z (Just Seis8s) = renderSeis8s z
renderBaseProgramAlways _ _ = return ()


animate :: R ()
animate = do
  t1 <- liftIO $ getCurrentTime
  defs <- gets cachedDefs
  traverseWithKey (animateZone t1) defs
  s <- get
  newWebGL <- liftIO $
    Punctual.displayPunctualWebGL (glContext s) (punctualWebGL s)
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  t2 <- liftIO $ getCurrentTime
  let newAnimationDelta = updateAverage (animationDelta s) (realToFrac $ diffUTCTime t1 (wakeTimeAnimation s))
  let newAnimationTime = updateAverage (animationTime s) (realToFrac $ diffUTCTime t2 t1)
  let newAnimationFPS = round $ 1 / getAverage newAnimationDelta
  let newAnimationLoad = round $ getAverage newAnimationTime * 1000
  modify' $ \x -> x {
    punctualWebGL = newWebGL,
    wakeTimeAnimation = t1,
    animationDelta = newAnimationDelta,
    animationTime = newAnimationTime,
    info = (info x) {
      animationFPS = newAnimationFPS,
      animationLoad = newAnimationLoad
      }
    }

animateZone :: UTCTime -> Int -> Definition -> R ()
animateZone tNow z (TextProgram x) = do
  t1 <- liftIO $ getCurrentTime
  animateTextProgram tNow z $ forRendering x
  t2 <- liftIO $ getCurrentTime
  s <- get
  let prevZoneAnimationTimes = findWithDefault (newAverage 20) z $ zoneAnimationTimes s
  let newZoneAnimationTimes = updateAverage prevZoneAnimationTimes (realToFrac $ diffUTCTime t2 t1)
  modify' $ \x -> x { zoneAnimationTimes = insert z newZoneAnimationTimes (zoneAnimationTimes s) }
  return ()
renderZoneAnimation  _ _ _ = return ()

animateTextProgram :: UTCTime -> Int -> TextProgram -> Renderer
animateTextProgram tNow z (Punctual,x,eTime) = animatePunctual tNow z
animateTextProgram tNow z (CineCer0,x,eTime) = animateCineCer0 tNow z
animateTextProgram tNow z (Hydra,x,eTime) = animateHydra tNow z
animateTextProgram  _ _ _ = return ()


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
  when (diff > 0) $ liftIO $ threadDelay $ floor $ realToFrac $ diff * 1000000

forkRenderThreads :: RenderEnvironment -> HTMLCanvasElement -> Punctual.GLContext -> HTMLCanvasElement -> MVar RenderInfo -> IO ()
forkRenderThreads re cvsElement glCtx hCanvas riM = do
  t0Audio <- liftAudioIO $ audioTime
  t0System <- getCurrentTime
  irs <- initialRenderState (mic irc) (out irc) cvsElement glCtx hCanvas t0System t0Audio
  rsM <- newMVar irs
  void $ forkIO $ mainRenderThread irc ctxM riM rsM
  void $ forkIO $ animationThread irc ctxM rsM

mainRenderThread :: RenderEnvironment -> MVar RenderState -> IO ()
mainRenderThread re rsM = do
  rs <- takeMVar rsM
  rs' <- execR re rs render
  let rs'' = rs' { videoDivCache = videoDivElement ctx } -- *** ??? should be moved to RenderEnvironment
  putMVar rsM rs''
  swapMVar riM (info rs'') -- copy RenderInfo from state into MVar for instant reading elsewhere
  _ <- execR re rs'' sleepIfNecessary
  mainRenderThread re rsM

animationThread :: RenderEnvironment -> MVar RenderState -> IO ()
animationThread re rsM = void $ inAnimationFrame ContinueAsync $ \_ -> do
  rs <- readMVar rsM
  when (animationOn rs) $ do
    rs' <- takeMVar rsM
    rs'' <- execStateT animate rs'
    putMVar rsM rs''
  animationThread irc ctxM rsM
