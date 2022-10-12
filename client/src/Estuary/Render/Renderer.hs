{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Estuary.Render.Renderer where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Control.Monad.Reader
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
import GHCJS.DOM.Types (HTMLCanvasElement,HTMLDivElement)
import Data.Witherable
import Data.Char
import Data.IORef

import Sound.MusicW.AudioContext
import qualified Sound.Seis8s.Parser as Seis8s
import Estuary.Languages.Punctual
import Estuary.Languages.CineCer0
import Estuary.Languages.LocoMotion
import Estuary.Languages.MiniTidal
import Estuary.Languages.TimeNot
import Sound.Punctual.GL (GLContext)

import qualified Estuary.Languages.Hydra.Types as Hydra
import qualified Estuary.Languages.Hydra.Parser as Hydra
import qualified Estuary.Languages.Hydra.Render as Hydra

import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.Tidal.Types
import Estuary.Types.ParamPatternable
import Estuary.Types.Live
import Estuary.Languages.TidalParsers
import qualified Estuary.Languages.JSoLang as JSoLang
import qualified Estuary.Render.WebDirt as WebDirt
import qualified Estuary.Render.SuperDirt as SuperDirt
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState hiding (LocoMotion,locoMotion)
import Estuary.Types.Tempo
import Estuary.Types.MovingAverage
import Estuary.Render.DynamicsMode
import Estuary.Render.MainBus
import Estuary.Render.R
import Estuary.Render.TextNotationRenderer
import Estuary.Render.RenderOp
import qualified Estuary.Client.Settings as Settings
import Estuary.Render.WebSerial as WebSerial


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


-- flush note events to WebDirt, SuperDirt, and/or WebSerial
flushEvents :: R ()
flushEvents = do
  rEnv <- ask
  s <- get
  unsafe <- unsafeModeOn

  -- maybe send events to WebDirt
  wdOn <- webDirtOn
  when wdOn $ liftIO $ do
    let cDiff = (wakeTimeSystem s,wakeTimeAudio s)
    noteEvents' <- witherM (WebDirt.noteEventToWebDirtJSVal unsafe (resources rEnv) cDiff) $ noteEvents s
    webDirtEvents' <- mapM (WebDirt.accessBufferForWebDirtEvent (resources rEnv)) $ webDirtEvents s
    mapM_ (WebDirt.playSample (webDirt rEnv)) $ noteEvents' ++ webDirtEvents'

  -- maybe send events to SuperDirt via the SuperDirt socket
  sdOn <- superDirtOn
  when sdOn $ liftIO $ do
    noteEvents' <- mapM SuperDirt.noteEventToSuperDirtJSVal $ noteEvents s
    mapM_ (SuperDirt.playSample (superDirt rEnv)) $ noteEvents'

  -- maybe send events to WebSerial
  webSerialOn <- liftIO $ WebSerial.isActive (webSerial rEnv)
  when webSerialOn $ liftIO $ mapM_ (WebSerial.send (webSerial rEnv)) $ noteEvents s

  -- clear the queue of all note events
  modify' $ \x -> x { noteEvents = [], webDirtEvents = [] } -- note: webDirtEvents is temporary/deprecated
  return ()

renderTidalPattern :: Tidal.ValueMap -> UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ValueMap)]
renderTidalPattern vMap start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (time t)) * freq t + count t -- start time in cycles since beginning of tempo
    end = realToFrac range * freq t + start' -- end time in cycles since beginning of tempo
    a = Tidal.Arc (toRational start') (toRational end)
    events = Tidal.query p $ Tidal.State a vMap
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - count t)/freq t)) (time t)
        w1 = Tidal.start $ fromJust $ Tidal.whole e

sequenceToControlPattern :: (Text,[Bool]) -> Tidal.ControlPattern
sequenceToControlPattern (sampleName,pat) = Tidal.s $ parseBP' $ intercalate " " $ fmap f pat
  where f False = "~"
        f True = T.unpack sampleName

render :: R ()
render = do
  -- rEnv <- ask
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
    updateTidalValueMap
    renderRenderOps
    renderZones
    flushEvents

    -- calculate how much time this render cycle took and update load measurements
    t2System <- liftIO $ getCurrentTime
    let mostRecentRenderTime = diffUTCTime t2System t1System
    let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
    let newAvgRenderLoad = ceiling (getAverage newRenderTime * 100 / realToFrac maxRenderPeriod)
    modify' $ \x -> x { renderTime = newRenderTime }
    modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
    return ()
  updateWebDirtVoices


renderRenderOps :: R ()
renderRenderOps = takeRenderOps >>= foldM renderRenderOp ()

renderRenderOp :: () -> RenderOp -> R ()

renderRenderOp _ (WriteTempo t) = do
  modify' $ \s -> s { tempoCache = t }

renderRenderOp _ (WriteZone z x) = do
  defs <- gets cachedDefs
  let x' = definitionForRendering x
  maybeClearChangedZone z (IntMap.lookup z defs) x'
  renderZoneChanged z x'
  modify' $ \s -> s { cachedDefs = insert z x' (cachedDefs s) }

renderRenderOp _ ResetZones = do
  gets cachedDefs >>= traverseWithKey clearZone
  modify' $ \s -> s { cachedDefs = empty }

renderZones :: R ()
renderZones = gets cachedDefs >>= traverseWithKey renderZoneAlways >> return ()


maybeClearChangedZone :: Int -> Maybe Definition -> Definition -> R ()
maybeClearChangedZone _ Nothing y = return ()
maybeClearChangedZone z (Just x) y
  | defsSameRender x y = return ()
  | otherwise = clearZone z x

defsSameRender :: Definition -> Definition -> Bool
defsSameRender (TextProgram x) (TextProgram y) = textProgramsSameRender (forRendering x) (forRendering y)
defsSameRender (Sequence _) (Sequence _) = True
defsSameRender (TidalStructure _) (TidalStructure _) = True
defsSameRender _ _ = False

textProgramsSameRender :: (TextNotation,Text,UTCTime) -> (TextNotation,Text,UTCTime) -> Bool
textProgramsSameRender (x,_,_) (y,_,_) = x==y

clearZone :: Int -> Definition -> R ()
clearZone z (TidalStructure _) = clearParamPattern z
clearZone z (TextProgram x) = clearTextProgram z $ forRendering x
clearZone z (Sequence _) = clearParamPattern z
clearZone _ _ = return ()

clearTextProgram :: Int -> (TextNotation,Text,UTCTime) -> R ()
clearTextProgram z (TidalTextNotation MiniTidal,_,_) = (clearZone' miniTidal) z
clearTextProgram z (Punctual,_,_) = (clearZone' punctual) z
clearTextProgram z (CineCer0,_,_) = (clearZone' cineCer0) z
clearTextProgram z (Hydra,_,_) = modify' $ \x -> x { hydras = IntMap.delete z $ hydras x }
clearTextProgram z (LocoMotion,_,_) = (clearZone' locoMotion) z
clearTextProgram _ _ = return ()


renderAnimation :: R ()
renderAnimation = do
  t1 <- liftIO $ getCurrentTime
  wta <- gets wakeTimeAnimation
  fpsl <- fpsLimit
  let okToRender = case fpsl of Nothing -> True; Just x -> diffUTCTime t1 wta > x
  rEnv <- ask
  liftIO $ WebSerial.flush (webSerial rEnv)
  when okToRender $ do
    preAnimationFrame punctual
    preAnimationFrame locoMotion
    ns <- baseNotations <$> get
    traverseWithKey (renderZoneAnimation t1) ns
    postAnimationFrame punctual
    postAnimationFrame locoMotion
    t2 <- liftIO $ getCurrentTime
    s <- get
    let newAnimationDelta = updateAverage (animationDelta s) (realToFrac $ diffUTCTime t1 wta)
    let newAnimationTime = updateAverage (animationTime s) (realToFrac $ diffUTCTime t2 t1)
    let newAnimationFPS = round $ 1 / getAverage newAnimationDelta
    let newAnimationLoad = round $ getAverage newAnimationTime * 1000
    modify' $ \x -> x {
      wakeTimeAnimation = t1,
      animationDelta = newAnimationDelta,
      animationTime = newAnimationTime,
      info = (info x) {
        animationFPS = newAnimationFPS,
        animationLoad = newAnimationLoad
        }
      }

renderZoneAnimation :: UTCTime -> Int -> TextNotation -> R ()
renderZoneAnimation tNow z n = renderZoneAnimationTextProgram tNow z n

renderZoneAnimationTextProgram :: UTCTime -> Int -> TextNotation -> R ()
renderZoneAnimationTextProgram tNow z Punctual = (zoneAnimationFrame punctual) tNow z
renderZoneAnimationTextProgram tNow z CineCer0 = (zoneAnimationFrame cineCer0) tNow z
renderZoneAnimationTextProgram tNow z Hydra = renderHydra tNow z
renderZoneAnimationTextProgram tNow z LocoMotion = (zoneAnimationFrame locoMotion) tNow z
renderZoneAnimationTextProgram  _ _ _ = return ()

renderHydra :: UTCTime -> Int -> R ()
renderHydra tNow z = do
  s <- get
  let wta = wakeTimeAnimation s
  let elapsed = realToFrac $ diffUTCTime tNow wta * 1000
  let x = IntMap.lookup z $ hydras s
  case x of
    Just hydra -> liftIO $ Hydra.tick hydra elapsed
    Nothing -> return ()

renderZoneChanged :: Int -> Definition -> R ()
renderZoneChanged z (TidalStructure x) = do
  let newParamPattern = toParamPattern x
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged z (TextProgram x) = renderTextProgramChanged z $ forRendering x
renderZoneChanged z (Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
renderZoneChanged _ _ = return ()

renderZoneAlways :: Int -> Definition -> R ()
renderZoneAlways z (TidalStructure _) = renderControlPattern z
renderZoneAlways z (TextProgram x) = renderTextProgramAlways z
renderZoneAlways z (Sequence _) = renderControlPattern z
renderZoneAlways _ _ = return ()


renderTextProgramChanged :: Int -> TextProgram -> R ()

renderTextProgramChanged z (UnspecifiedNotation,x,eTime) = do
  ns <- (Map.keys . jsoLangs) <$> get
  case determineTextNotation x ns of
    Left err -> do
      setZoneError z (T.pack $ show err)
      setBaseNotation z UnspecifiedNotation
    Right (x',n) -> do
      case n of
        UnspecifiedNotation -> do
          case T.filter (\c -> not (isControl c) && not (isSpace c)) x' of
            "" -> do -- notation is unspecified but
              clearZoneError z
              setBaseNotation z UnspecifiedNotation
            _ -> do
              setZoneError z "no base notation specified"
              setBaseNotation z UnspecifiedNotation
        _-> renderTextProgramChanged z (n,x',eTime)

renderTextProgramChanged z (TidalTextNotation _,x,eTime) = (parseZone miniTidal) z x eTime
renderTextProgramChanged z (Punctual,x,eTime) = (parseZone punctual) z x eTime
renderTextProgramChanged z (CineCer0,x,eTime) = (parseZone cineCer0) z x eTime
renderTextProgramChanged z (Hydra,x,_) = parseHydra z x
renderTextProgramChanged z (LocoMotion,x,eTime) = (parseZone locoMotion) z x eTime
renderTextProgramChanged z (TimeNot,x,eTime) = (parseZone timeNot) z x eTime

renderTextProgramChanged z (Seis8s,x,eTime) = do
  let parseResult = Seis8s.parseLang $ T.unpack x
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseNotation z Seis8s
      setEvaluationTime z eTime
      modify' $ \xx -> xx { seis8ses = insert z p $ seis8ses xx }
    Left e -> setZoneError z (T.pack $ show e)

renderTextProgramChanged z (JSoLang x,y,eTime) = do
  parseResult <- liftIO $ JSoLang.define y
  case parseResult of
    Right j -> do
      clearZoneError z
      setBaseNotation z (JSoLang x)
      setEvaluationTime z eTime
      modify' $ \xx -> xx { jsoLangs = Map.insert x j $ jsoLangs xx }
      liftIO $ T.putStrLn $ "defined JSoLang " <> x
    Left e -> setZoneError z (T.pack $ show e)

renderTextProgramChanged z (EphemeralNotation x,y,eTime) = do
  maybeJSoLang <- (Map.lookup x . jsoLangs) <$> get
  case maybeJSoLang of
    Just j -> do
      parseResult <- liftIO $ JSoLang.parse j y
      case parseResult of
        Right x' -> do
          liftIO $ T.putStrLn $ "result of parsing " <> x <> ":"
          liftIO $ T.putStrLn x'
          renderTextProgramChanged z (UnspecifiedNotation,x',eTime)
        Left e -> setZoneError z e
    Nothing -> setZoneError z $ "no ephemeral notation called " <> y <> " exists"

renderTextProgramChanged z _ = setZoneError z "renderTextProgramChanged: no match for base notation"

parseHydra :: Int -> Text -> R ()
parseHydra z t = do
 s <- get
 parseResult <- liftIO $ try $ return $! Hydra.parseHydra t
 case parseResult of
   Right (Right stmts) -> do
     clearZoneError z
     setBaseNotation z Hydra
     -- setEvaluationTime z eTime ???
     let x = IntMap.lookup z $ hydras s
     hydra <- case x of
       Just h -> return h
       Nothing -> do
         h <- liftIO $ Hydra.newHydra $ hydraCanvas s
         modify' $ \x -> x { hydras = IntMap.insert z h (hydras x)}
         return h
     -- liftIO $ Hydra.setResolution hydra 1280 720
     liftIO $ Hydra.evaluate hydra stmts
   Right (Left parseErr) -> setZoneError z (T.pack $ show parseErr)
   Left exception -> setZoneError z (T.pack $ show (exception :: SomeException))


renderTextProgramAlways :: Int -> R ()
renderTextProgramAlways z = do
  s <- get
  renderBaseProgramAlways z $ IntMap.lookup z $ baseNotations s


renderBaseProgramAlways :: Int -> Maybe TextNotation -> R ()
renderBaseProgramAlways z (Just (TidalTextNotation _)) = (scheduleNoteEvents miniTidal) z >>= pushNoteEvents
renderBaseProgramAlways z (Just TimeNot) = (scheduleWebDirtEvents timeNot) z >>= pushWebDirtEvents
renderBaseProgramAlways z (Just Seis8s) = do
  s <- get
  let p = IntMap.lookup z $ seis8ses s
  case p of
    Just p' -> do
      let theTempo = tempoCache s
      let wStart = renderStart s
      let wEnd = renderEnd s
      pushNoteEvents $ Seis8s.render p' theTempo wStart wEnd
    Nothing -> return ()
renderBaseProgramAlways _ _ = return ()


calculateZoneRenderTimes :: Int -> MovingAverage -> R ()
calculateZoneRenderTimes z zrt = do
  s <- get
  let newAvgMap = insert z (getAverage zrt) (avgZoneRenderTime $ info s)
  modify' $ \x -> x { info = (info x) { avgZoneRenderTime = newAvgMap }}

calculateZoneAnimationTimes :: Int -> MovingAverage -> R ()
calculateZoneAnimationTimes z zat = do
  s <- get
  let newAvgMap = insert z (getAverage zat) (avgZoneAnimationTime $ info s)
  modify' $ \x -> x { info = (info x) { avgZoneAnimationTime = newAvgMap }}

sleepIfNecessary :: R ()
sleepIfNecessary = do
  s <- get
  let targetTime = addUTCTime (maxRenderLatency * (-1) - earlyWakeUp) (renderEnd s)
  tNow <- liftIO $ getCurrentTime
  let diff = diffUTCTime targetTime tNow
  when (diff > 0) $ liftIO $ threadDelay $ floor $ realToFrac $ diff * 1000000

forkRenderThreads :: RenderEnvironment -> Settings.Settings -> HTMLDivElement -> HTMLCanvasElement -> GLContext -> HTMLCanvasElement -> HTMLCanvasElement -> IO ()
forkRenderThreads rEnv s vidDiv cvsElement glCtx hCanvas lCanvas = do
  t0Audio <- liftAudioIO $ audioTime
  t0System <- getCurrentTime
  pIn <- getPunctualInput $ mainBus rEnv
  pOut <- getMainBusInput $ mainBus rEnv
  putStrLn "about to do initialRenderState"
  irs <- initialRenderState pIn pOut cvsElement glCtx hCanvas lCanvas t0System t0Audio
  putStrLn "returned from initialRenderState"
  let irs' = irs { videoDivCache = Just vidDiv }
  rsM <- newMVar irs'
  putStrLn "about to fork mainRenderThread..."
  void $ forkIO $ mainRenderThread rEnv rsM
  putStrLn "returned from forking mainRenderThread"
  void $ forkIO $ animationThread rEnv rsM
  putStrLn "returned from forking animationThread"

mainRenderThread :: RenderEnvironment -> MVar RenderState -> IO ()
mainRenderThread rEnv rsM = do
  rs <- takeMVar rsM
  rs' <- runR render rEnv rs
  putMVar rsM rs'
  swapMVar (renderInfo rEnv) (info rs') -- copy RenderInfo from state into MVar for instant reading elsewhere
  _ <- runR sleepIfNecessary rEnv rs'
  mainRenderThread rEnv rsM

animationThread :: RenderEnvironment -> MVar RenderState -> IO ()
animationThread rEnv rsM = void $ inAnimationFrame ContinueAsync $ \_ -> do
  rs <- readMVar rsM
  animOn <- Settings.canvasOn <$> (readIORef $ _settings rEnv)
  when animOn $ do
    rs' <- takeMVar rsM
    rs'' <- runR renderAnimation rEnv rs'
    putMVar rsM rs''
  animationThread rEnv rsM
