{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Estuary.Render.RenderEngine where

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
import Text.Parsec (ParseError)
import qualified Data.ByteString as B
import GHCJS.DOM.Types (HTMLCanvasElement,HTMLDivElement)
import Data.Witherable
import Data.Char
import Data.IORef

import Sound.MusicW.AudioContext
import qualified Sound.Seis8s.Parser as Seis8s
-- import Estuary.Languages.Punctual
import Estuary.Languages.CineCer0
import qualified Estuary.Languages.MiniTidal as MiniTidal
import Estuary.Languages.TimeNot
import qualified Estuary.Languages.ExoLang as ExoLang
-- import Sound.Punctual.GL (GLContext)

import qualified Estuary.Languages.Hydra.Types as Hydra
import qualified Estuary.Languages.Hydra.Parser as Hydra
import qualified Estuary.Languages.Hydra.Render as Hydra

import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Tidal.Types
import Estuary.Types.ParamPatternable
import Estuary.Types.Live
import qualified Estuary.Languages.JSoLang as JSoLang
import qualified Estuary.Render.WebDirt as WebDirt
import qualified Estuary.Render.SuperDirt as SuperDirt
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState hiding (LocoMotion)
import Estuary.Types.Tempo
import Estuary.Types.MovingAverage
import Estuary.Render.DynamicsMode
import Estuary.Render.MainBus
import Estuary.Render.R
import Estuary.Render.RenderEnvironment
import Estuary.Render.TextNotationRenderer
import Estuary.Render.RenderOp
import qualified Estuary.Client.Settings as Settings
import Estuary.Render.WebSerial as WebSerial



mainRenderThreadSleepTime :: Int
mainRenderThreadSleepTime = 200000 -- in microseconds

mainRenderPeriod :: NominalDiffTime
mainRenderPeriod = 0.2

fastForwardThreshold :: NominalDiffTime
fastForwardThreshold = 0.070

catchupThreshold :: NominalDiffTime
catchupThreshold = 0.300

microRenderThreshold :: NominalDiffTime
microRenderThreshold = 0.250

waitThreshold :: NominalDiffTime
waitThreshold = 0.500

rewindThreshold :: NominalDiffTime
rewindThreshold = 0.800

clockRatioThreshold :: Double
clockRatioThreshold = 0.8


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


sequenceToControlPattern :: (Text,[Bool]) -> Tidal.ControlPattern
sequenceToControlPattern (sampleName,pat) = Tidal.s $ parseBP' $ intercalate " " $ fmap f pat
  where f False = "~"
        f True = T.unpack sampleName


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

  let diff = diffUTCTime (renderEnd s) t1System

  -- 1. Fast Forward
  -- if the logical endtime of previous rendering is very soon in the future (below a threshold), or is in the past
  -- then skip ahead so that the logical starttime of new rendering is at least in the future by that threshold
  let fastForward = diff < fastForwardThreshold
  when fastForward $ do
    liftIO $ T.putStrLn "FAST-FORWARD"
    modify' $ \x -> x {
      renderStart = addUTCTime fastForwardThreshold t1System,
      renderPeriod = mainRenderPeriod,
      renderEnd = addUTCTime (mainRenderPeriod+fastForwardThreshold) t1System
    }

  -- 2. Catchup
  let catchup = diff >= fastForwardThreshold && diff < catchupThreshold
  when catchup $ do
    liftIO $ T.putStrLn $ "CATCHUP"
    modify' $ \x -> x {
      renderStart = renderEnd s,
      renderPeriod = mainRenderPeriod,
      renderEnd = addUTCTime mainRenderPeriod (renderEnd s)
    }

  -- 3. Wait
  -- let wait = diff >= waitThreshold && diff < rewindThreshold
 
  -- 4. Rewind
  let rewind = (diff >= rewindThreshold)
  when rewind $ do
    liftIO $ T.putStrLn $ "REWIND"
    modify' $ \x -> x {
      renderStart = addUTCTime fastForwardThreshold t1System,
      renderPeriod = mainRenderPeriod,
      renderEnd = addUTCTime (mainRenderPeriod+fastForwardThreshold) t1System
    }
  
  renderRenderOps
  when (fastForward || catchup || rewind) $ do
    updateTidalValueMap
    renderZones
    flushEvents

  -- calculate how much time this render cycle took and update load measurements
  -- TODO: these calculations don't completely make sense given the microRendering system
  t2System <- liftIO $ getCurrentTime
  let mostRecentRenderTime = diffUTCTime t2System t1System
  let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
  let newAvgRenderLoad = ceiling (getAverage newRenderTime * 100 / realToFrac mainRenderPeriod)
  modify' $ \x -> x { renderTime = newRenderTime }
  modify' $ \x -> x { info = (info x) { avgRenderLoad = newAvgRenderLoad }}
  updateWebDirtVoices


microRenderInAnimationFrame :: R ()
microRenderInAnimationFrame = do
  s <- get
  tNow <- liftIO $ getCurrentTime
  let diff = diffUTCTime (renderEnd s) tNow
  let microRender = diff >= microRenderThreshold && diff < waitThreshold
  when microRender $ do
    fpsl <- fpsLimit
    let mDelta = realToFrac $ getAverage $ animationDelta s
    let microRenderPeriod = calculateMicroRenderPeriod mDelta fpsl
    modify' $ \x -> x {
      renderStart = renderEnd s,
      renderEnd = addUTCTime microRenderPeriod (renderEnd s),
      renderPeriod = microRenderPeriod
    }
    updateTidalValueMap
    renderZones True
    flushEvents

calculateMicroRenderPeriod :: NominalDiffTime -> Maybe NominalDiffTime -> NominalDiffTime
calculateMicroRenderPeriod _ (Just fpsl) = fpsl
calculateMicroRenderPeriod measuredDelta Nothing = measuredDelta


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

renderZones :: Bool -> R ()
renderZones canDraw = gets cachedDefs >>= traverseWithKey (renderZoneAlways canDraw) >> return ()


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
clearTextProgram z ("MiniTidal",_,_) = gets miniTidal >>= liftIO . (flip clearZone) z
clearTextProgram z ("Punctual",_,_) = gets punctual >>= liftIO . (flip clearZone) z
clearTextProgram z ("CineCer0",_,_) = (clearZone' cineCer0) z
clearTextProgram z ("Hydra",_,_) = modify' $ \x -> x { hydras = IntMap.delete z $ hydras x }
clearTextProgram z ("LocoMotion",_,_) = gets locoMotion >>= (flip $ clearZone' $ exoLangToRenderer "LocoMotion") z
clearTextProgram z ("TranMit",_,_) = gets locoMotion >>= (flip $ clearZone' $ exoLangToRenderer "TransMit") z
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
    s <- get
    let ns = baseNotations s
    let anyPunctualZones = elem "Punctual" ns
    let anyLocoMotionZones = elem "LocoMotion" ns
    let anyTransMitZones = elem "TransMit" ns
    when anyPunctualZones $ preAnimationFrame punctual
    when anyLocoMotionZones $ liftIO $ ExoLang.preAnimate (locoMotion s)
    when anyTransMitZones $ liftIO $ ExoLang.preAnimate (transMit s)
    traverseWithKey (renderZoneAnimation t1) ns
    when anyPunctualZones $ postAnimationFrame punctual
    when anyLocoMotionZones $ liftIO $ ExoLang.postAnimate (locoMotion s)
    when anyTransMitZones $ liftIO $ ExoLang.postAnimate (transMit s)
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
  microRenderInAnimationFrame


defineZone :: Int -> Definition -> R ()
defineZone z d@(TidalStructure x) = do
  let newParamPattern = toParamPattern x
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
  setBaseDefinition z d 
defineZone z d@(Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
  setBaseDefinition z d
defineZone z (TextProgram x) = defineZoneTextProgram z $ forRendering x
defineZone _ _ = return ()

defineZoneTextProgram :: Int -> TextProgram -> R ()
defineZoneTextProgram z d@("MiniTidal",_,_) = gets miniTidal >>= defineZoneGeneric z d
defineZoneTextProgram z d@("Punctual",_,_) = gets punctual >>= defineZoneGeneric z d
defineZoneTextProgram z ("CineCer0",x,eTime) = (parseZone cineCer0) z x eTime
defineZoneTextProgram z ("Hydra",x,_) = defineZoneHydra z x
defineZoneTextProgram z ("LocoMotion",x,eTime) = do
  s <- get
  parseZone (exoLangToRenderer "LocoMotion" $ locoMotion s) z x eTime
defineZoneTextProgram z ("TransMit",x,eTime) = do
  s <- get
  parseZone (exoLangToRenderer "TransMit" $ transMit s) z x eTime
defineZoneTextProgram z ("TimeNot",x,eTime) = (parseZone timeNot) z x eTime
defineZoneTextProgram z d@("Seis8s",x,eTime) = do
  let parseResult = Seis8s.parseLang $ T.unpack x
  case parseResult of
    Right p -> do
      clearZoneError z
      setBaseDefinition z d
      modify' $ \xx -> xx { seis8ses = insert z p $ seis8ses xx }
    Left e -> setZoneError z (T.pack $ show e)
defineZoneTextProgram z ("",x,eTime) = do
  ns <- (Map.keys . jsoLangs) <$> get
  case determineTextNotation x ns of
    Left err -> setZoneError z (T.pack $ show err)
    Right (x',n) -> do
      case n of
        "" -> do
          case T.filter (\c -> not (isControl c) && not (isSpace c)) x' of
            "" -> do -- notation is unspecified but there is also no program, so okay
              clearZoneError z
              clearBaseDefinition z
            _ -> setZoneError z "no base notation specified"
        _ -> defineZoneTextProgram z (n,x',eTime)
defineZoneTextProgram z d@("JSoLang",y,eTime) = do
  let x = "JSoLang'" -- TODO: replace this placeholder which hard codes JSoLangs to produce a notation called JSoLang'
  parseResult <- liftIO $ JSoLang.define y
  case parseResult of
    Right j -> do
      clearZoneError z
      setBaseDefinition d
      modify' $ \xx -> xx { jsoLangs = Map.insert x j $ jsoLangs xx }
      liftIO $ T.putStrLn $ "defined JSoLang " <> x
    Left e -> setZoneError z (T.pack $ show e)
defineZoneTextProgram z (x,y,eTime) = do
  maybeJSoLang <- (Map.lookup x . jsoLangs) <$> get
  case maybeJSoLang of
    Just j -> do
      parseResult <- liftIO $ JSoLang.parse j y
      case parseResult of
        Right x' -> do
          liftIO $ T.putStrLn $ "result of parsing " <> x <> ":"
          liftIO $ T.putStrLn x'
          defineZoneTextProgram z ("",x',eTime)
        Left e -> setZoneError z e
    Nothing -> setZoneError z $ "renderTextProgramChanged: no language called " <> x <> " exists"

defineZoneGeneric :: Int -> Definition -> Renderer -> R ()
defineZoneGeneric z d r = do
  result <- liftIO $ (Renderer.defineZone r) z d
  case r of 
    Left err -> setZoneError z err
    Right _ -> do
      setBaseDefinition z d
      clearZoneError z

defineZoneHydra :: Int -> Definition -> R ()
defineZoneHydra z d@(_,txt,eTime) = do
 s <- get
 parseResult <- liftIO $ try $ return $! Hydra.parseHydra t
 case parseResult of
   Right (Right stmts) -> do
     clearZoneError z
     setBaseDefinition z d
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



renderZone :: Bool -> Int -> Definition -> R ()
renderZone _ z (Sequence _) = renderControlPattern z
renderZone _ z (TidalStructure _) = renderControlPattern z
renderZone canDraw z (TextProgram x) = renderZoneText canDraw z x
renderZone _ _ _ = pure ()

renderZoneText :: Bool -> Int -> Live TextProgram -> R ()
renderZoneText canDraw z x = do
  let (textNotation,_,_) = forRendering x
  renderZoneBase canDraw z textNotation 

renderZoneBase :: Bool -> Int -> TextNotation -> R ()
renderZoneBase canDraw z "MiniTidal" = gets miniTidal >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "Punctual" = gets punctual >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "LocoMotion" = when canDraw $ do
  lm <- gets locoMotion
  tNow <- gets wakeTimeAnimation
  (zoneAnimationFrame $ exoLangToRenderer "LocoMotion" lm) tNow z
renderZoneBase canDraw z "TransMit" = when canDraw $ do
  tm <- gets transMit
  tNow <- gets wakeTimeAnimation
  (zoneAnimationFrame $ exoLangToRenderer "TransMit" tm) tNow z
renderZoneBase _ z "TimeNot" = (scheduleWebDirtEvents timeNot) z >>= pushWebDirtEvents
renderZoneBase _ z "Seis8s" = do
  s <- get
  let p = IntMap.lookup z $ seis8ses s
  case p of
    Just p' -> do
      let theTempo = tempoCache s
      let wStart = renderStart s
      let wEnd = renderEnd s
      pushNoteEvents $ Seis8s.render p' theTempo wStart wEnd
    Nothing -> return ()
renderZoneBase canDraw z "CineCero" = when canDraw $ do
  tNow <- gets wakeTimeAnimation
  (zoneAnimationFrame cineCer0) tNow z
renderZoneBase canDraw z "Hydra" = when canDraw $ do
  s <- get
  let wta = wakeTimeAnimation s
  let elapsed = realToFrac $ diffUTCTime tNow wta * 1000
  let x = IntMap.lookup z $ hydras s
  case x of
    Just hydra -> liftIO $ Hydra.tick hydra elapsed
    Nothing -> pure ()
renderZoneBase _ _ _ = pure ()

renderZoneGeneric :: Bool -> Int -> Renderer -> R ()
renderZoneGeneric canDraw z r = do
  gets tempoCache >>= (liftIO . setTempo r)
  gets valueMap >>= (liftIO . setValueMap r)
  tNow <- gets wakeTimeAnimation
  wStart <- gets renderStart
  wEnd <- gets renderEnd
  liftIO $ preRender r canDraw
  ns <- liftIO $ renderZone mt tNow wStart wEnd canDraw z
  pushNoteEvents ns

renderControlPattern :: Int -> R () -- used by Tidal structure editor, and Sequencer
renderControlPattern z = do
  wdOn <- webDirtOn
  sdOn <- superDirtOn
  when (wdOn || sdOn) $ do
    s <- get
    let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
    let vMap = valueMap s
    case controlPattern of
      Just controlPattern' -> do
        let lt = renderStart s
        let rp = renderPeriod s
        ns <- liftIO $ (return $! force $ MiniTidal.renderTidalPattern vMap lt rp (tempoCache s) controlPattern')
          `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
        pushNoteEvents $ tidalEventToNoteEvent <$> ns
      Nothing -> pure ()



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

  
forkRenderThreads :: RenderEnvironment -> Settings.Settings -> HTMLDivElement -> HTMLCanvasElement -> HTMLCanvasElement -> HTMLCanvasElement -> IO ()
forkRenderThreads rEnv s vidDiv cvsElement hCanvas lCanvas = do
  t0Audio <- liftAudioIO $ audioTime
  t0System <- getCurrentTime
  pIn <- getPunctualInput $ mainBus rEnv
  pOut <- getMainBusInput $ mainBus rEnv
  irs <- initialRenderState pIn pOut cvsElement hCanvas lCanvas t0System t0Audio
  let irs' = irs { videoDivCache = Just vidDiv }
  rsM <- newMVar irs'
  void $ forkIO $ mainRenderThread rEnv rsM
  void $ forkIO $ animationThread rEnv rsM

mainRenderThread :: RenderEnvironment -> MVar RenderState -> IO ()
mainRenderThread rEnv rsM = do
  rs <- takeMVar rsM
  rs' <- runR render rEnv rs
  putMVar rsM rs'
  swapMVar (renderInfo rEnv) (info rs') -- copy RenderInfo from state into MVar for instant reading elsewhere
  -- _ <- runR sleepIfNecessary rEnv rs'
  threadDelay mainRenderThreadSleepTime
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

