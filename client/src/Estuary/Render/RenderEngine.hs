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

import qualified Estuary.Languages.MiniTidal as MiniTidal

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
import Estuary.Render.RenderOp
import Estuary.Render.Renderer
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
  let ns = noteEvents s
  -- maybe send events to WebSerial (no modification of underlying JSVals)
  webSerialOn <- liftIO $ WebSerial.isActive (webSerial rEnv)
  when webSerialOn $ liftIO $ mapM_ (WebSerial.send (webSerial rEnv)) ns
  -- maybe send events to SuperDirt (no modification of underlying JSVals)   
  sdOn <- superDirtOn
  when sdOn $ liftIO $ mapM_ (SuperDirt.playSample (superDirt rEnv)) ns
  -- maybe send events to WebDirt (note: WebDirt modifies underlying JSVals)
  wdOn <- webDirtOn   
  unsafe <- unsafeModeOn
  when wdOn $ liftIO $ mapM_ (WebDirt.playSample (webDirt rEnv) (resources rEnv) unsafe clockDiff) ns
  modify' $ \x -> x { noteEvents = [] } -- clear the queue of all note events
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

-- TODO: how is clearZone handled for JSoLangs????
clearTextProgram :: Int -> (TextNotation,Text,UTCTime) -> R ()
clearTextProgram z ("MiniTidal",_,_) = gets miniTidal >>= clearTextProgramGeneric z
clearTextProgram z ("Punctual",_,_) = gets punctual >>= clearTextProgramGeneric z
clearTextProgram z ("CineCer0",_,_) = gets cineCer0 >>= clearTextProgramGeneric z
clearTextProgram z ("Seis8s",_,_) = gets seis8s >>= clearTextProgramGeneric z
clearTextProgram z ("LocoMotion",_,_) = gets locoMotion >>= clearTextProgramGeneric z
clearTextProgram z ("TransMit",_,_) = gets transMit >>= clearTextProgramGeneric z
clearTextProgram z ("Hydra",_,_) = gets hydra >>= clearTextProgramGeneric z
clearTextProgram z ("TimeNot",_,_) = gets timeNot >>= clearTextProgramGeneric z
clearTextProgram _ _ = return ()

clearTextProgramGeneric :: Int -> Renderer -> R ()
clearTextProgramGeneric z r = liftIO $ clear r z


renderAnimation :: R ()
renderAnimation = do
  t1 <- liftIO $ getCurrentTime
  wta <- gets wakeTimeAnimation
  fpsl <- fpsLimit
  let okToRender = case fpsl of Nothing -> True; Just x -> diffUTCTime t1 wta > x
  rEnv <- ask
  liftIO $ WebSerial.flush (webSerial rEnv)
  punctual' <- gets punctual
  
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
defineZoneTextProgram z d@("CineCer0",_,_) = gets cineCer0 >>= defineZoneGeneric z d
defineZoneTextProgram z d@("LocoMotion",_,_) = gets locoMotion >>= defineZoneGeneric z d
defineZoneTextProgram z d@("TransMit",_,_) = gets transMit >>= defineZoneGeneric z d
defineZoneTextProgram z d@("Seis8s",_,_) = gets seis8s >>= defineZoneGeneric z d
defineZoneTextProgram z d@("Hydra",_,_) = gets hydra >>= defineZoneGeneric z d
defineZoneTextProgram z d@("TimeNot",_,_) = gets timeNot >>= defineZoneGeneric z d 
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
  result <- liftIO $ (define r) z d
  case r of 
    Left err -> setZoneError z err
    Right _ -> do
      setBaseDefinition z d
      clearZoneError z


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
renderZoneBase canDraw z "CineCer0" = gets cineCer0 >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "Seis8s" = gets seis8s >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "LocoMotion" = gets locoMotion >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "TransMit" = gets transMit >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "Hydra" = gets hydra >>= renderZoneGeneric canDraw z
renderZoneBase canDraw z "TimeNot" = gets timeNot >>= renderZoneGeneric canDraw z
renderZoneBase _ _ _ = pure ()

renderZoneGeneric :: Bool -> Int -> Renderer -> R ()
renderZoneGeneric canDraw z r = do
  gets tempoCache >>= (liftIO . setTempo r)
  gets valueMap >>= (liftIO . setValueMap r)
  tNow <- gets wakeTimeAnimation
  tPrevDraw <- gets prevDrawTime
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
forkRenderThreads rEnv s cineCer0Div pCanvas lCanvas hCanvas = do
  t0Audio <- liftAudioIO $ audioTime
  t0System <- getCurrentTime
  pIn <- getPunctualInput $ mainBus rEnv
  pOut <- getMainBusInput $ mainBus rEnv
  irs <- initialRenderState pIn pOut cineCer0Div pCanvas lCanvas hCanvas t0System t0Audio
  rsM <- newMVar irs
  void $ forkIO $ mainRenderThread rEnv rsM
  void $ forkIO $ animationThread rEnv rsM

mainRenderThread :: RenderEnvironment -> MVar RenderState -> IO ()
mainRenderThread rEnv rsM = do
  rs <- takeMVar rsM
  rs' <- runR Estuary.Render.RenderEngine.render rEnv rs
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

