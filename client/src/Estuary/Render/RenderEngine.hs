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

import Sound.MusicW.AudioContext (liftAudioIO)

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
  let cDiff = (systemTime s, audioTime s)
  when wdOn $ liftIO $ mapM_ (WebDirt.playSample (webDirt rEnv) (resources rEnv) unsafe cDiff) ns
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
  let elapsedSystem = (realToFrac $ diffUTCTime t1System $ systemTime s) :: Double
  let elapsedAudio = t1Audio - audioTime s
  let cr = elapsedAudio / elapsedSystem
  let crProblem = cr < clockRatioThreshold && cr > 0
  modify' $ \x -> x {
    systemTime = t1System,
    audioTime = t1Audio,
    info = (info x) { clockRatio = cr, clockRatioProblem = crProblem }
  }
  when crProblem $ liftIO $ T.putStrLn $ "audio clock slower, ratio=" <> showt cr

  let diff = diffUTCTime (windowEnd s) t1System

  -- 1. Fast Forward
  -- if the logical endtime of previous rendering is very soon in the future (below a threshold), or is in the past
  -- then skip ahead so that the logical starttime of new rendering is at least in the future by that threshold
  let fastForward = diff < fastForwardThreshold
  when fastForward $ do
    liftIO $ T.putStrLn "FAST-FORWARD"
    modify' $ \x -> x {
      windowStart = addUTCTime fastForwardThreshold t1System,
      windowPeriod = mainRenderPeriod,
      windowEnd = addUTCTime (mainRenderPeriod+fastForwardThreshold) t1System
    }

  -- 2. Catchup
  let catchup = diff >= fastForwardThreshold && diff < catchupThreshold
  when catchup $ do
    liftIO $ T.putStrLn $ "CATCHUP"
    modify' $ \x -> x {
      windowStart = windowEnd s,
      windowPeriod = mainRenderPeriod,
      windowEnd = addUTCTime mainRenderPeriod (windowEnd s)
    }

  -- 3. Wait
  -- let wait = diff >= waitThreshold && diff < rewindThreshold
 
  -- 4. Rewind
  let rewind = (diff >= rewindThreshold)
  when rewind $ do
    liftIO $ T.putStrLn $ "REWIND"
    modify' $ \x -> x {
      windowStart = addUTCTime fastForwardThreshold t1System,
      windowPeriod = mainRenderPeriod,
      windowEnd = addUTCTime (mainRenderPeriod+fastForwardThreshold) t1System
    }
  
  processRenderOps
  when (fastForward || catchup || rewind) $ do
    updateTidalValueMap
    renderZones
    flushEvents

  -- calculate how much time this render cycle took and update load measurements
  -- TODO: these calculations don't really make sense the way the rendering system has evolved
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
  let diff = diffUTCTime (windowEnd s) tNow
  let microRender = diff >= microRenderThreshold && diff < waitThreshold
  when microRender $ do
    fpsl <- fpsLimit
    let mDelta = realToFrac $ getAverage $ animationDelta s
    let microRenderPeriod = calculateMicroRenderPeriod mDelta fpsl
    modify' $ \x -> x {
      windowStart = windowEnd s,
      windowEnd = addUTCTime microRenderPeriod (windowEnd s),
      windowPeriod = microRenderPeriod
    }
    updateTidalValueMap
    renderZones True
    flushEvents

calculateMicroRenderPeriod :: NominalDiffTime -> Maybe NominalDiffTime -> NominalDiffTime
calculateMicroRenderPeriod _ (Just fpsl) = fpsl
calculateMicroRenderPeriod measuredDelta Nothing = measuredDelta


processRenderOps :: R ()
processRenderOps = takeRenderOps >>= foldM processRenderOp ()

processRenderOp :: () -> RenderOp -> R ()

processRenderOp _ (WriteTempo t) = do
  modify' $ \s -> s { tempo = t }

processRenderOp _ (WriteZone z x) = do
  defs <- gets cachedDefs
  let x' = definitionForRendering x
  maybeClearChangedZone z (IntMap.lookup z defs) x'
  renderZoneChanged z x'
  modify' $ \s -> s { cachedDefs = insert z x' (cachedDefs s) }

processRenderOp _ ResetZones = do
  gets cachedDefs >>= traverseWithKey clearZone
  modify' $ \s -> s { cachedDefs = empty }



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
clearZone z (TextProgram x) = clearTextProgramGeneric z
clearZone z (Sequence _) = clearParamPattern z
clearZone _ _ = return ()

clearTextProgramGeneric :: Int -> R ()
clearTextProgramGeneric z = do
  mr <- getActiveRenderer z
  case mr of
    Nothing -> pure ()
    Just r -> clear r z
  clearActiveRenderer z


preRenderRenderPostRender :: Bool -> R ()
preRenderRenderPostRender canDraw = do
  rs <- getActiveRenderers
  -- preRender
  tNow <- gets frameSystemTime
  tPrev <- gets prevDrawTime
  liftIO $ traverse_ (\r -> (preRender r) canDraw tNow tPrev) activeRenderers
  -- render for each active zone
  traverseWithKey_ (renderZone canDraw) defs
  -- postRender
  liftIO $ traverse_ (\r -> (postRender r) canDraw tNow tPrev) activeRenderers
  -- if canDraw, update record of previous drawing times
  when canDraw $ modify' $ \s -> s { prevDrawTime = tNow }
      

{-
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
-}

defineZone :: Int -> Definition -> R ()
defineZone z d@(TidalStructure x) = do
  let newParamPattern = toParamPattern x
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
  setBaseDefinition z d 
defineZone z d@(Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
  setBaseDefinition z d
defineZone z d@(TextProgram x) = defineZoneTextProgram z d $ forRendering x
defineZone _ _ = return ()

defineZoneTextProgram :: Int -> Definition -> (TextNotation,Text,UTCTime) -> R ()
defineZoneTextProgram z d ("MiniTidal",_,_) = defineZoneGeneric z d "MiniTidal"
defineZoneTextProgram z d ("Punctual",_,_) = defineZoneGeneric z d "Punctual"
defineZoneTextProgram z d ("CineCer0",_,_) = defineZoneGeneric z d "CineCer0"
defineZoneTextProgram z d ("LocoMotion",_,_) = defineZoneGeneric z d "LocoMotion"
defineZoneTextProgram z d ("TransMit",_,_) = defineZoneGeneric z d "TransMit"
defineZoneTextProgram z d ("Seis8s",_,_) = defineZoneGeneric z d "Seis8s"
defineZoneTextProgram z d ("Hydra",_,_) = defineZoneGeneric z d "Hydra"
defineZoneTextProgram z d ("TimeNot",_,_) = defineZoneGeneric z d  "TimeNot"
defineZoneTextProgram z d ("",x,eTime) = do
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
        _ -> defineZoneTextProgram z d (n,x',eTime)
defineZoneTextProgram z d ("JSoLang",y,eTime) = do
  let x = "JSoLang'" -- TODO: replace this placeholder which hard codes JSoLangs to produce a notation called JSoLang'
  parseResult <- liftIO $ JSoLang.define y
  case parseResult of
    Right j -> do
      clearZoneError z
      setBaseDefinition d
      modify' $ \xx -> xx { jsoLangs = Map.insert x j $ jsoLangs xx }
      liftIO $ T.putStrLn $ "defined JSoLang " <> x
    Left e -> setZoneError z (T.pack $ show e)
defineZoneTextProgram z d (x,y,eTime) = do
  maybeJSoLang <- (Map.lookup x . jsoLangs) <$> get
  case maybeJSoLang of
    Just j -> do
      parseResult <- liftIO $ JSoLang.parse j y
      case parseResult of
        Right x' -> do
          liftIO $ T.putStrLn $ "result of parsing " <> x <> ":"
          liftIO $ T.putStrLn x'
          defineZoneTextProgram z d ("",x',eTime)
        Left e -> setZoneError z e
    Nothing -> setZoneError z $ "renderTextProgramChanged: no language called " <> x <> " exists"

defineZoneGeneric :: Int -> Definition -> Text -> R ()
defineZoneGeneric z d rName = do
  rs <- gets allRenderers
  case Map.lookup rName rs of
    Nothing -> do
      let err = "internal error in defineZoneGeneric: no renderer named " <> rName
      T.putStrLn err
      setZoneError z err
    Just r -> do
      result <- liftIO $ (define r) z d
      case result of 
        Left err -> setZoneError z err
        Right _ -> do
          setBaseDefinition z d
          setBaseRenderer z r
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
  gets tempo >>= (liftIO . setTempo r)
  gets valueMap >>= (liftIO . setValueMap r)
  tNow <- gets systemTime
  tPrev <- gets prevDrawTime
  wStart <- gets windowStart
  wEnd <- gets windowEnd
  liftIO $ preRender r canDraw
  ns <- liftIO $ renderZone r tNow tPrev wStart wEnd canDraw z
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
        let lt = windowStart s
        let rp = windowPeriod s
        ns <- liftIO $ (return $! force $ MiniTidal.renderTidalPattern vMap lt rp (tempo s) controlPattern')
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
  rsRef <- newIORef irs
  void $ forkIO $ mainThread rEnv rsRef
  void $ forkIO $ animationThread rEnv rsRef

mainThread :: RenderEnvironment -> IORef RenderState -> IO ()
mainThread rEnv rsRef = do
  rs <- readIORef rsRef
  rs' <- runR Estuary.Render.RenderEngine.render rEnv rs
  writeIORef rsRef rs'
  swapMVar (renderInfo rEnv) (info rs') -- copy RenderInfo from state into MVar for instant reading elsewhere TODO: replace MVar with IORef, allow renderInfo to be updated on the spot so this operation is unnecessary
  threadDelay mainRenderThreadSleepTime
  mainRenderThread rEnv rsRef

animationThread :: RenderEnvironment -> IORef RenderState -> IO ()
animationThread rEnv rsRef = void $ inAnimationFrame ContinueAsync $ \_ -> do
  animOn <- Settings.canvasOn <$> (readIORef $ _settings rEnv)
  when animOn $ do
    rs <- readIORef rsRef
    rs' <- runR renderAnimation rEnv rs
    writeIORef rsRef rs'
  animationThread rEnv rsRef

