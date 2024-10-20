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

import qualified Sound.MusicW.AudioContext as MusicW

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
import Estuary.Render.R as R
import Estuary.Render.RenderEnvironment as RenderEnvironment
import Estuary.Render.RenderOp
import Estuary.Render.Renderer
import qualified Estuary.Client.Settings as Settings
import Estuary.Render.WebSerial as WebSerial
import qualified Estuary.Resources as Resources


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


renderMainThread :: R ()
renderMainThread = do
  updateTimeMain
  processRenderOps
  R.updateActiveRenderers
  calculateEventRenderingWindow mainRenderPeriod
  s <- get
  when (windowPeriod s /= 0) $ do
    updateTidalValueMap
    renderZones False
    flushEvents
  R.updateWebDirtVoices
  updateRenderLoad


-- to be done at the beginning of a render frame (in main thread only, not animation)
updateTimeMain :: R ()
updateTimeMain = do
  t1System <- liftIO $ getCurrentTime
  t1Audio <- MusicW.liftAudioIO $ MusicW.audioTime
  modify' $ \x -> x {
    systemTime = t1System,
    audioTime = t1Audio
    }
  -- let elapsedSystem = (realToFrac $ diffUTCTime t1System $ systemTime s) :: Double
  -- let elapsedAudio = t1Audio - audioTime s
  
-- to be done at the beginning of an animation frame only
updateTimeAnimation :: R Bool
updateTimeAnimation = do
  s <- get
  t1System <- liftIO $ getCurrentTime
  t1Audio <- MusicW.liftAudioIO $ MusicW.audioTime
  let immediateDelta = realToFrac $ diffUTCTime t1System (prevDrawTime s)
  fpsl <- fpsLimit
  let okToRender = case fpsl of Nothing -> True; Just x -> immediateDelta >= (realToFrac x*0.95)
  case okToRender of
    False -> pure False
    True -> do
      let newAnimationDelta = updateAverage (animationDelta s) immediateDelta
      let fps = round $ 1 / getAverage newAnimationDelta
      modify' $ \x -> x {
        systemTime = t1System,
        audioTime = t1Audio,
        prevDrawTime = systemTime s,
        animationDelta = newAnimationDelta
      }
      rEnv <- ask
      modifyRenderInfo rEnv $ \i -> i { animationFPS = fps }
      pure True
 
-- to be done at the end of a render frame (whether in main or animation thread)  
updateRenderLoad :: R () 
updateRenderLoad = do
  s <- get
  t2System <- liftIO $ getCurrentTime
  let mostRecentRenderTime = diffUTCTime t2System (systemTime s)
  let newRenderTime = updateAverage (renderTime s) $ realToFrac mostRecentRenderTime
  let newRenderedTime = updateAverage (renderedTime s) $ realToFrac (windowPeriod s)
  let newRenderLoad = ceiling (getAverage newRenderTime * 100 / getAverage newRenderedTime)
  modify' $ \x -> x {
    renderTime = newRenderTime,
    renderedTime = newRenderedTime
    }
  rEnv <- ask
  modifyRenderInfo rEnv $ \i -> i { avgRenderLoad = newRenderLoad }

  
  
calculateEventRenderingWindow :: NominalDiffTime -> R ()
calculateEventRenderingWindow renderPeriodTarget = do
  s <- get 
  let diff = diffUTCTime (windowEnd s) (systemTime s)
  
  -- 1. Fast Forward or Rewind
  -- if the logical endtime of previous rendering is too soon in the future (below fastForwardThreshold), or is in the past
  -- then fast forward ahead so that the logical starttime of new rendering is at least in the future by that threshold
  -- if the logical endtime of previous rendering is very far in the future (above rewindThreshold),
  -- then rewind. 
  let fastForward = diff < fastForwardThreshold
  let rewind = (diff >= rewindThreshold)
  when fastForward $ liftIO $ T.putStrLn "FAST-FORWARD"
  when rewind $ liftIO $ T.putStrLn "REWIND"
  when (fastForward || rewind) $ do
    modify' $ \x -> x {
      windowStart = addUTCTime fastForwardThreshold (systemTime s),
      windowPeriod = renderPeriodTarget,
      windowEnd = addUTCTime (renderPeriodTarget+fastForwardThreshold) (systemTime s)
      }
    
  -- 2. Normal
  -- if the logical endtime of previous rendering is sufficiently in the future (above fastForwardThreshold)
  -- but not so far in the future that we should wait (below waitThreshold), render the full renderPeriodTarget
  let normal = diff >= fastForwardThreshold && diff < waitThreshold
  when normal $ do
    modify' $ \x -> x {
      windowStart = windowEnd x,
      windowPeriod = renderPeriodTarget,
      windowEnd = addUTCTime renderPeriodTarget (windowEnd x)
      }
  
  -- 3. Wait
  -- if the logical endtime of previous rendering is quite far in the future (above waitThreshold)
  -- but not so far in the future that we are going to rewind (below rewindThreshold)
  -- then signal that no events should be rendered by holding windowEnd and setting windowStart to equal it
  let wait = diff >= waitThreshold && diff < rewindThreshold
  when wait $ do
    modify' $ \x -> x {
      windowStart = windowEnd x,
      windowPeriod = 0
      }


renderAnimationFrame :: R ()
renderAnimationFrame = do
  canRender <- updateTimeAnimation
  when canRender $ do
    s <- get
    let mDelta = realToFrac $ getAverage $ animationDelta s
    fpsl <- fpsLimit
    let p = calculateMicroRenderPeriod mDelta fpsl
    calculateEventRenderingWindow p
    renderZones True
    flushEvents
    R.updateWebDirtVoices
    updateRenderLoad

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
  let x' = sanitizeDefinition $ definitionForRendering x
  maybeClearChangedZone z (IntMap.lookup z defs) x'
  defineZone z x'
  modify' $ \s -> s { cachedDefs = insert z x' (cachedDefs s) }

processRenderOp _ ResetZones = do
  gets cachedDefs >>= traverseWithKey clearZone
  modify' $ \s -> s { cachedDefs = empty }

sanitizeDefinition :: Definition -> Definition
sanitizeDefinition (TextProgram x) = TextProgram $ sanitizeLiveTextProgram x
sanitizeDefinition x = x

sanitizeLiveTextProgram :: Live TextProgram -> Live TextProgram
sanitizeLiveTextProgram (Live a lness) = Live (sanitizeTextProgram a) lness
sanitizeLiveTextProgram (Edited a b) = Edited (sanitizeTextProgram a) (sanitizeTextProgram b)

sanitizeTextProgram :: TextProgram -> TextProgram
sanitizeTextProgram (tn,txt,eTime) = (tn,sanitizeText txt,eTime)

sanitizeText :: Text -> Text
sanitizeText = T.map sanitizeChar 

sanitizeChar :: Char -> Char
sanitizeChar '“' = '"'
sanitizeChar '”' = '"'
sanitizeChar x = x

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
  mr <- R.getActiveRenderer z
  case mr of
    Nothing -> pure ()
    Just r -> liftIO $ clear r z
  R.clearBaseRenderer z


renderZones :: Bool -> R ()
renderZones canDraw = do
  -- preRender
  tNow <- gets systemTime
  tPrev <- gets prevDrawTime
  rs <- R.getActiveRenderers
  res <- resolution
  liftIO $ mapM_ (\r -> (setResolution r) res) rs -- later optimize so that these setters are only called when settings actual change not every frame...
  b <- brightness
  liftIO $ mapM_ (\r -> (setBrightness r) b) rs  
  liftIO $ mapM_ (\r -> (preRender r) canDraw tNow tPrev) rs
  -- render for each active zone
  rEnv <- ask
  defs <- getBaseDefinitions rEnv
  IntMap.traverseWithKey (renderZone canDraw) defs
  -- postRender
  liftIO $ mapM_ (\r -> (postRender r) canDraw tNow tPrev) rs
      

defineZone :: Int -> Definition -> R ()
defineZone z d@(TidalStructure x) = do
  let newParamPattern = toParamPattern x
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
  R.setBaseDefinition z d 
defineZone z d@(Sequence xs) = do
  let newParamPattern = Tidal.stack $ Map.elems $ Map.map sequenceToControlPattern xs
  modify' $ \s -> s { paramPatterns = insert z newParamPattern (paramPatterns s) }
  R.setBaseDefinition z d
defineZone z d@(TextProgram x) = defineZoneTextProgram z (forRendering x) ""
defineZone _ _ = return ()


defineZoneTextProgram :: Int -> TextProgram -> Text -> R ()

defineZoneTextProgram z ("",txt,eTime) _ = do
  case determineTextNotation txt of
    ("",_,txt') -> do -- if there's still no language specified, check if there's actually any program at all
      case T.filter (\c -> not (isControl c) && not (isSpace c)) txt' of
        "" -> do -- notation is unspecified but there is also no program, so okay
          R.clearZoneError z
          R.clearBaseDefinition z
          R.clearBaseRenderer z
        _ -> R.setZoneError z "no language specified"
    (tn,options,txt') -> defineZoneTextProgram z (tn,txt',eTime) options
    
defineZoneTextProgram z ("jsolang",txt,eTime) options = do
  case determineJSoLangName options of
    "" -> R.setZoneError z "JSoLang requires a name (hint: after ##JSoLang)"
    jsoLangName -> do
      parseResult <- liftIO $ JSoLang.define txt
      case parseResult of
        Right j -> do
          R.clearZoneError z
          R.clearBaseDefinition z
          R.clearBaseRenderer z
          modify' $ \s -> s { jsoLangs = Map.insert jsoLangName j $ jsoLangs s }
          liftIO $ T.putStrLn $ "defined JSoLang " <> jsoLangName
        Left e -> R.setZoneError z (T.pack $ show e)
        
defineZoneTextProgram z (tn,txt,eTime) options = do
  maybeJSoLang <- Map.lookup tn <$> gets jsoLangs
  case maybeJSoLang of -- is it a JSoLang?
    Just j -> do
      parseResult <- liftIO $ JSoLang.parse j txt
      case parseResult of
        Right txt' -> defineZoneTextProgram z ("",txt',eTime) ""
        Left e -> R.setZoneError z e
    Nothing -> defineZoneExistingRenderer z (tn,txt,eTime) options 
    
defineZoneExistingRenderer :: Int -> TextProgram -> Text -> R ()
defineZoneExistingRenderer z (rName,txt,eTime) options = do
  rEnv <- ask
  rs <- liftIO $ readIORef (allRenderers rEnv)
  case Map.lookup rName rs of -- or is it an existing renderer?
    Just r -> defineZoneExoLang r z (rName,txt,eTime) options
    Nothing -> defineZoneNewExoLang z (rName,txt,eTime) options
    
defineZoneNewExoLang :: Int -> TextProgram -> Text -> R ()
defineZoneNewExoLang z (rName,txt,eTime) options = do
  rEnv <- ask
  mURL <- Resources.findExoLangURL (resources rEnv) rName
  case mURL of -- or is it an ExoLang that hasn't been loaded yet?
    Just url -> do
      r <- insertExoLang rEnv rName url
      defineZoneExoLang r z (rName,txt,eTime) options    
    Nothing -> R.setZoneError z $ "no language called " <> rName <> " exists"
    
defineZoneExoLang :: Renderer -> Int -> TextProgram -> Text -> R ()
defineZoneExoLang r z (rName,txt,eTime) options = do
  let d = TextProgram (Live (rName,txt,eTime) L3)
  rEnv <- ask
  liftIO $ do 
    n <- getAudioOutputs $ mainBus rEnv
    (setNchnls r) n -- TODO: possibly other options need to be sent to renderer as well
    let okCb z' _ = do
                      RenderEnvironment.clearZoneError rEnv z'
                      RenderEnvironment.setBaseRenderer rEnv z' rName
                      RenderEnvironment.setBaseDefinition rEnv z' d
    let errorCb z' err = RenderEnvironment.setZoneError rEnv z' err
    (define r) okCb errorCb z d
     
determineTextNotation :: Text -> (TextNotation,Text,Text) -- (text notation, options, text code with pragma removed)
determineTextNotation x =
  case T.isPrefixOf "##" x of
    False -> ("","",x)
    True -> (textNotation,options,newText)
      where 
        x' = T.lines x
        firstLine = head x'
        otherLines = tail x'
        (firstWord,restOfLine) = T.break isSpace firstLine
        textNotation = T.toLower $ T.drop 2 firstWord
        options = T.stripStart restOfLine
        newText = T.unlines ("":otherLines)

determineJSoLangName :: Text -> Text
determineJSoLangName x = T.toLower $ fst $ T.break isSpace $ T.stripStart x


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
renderZoneBase canDraw z tn = do
  mr <- R.getActiveRenderer z -- :: Maybe Renderer
  case mr of
    Just r -> renderZoneGeneric canDraw z r
    Nothing -> R.setZoneError z $ "no renderer for " <> tn <> " in zone " <> showt z
    
renderZoneGeneric :: Bool -> Int -> Renderer -> R ()
renderZoneGeneric canDraw z r = do
  gets tempo >>= (liftIO . setTempo r)
  gets valueMap >>= (liftIO . setValueMap r)
  tNow <- gets systemTime
  tPrev <- gets prevDrawTime
  wStart <- gets windowStart
  wEnd <- gets windowEnd
  ns <- liftIO $ render r tNow tPrev wStart wEnd canDraw z
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
        ns' <- mapM tidalEventToNoteEvent ns
        pushNoteEvents ns'
      Nothing -> pure ()


calculateZoneRenderTimes :: Int -> MovingAverage -> R ()
calculateZoneRenderTimes z zrt = do
  rEnv <- ask
  modifyRenderInfo rEnv $ \i -> i { avgZoneRenderTime = insert z (getAverage zrt) (avgZoneRenderTime i) }
  
calculateZoneAnimationTimes :: Int -> MovingAverage -> R ()
calculateZoneAnimationTimes z zat = do
  rEnv <- ask
  modifyRenderInfo rEnv $ \i -> i { avgZoneAnimationTime = insert z (getAverage zat) (avgZoneAnimationTime i) }

  
forkRenderThreads :: RenderEnvironment -> Settings.Settings -> HTMLDivElement -> HTMLCanvasElement -> HTMLCanvasElement -> HTMLCanvasElement -> IO ()
forkRenderThreads rEnv s cineCer0Div pCanvas lCanvas hCanvas = do
  t0Audio <- MusicW.liftAudioIO $ MusicW.audioTime
  t0System <- getCurrentTime
  irs <- initialRenderState cineCer0Div pCanvas lCanvas hCanvas t0System t0Audio
  rsRef <- newIORef irs
  void $ forkIO $ mainThread rEnv rsRef
  void $ forkIO $ animationThread rEnv rsRef

mainThread :: RenderEnvironment -> IORef RenderState -> IO ()
mainThread rEnv rsRef = do
  rs <- readIORef rsRef
  rs' <- runR renderMainThread rEnv rs
  writeIORef rsRef rs'
  -- swapMVar (renderInfo rEnv) (info rs') -- copy RenderInfo from state into MVar for instant reading elsewhere TODO: replace MVar with IORef, allow renderInfo to be updated on the spot so this operation is unnecessary
  threadDelay mainRenderThreadSleepTime
  mainThread rEnv rsRef

animationThread :: RenderEnvironment -> IORef RenderState -> IO ()
animationThread rEnv rsRef = void $ inAnimationFrame ContinueAsync $ \_ -> do
  animOn <- Settings.canvasOn <$> (readIORef $ _settings rEnv)
  when animOn $ do
    rs <- readIORef rsRef
    rs' <- runR renderAnimationFrame rEnv rs
    writeIORef rsRef rs'
  animationThread rEnv rsRef

