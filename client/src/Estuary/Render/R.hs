{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.R where

import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception (evaluate,catch,SomeException,try)
import Control.Concurrent.MVar
import Data.IntMap.Strict as IntMap
import Data.Map.Strict as Map
import Data.IORef
import Sound.MusicW
import TextShow
import GHCJS.Types (JSVal)
import GHCJS.DOM.Types hiding (Text)
import Sound.MusicW.AudioContext
import Sound.MusicW.Node as MusicW

import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual
import qualified Sound.Punctual as Punctual
import Sound.Punctual.GL

import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.MovingAverage

import Estuary.Types.Definition
import Estuary.Types.RenderInfo
import Estuary.Types.NoteEvent
import Estuary.Types.MovingAverage
import Estuary.Types.TextNotation hiding (LocoMotion)
import qualified Estuary.Languages.CineCer0.CineCer0State as CineCer0
import qualified Estuary.Languages.CineCer0.Spec as CineCer0
import qualified Estuary.Languages.CineCer0.Parser as CineCer0
import qualified Sound.Seis8s.Program as Seis8s
import qualified Estuary.Languages.Hydra.Render as Hydra
import Estuary.Languages.JSoLang
import Estuary.Languages.ExoLang
import Estuary.Render.MainBus
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Render.RenderOp
import Estuary.Types.Tempo
import Estuary.Resources
import Estuary.Types.ResourceOp
import Estuary.Client.Settings as Settings
import Estuary.Types.Language
import Estuary.Render.WebSerial as WebSerial
import Estuary.Render.WebDirt as WebDirt
import Estuary.Types.RenderInfo
import Estuary.Render.RenderEnvironment
import Estuary.Languages.ExoLang
import Estuary.Languages.JSoLang

putRenderOps :: MonadIO m => RenderEnvironment -> [RenderOp] -> m ()
putRenderOps re x = liftIO $ do
  ops <- takeMVar $ renderOps re
  putMVar (renderOps re) $ ops ++ x

takeRenderOps :: R [RenderOp]
takeRenderOps = do
  mv <- asks renderOps
  ops <- liftIO $ takeMVar mv
  liftIO $ putMVar mv []
  -- when (length ops > 0) $ liftIO $ putStrLn $ show (length ops) ++ " ops"
  return ops


setCC :: Int -> Double -> RenderEnvironment -> IO ()
setCC n v rEnv = modifyIORef' (ccMap rEnv) $ Map.insert (showt n) v

getCC :: Int -> RenderEnvironment -> IO (Maybe Double)
getCC n irc = do
  m <- readIORef $ ccMap irc
  return $ Map.lookup (showt n) m

updateTidalValueMap :: R ()
updateTidalValueMap = do
  rEnv <- ask
  m <- liftIO $ readIORef $ ccMap rEnv
  modify' $ \x -> x { valueMap = fmap Tidal.toValue $ Map.mapKeys T.unpack m}


type R = ReaderT RenderEnvironment (StateT RenderState IO)

runR :: R a -> RenderEnvironment -> RenderState -> IO RenderState
runR r rEnv rState = do
  let r' = runReaderT r rEnv -- :: (StateT RenderState IO) a
  (execStateT r' rState)
  `catch` (\e -> putStrLn "runRenderer" >> putStrLn (show (e :: SomeException)) >> return rState)

pushNoteEvents :: [NoteEvent] -> R ()
pushNoteEvents xs = modify' $ \x -> x { noteEvents = noteEvents x ++ xs }

{- already obsolete?
pushTidalEvents :: [(UTCTime,Tidal.ValueMap)] -> R ()
pushTidalEvents = pushNoteEvents . fmap tidalEventToNoteEvent
-}

-- deprecated/temporary
pushWebDirtEvents :: [JSVal] -> R ()
pushWebDirtEvents xs = modify' $ \x -> x { webDirtEvents = webDirtEvents x ++ xs }


setZoneError :: Int -> Text -> R ()
setZoneError z t = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = IntMap.insert z t oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

clearZoneError :: Int -> R ()
clearZoneError z = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = IntMap.delete z oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

setBaseNotation :: Int -> TextNotation -> R ()
setBaseNotation z n = modify' $ \x -> x { baseNotations = IntMap.insert z n $ baseNotations x}

clearBaseNotation :: Int -> R ()
clearBaseNotation z = modify' $ \x -> x { baseNotations = IntMap.delete z $ baseNotations x}

setBaseDefinition :: Int -> Definition -> R ()
setBaseDefinition z n = modify' $ \x -> x { baseDefinitions = IntMap.insert z n $ baseDefinitions x}

clearBaseDefinition :: Int -> R ()
clearBaseDefinition z = modify' $ \x -> x { baseDefinitions = IntMap.delete z $ baseDefinitions x}

setEvaluationTime :: Int -> UTCTime -> R ()
setEvaluationTime z n = modify' $ \x -> x { evaluationTimes = IntMap.insert z n $ evaluationTimes x}

clearEvaluationTime :: Int -> R ()
clearEvaluationTime z = modify' $ \x -> x { evaluationTimes = IntMap.delete z $ evaluationTimes x}

clearParamPattern :: Int -> R ()
clearParamPattern z = modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }


-- updating Settings (eg. in response to user action in widgets)

updateSettings :: RenderEnvironment -> Settings -> IO ()
updateSettings re = writeIORef (_settings re)


-- asking/reading settings from within the R monad
-- (note: not all Settings are given accessors here, just those that might
-- be useful during computations with Estuary's two render threads.)

askSettings :: (Settings -> a) -> R a
askSettings f = asks _settings >>= liftIO . readIORef >>= return . f

language :: R Language
language = askSettings Settings.language

canvasOn :: R Bool
canvasOn = askSettings Settings.canvasOn

resolution :: R Punctual.Resolution
resolution = askSettings Settings.resolution

brightness :: R Double
brightness = askSettings Settings.brightness

fpsLimit :: R (Maybe NominalDiffTime)
fpsLimit = askSettings Settings.fpsLimit

cineCer0ZIndex :: R Int
cineCer0ZIndex = askSettings Settings.cineCer0ZIndex

punctualZIndex :: R Int
punctualZIndex = askSettings Settings.punctualZIndex

improvizZIndex :: R Int
improvizZIndex = askSettings Settings.improvizZIndex

hydraZIndex :: R Int
hydraZIndex = askSettings Settings.hydraZIndex

webDirtOn :: R Bool
webDirtOn = askSettings Settings.webDirtOn

unsafeModeOn :: R Bool
unsafeModeOn = askSettings Settings.unsafeModeOn

superDirtOn :: R Bool
superDirtOn = askSettings Settings.superDirtOn


updateWebDirtVoices :: R ()
updateWebDirtVoices = do
  wd <- asks webDirt
  n <- liftIO $ WebDirt.voices wd
  modify' $ \s -> s { info = (info s) { webDirtVoices = n } }
  
  
  
data TextNotationRenderer = TextNotationRenderer {
  parseZone :: Int -> Text -> UTCTime -> R (),
  scheduleNoteEvents :: Int -> R [NoteEvent],
  scheduleWebDirtEvents :: Int -> R [JSVal], -- deprecated/temporary
  clearZone' :: Int -> R (),
  zoneAnimationFrame :: UTCTime -> Int -> R (),
  preAnimationFrame :: R (),
  postAnimationFrame :: R ()
}

emptyTextNotationRenderer :: TextNotationRenderer
emptyTextNotationRenderer = TextNotationRenderer {
  parseZone = \_ _ _ -> return (),
  scheduleNoteEvents = \_ -> return [],
  scheduleWebDirtEvents = \_ -> return [],
  clearZone' = \_ -> return (),
  zoneAnimationFrame = \_ _ -> return (),
  preAnimationFrame = return (),
  postAnimationFrame = return ()
  }

exoLangToRenderer :: TextNotation -> ExoLang -> TextNotationRenderer
exoLangToRenderer tn exolang = emptyTextNotationRenderer {
  parseZone = parseZone' tn exolang,
  scheduleWebDirtEvents = scheduleWebDirtEvents' exolang,
  clearZone' = clearZone'' exolang,
  preAnimationFrame = preAnimationFrame' exolang,
  zoneAnimationFrame = zoneAnimationFrame' exolang,
  postAnimationFrame = postAnimationFrame' exolang
  }

parseZone' :: TextNotation -> ExoLang -> Int -> Text -> UTCTime -> R ()
parseZone' tn exoLang z txt eTime = do
  r <- liftIO $ Estuary.Languages.ExoLang.evaluate exoLang z txt
  case r of
    Just err -> do
      setBaseNotation z tn
      setZoneError z err
    Nothing -> do
      setBaseNotation z tn
      setEvaluationTime z eTime
      clearZoneError z

scheduleWebDirtEvents' :: ExoLang -> Int -> R [JSVal]
scheduleWebDirtEvents' exoLang z = do
  s <- get
  let wStart = renderStart s
  let wEnd = renderEnd s
  liftIO $ render exoLang z wStart wEnd

clearZone'' :: ExoLang -> Int -> R ()
clearZone'' exoLang z = do
  x <- liftIO $ try $ clearZone exoLang z
  case x of
    Right () -> pure ()
    Left exception -> do
      let msg = "Estuary: exception in clearZone'': " ++ show (exception :: SomeException)
      liftIO $ putStrLn msg
      pure ()
      

preAnimationFrame' :: ExoLang -> R ()
preAnimationFrame' exoLang = liftIO $ preAnimate exoLang

zoneAnimationFrame' :: ExoLang -> UTCTime -> Int -> R ()
zoneAnimationFrame' exoLang _ z = do
  s <- get
  liftIO $ setTempo exoLang (tempoCache s)
  liftIO $ animateZone exoLang z

postAnimationFrame' :: ExoLang -> R ()
postAnimationFrame' exoLang = liftIO $ postAnimate exoLang


newPunctual :: HTMLCanvasElement -> IO TextNotationRenderer
newPunctual cvs = do
  p <- Punctual.new cvs
  pure $ emptyTextNotationRenderer {
    parseZone = _parseZonePunctual p,
    clearZone' = _clearZonePunctual p,
    preAnimationFrame = _preAnimationFramePunctual p,
    zoneAnimationFrame = _zoneAnimationFramePunctual p,
    postAnimationFrame = _postAnimationFramePunctual p
    }


_parseZonePunctual :: Punctual.Punctual -> Int -> Text -> UTCTime -> R ()
_parseZonePunctual p z t eTime = do
  s <- get
  liftIO $ Punctual.setTempo p (tempoCache s)
  r <- liftIO $ try $ Punctual.evaluate p z t eTime
  case r of 
    Right (Right _) -> do
      setBaseNotation z Punctual
      setEvaluationTime z eTime
      clearZoneError z
      pure ()
    Right (Left parseErr) -> do
      setZoneError z $ T.pack $ show parseErr
    Left exception -> do
      setZoneError z $ T.pack $ show (exception :: SomeException)


_clearZonePunctual :: Punctual.Punctual -> Int -> R ()
_clearZonePunctual p z = do
  liftIO $ Punctual.clear p z
  clearZoneError z
  clearBaseNotation z


_preAnimationFramePunctual :: Punctual.Punctual -> R ()
_preAnimationFramePunctual p = do
  s <- get
  res <- Estuary.Render.R.resolution
  b <- Estuary.Render.R.brightness
  liftIO $ Punctual.setResolution p res
  liftIO $ Punctual.setBrightness p b
  liftIO $ Punctual.setTempo p (tempoCache s)


_zoneAnimationFramePunctual :: Punctual.Punctual -> UTCTime -> Int -> R ()
_zoneAnimationFramePunctual p tNow z = liftIO $ Punctual.render p True z tNow
  

_postAnimationFramePunctual :: Punctual.Punctual -> R ()
_postAnimationFramePunctual p = liftIO $ Punctual.postRender p True


newtype LocoMotion = LocoMotion JSVal

instance PToJSVal LocoMotion where pToJSVal (Estuary.Render.R.LocoMotion x) = x

instance PFromJSVal LocoMotion where pFromJSVal = Estuary.Render.R.LocoMotion

data RenderState = RenderState {
  wakeTimeAudio :: !Double,
  wakeTimeSystem :: !UTCTime,
  renderStart :: !UTCTime,
  renderPeriod :: !NominalDiffTime,
  renderEnd :: !UTCTime,
  cachedDefs :: !DefinitionMap,
  paramPatterns :: !(IntMap Tidal.ControlPattern),
  noteEvents :: ![NoteEvent],
  webDirtEvents :: ![JSVal], -- deprecated/temporary
  baseNotations :: !(IntMap TextNotation),
  baseDefinitions :: !(IntMap Definition),
  cineCer0Specs :: !(IntMap CineCer0.Spec),
  cineCer0States :: !(IntMap CineCer0.CineCer0State),
  timeNots :: IntMap JSVal,
  seis8ses :: IntMap Seis8s.Program,
  hydras :: IntMap Hydra.Hydra,
  evaluationTimes :: IntMap UTCTime, -- this is probably temporary
  renderTime :: !MovingAverage,
  wakeTimeAnimation :: !UTCTime,
  animationDelta :: !MovingAverage, -- time between frame starts, ie. 1/FPS
  animationTime :: !MovingAverage, -- time between frame start and end of drawing operations
  zoneRenderTimes :: !(IntMap MovingAverage),
  zoneAnimationTimes :: !(IntMap MovingAverage),
  info :: !RenderInfo,
  glContext :: GLContext,
  canvasElement :: HTMLCanvasElement,
  hydraCanvas :: HTMLCanvasElement,
  locoMotionCanvas :: HTMLCanvasElement,
  videoDivCache :: Maybe HTMLDivElement,
  tempoCache :: Tempo,
  jsoLangs :: Map.Map Text JSoLang,
  valueMap :: Tidal.ValueMap,
  locoMotion :: ExoLang,
  exoLangTest :: ExoLang,
  transMit :: ExoLang,
  punctual :: TextNotationRenderer
  }


initialRenderState :: MusicW.Node -> MusicW.Node -> HTMLCanvasElement -> GLContext -> HTMLCanvasElement -> HTMLCanvasElement -> UTCTime -> AudioTime -> IO RenderState
initialRenderState pIn pOut cvsElement glCtx hCanvas lCanvas t0System t0Audio = do
  punctual' <- newPunctual cvsElement
  lm <- exoLang lCanvas "https://dktr0.github.io/LocoMotion/locoMotion.js"
  elt <- exoLang lCanvas "./exolang.js"
  tm <- exoLang lCanvas "https://jac307.github.io/TransMit/exolang.js"
  return $ RenderState {
    wakeTimeSystem = t0System,
    wakeTimeAudio = t0Audio,
    renderStart = t0System,
    renderPeriod = 0,
    renderEnd = t0System,
    cachedDefs = IntMap.empty,
    paramPatterns = IntMap.empty,
    noteEvents = [],
    webDirtEvents = [],
    baseNotations = IntMap.empty,
    baseDefinitions = IntMap.empty,
    cineCer0Specs = IntMap.empty,
    cineCer0States = IntMap.empty,
    timeNots = IntMap.empty,
    seis8ses = IntMap.empty,
    hydras = IntMap.empty,
    evaluationTimes = IntMap.empty,
    renderTime = newAverage 20,
    wakeTimeAnimation = t0System,
    animationDelta = newAverage 20,
    animationTime = newAverage 20,
    zoneRenderTimes = IntMap.empty,
    zoneAnimationTimes = IntMap.empty,
    info = emptyRenderInfo,
    glContext = glCtx,
    canvasElement = cvsElement,
    hydraCanvas = hCanvas,
    locoMotionCanvas = lCanvas,
    videoDivCache = Nothing,
    tempoCache = Tempo { freq = 0.5, time = t0System, count = 0 },
    jsoLangs = Map.empty,
    valueMap = Map.empty,
    locoMotion = lm,
    exoLangTest = elt,
    transMit = tm,
    punctual = punctual'
  }
