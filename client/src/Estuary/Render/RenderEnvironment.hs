{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.RenderEnvironment where

import Data.IORef
import Data.Map as Map
import Data.IntMap.Strict as IntMap
import Data.Text
import Control.Concurrent.MVar
import Sound.MusicW
import Data.Time (getCurrentTime)
import GHCJS.DOM.Types (HTMLCanvasElement)
import Control.Monad.IO.Class
import Data.Witherable (catMaybes)
import Data.List (nub)
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual
import qualified Sound.MusicW as MusicW

import Estuary.Render.MainBus
import Estuary.Render.WebDirt as WebDirt
import Estuary.Render.SuperDirt
import Estuary.Render.WebSerial as WebSerial
import Estuary.Render.Renderer
import Estuary.Resources
import Estuary.Client.Settings as Settings
import Estuary.Types.RenderInfo
import Estuary.Render.RenderOp
import Estuary.Types.ResourceOp
import Estuary.Languages.ExoLang
import Estuary.Types.Live
import Estuary.Types.Definition (Definition(..),DefinitionMap)
import Estuary.Types.TextNotation
import Estuary.Types.AsyncValue (nonBlocking)
import Estuary.Types.Tempo
import qualified Estuary.Render.Renderer as Renderer


data RenderEnvironment = RenderEnvironment {
  mainBus :: MainBus,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  webSerial :: WebSerial.WebSerial,
  resources :: Resources,
  ccMap :: IORef (Map.Map Text Double),
  _settings :: IORef Settings,
  renderOps :: MVar [RenderOp], -- todo: could this be IORef?
  renderInfo :: IORef RenderInfo,
  allRenderers :: IORef (Map.Map Text Renderer),
  sharedCanvas :: HTMLCanvasElement, -- this will be replaced by a sharedDiv very soon!
  baseDefinitions :: IORef DefinitionMap, -- the map of definitions as actually rendered (eg. with jsolang translations)
  activeRenderersMap :: IORef (IntMap Text),
  activeRenderers :: IORef [Renderer]
  }
  
initialRenderEnvironment :: Settings -> HTMLCanvasElement -> IO RenderEnvironment
initialRenderEnvironment s cvs = do
  ac <- getGlobalAudioContextPlayback
  addWorklets ac
  mb <- initializeMainBus
  wdOutput <- getWebDirtOutput mb
  wd <- liftAudioIO $ newWebDirt wdOutput
  initializeWebAudio wd
  sd <- newSuperDirt
  _webSerial <- WebSerial.newWebSerial
  resources' <- newResources
  addResourceOp resources' $ ResourceListURL "samples/resources.json"
  ccMap' <- newIORef Map.empty
  settings' <- newIORef s
  now <- getCurrentTime
  iRenderOps <- case minitidal s of
                     "" -> pure []
                     x -> do
                       putStrLn $ "minitidal: " ++ unpack (minitidal s)
                       pure [WriteZone 1 $ TextProgram $ Live ("MiniTidal",x,now) L3]
  renderOps' <- newMVar iRenderOps
  renderInfo' <- newIORef emptyRenderInfo
  allRenderers' <- newIORef Map.empty
  baseDefinitions' <- newIORef IntMap.empty
  activeRenderersMap' <- newIORef IntMap.empty
  activeRenderers' <- newIORef []
  putStrLn "finished initialRenderEnvironment"
  return $ RenderEnvironment {
    mainBus = mb,
    webDirt = wd,
    superDirt = sd,
    webSerial = _webSerial,
    resources = resources',
    ccMap = ccMap',
    _settings = settings',
    renderOps = renderOps',
    renderInfo = renderInfo',
    allRenderers = allRenderers',
    sharedCanvas = cvs,
    baseDefinitions = baseDefinitions',
    activeRenderersMap = activeRenderersMap',
    activeRenderers = activeRenderers'
    }
    
insertRenderer :: MonadIO m => RenderEnvironment -> Text -> Renderer -> m ()
insertRenderer rEnv name r = liftIO $ modifyIORef (allRenderers rEnv) $ Map.insert name r

-- insertExoLang inserts the reference to the exolang but doesn't force it to load yet
insertExoLang :: MonadIO m => RenderEnvironment -> Text -> Text -> m Renderer
insertExoLang rEnv name url = do
  let name' = toLower name  
  let cvs = sharedCanvas rEnv
  r <- liftIO $ exoLangRenderer name' cvs url
  insertRenderer rEnv name' r
  pure r


modifyRenderInfo :: MonadIO m => RenderEnvironment -> (RenderInfo -> RenderInfo) -> m ()
modifyRenderInfo rEnv f = liftIO $ modifyIORef (renderInfo rEnv) f

setZoneError :: MonadIO m => RenderEnvironment -> Int -> Text -> m ()
setZoneError rEnv z t = modifyRenderInfo rEnv $ \rInfo -> rInfo { errors = IntMap.insert z t (errors rInfo) }

clearZoneError :: MonadIO m => RenderEnvironment -> Int -> m ()
clearZoneError rEnv z = modifyRenderInfo rEnv $ \rInfo -> rInfo { errors = IntMap.delete z (errors rInfo) }

updateWebDirtVoices :: MonadIO m => RenderEnvironment -> m ()
updateWebDirtVoices rEnv = do
  n <- liftIO $ WebDirt.voices (webDirt rEnv)
  modifyRenderInfo rEnv $ \rInfo -> rInfo { webDirtVoices = n }

setBaseDefinition :: MonadIO m => RenderEnvironment -> Int -> Definition -> m ()
setBaseDefinition rEnv z x = liftIO $ modifyIORef (baseDefinitions rEnv) $ IntMap.insert z x

clearBaseDefinition :: MonadIO m => RenderEnvironment -> Int -> m ()
clearBaseDefinition rEnv z = liftIO $ modifyIORef (baseDefinitions rEnv) $ IntMap.delete z

getBaseDefinition :: MonadIO m => RenderEnvironment -> Int -> m (Maybe Definition)
getBaseDefinition rEnv z = IntMap.lookup z <$> getBaseDefinitions rEnv

getBaseDefinitions :: MonadIO m => RenderEnvironment -> m DefinitionMap
getBaseDefinitions rEnv = liftIO $ readIORef (baseDefinitions rEnv)

setBaseRenderer :: MonadIO m => RenderEnvironment -> Int -> Text -> m ()
setBaseRenderer rEnv z name = liftIO $ modifyIORef (activeRenderersMap rEnv) $ IntMap.insert z name

clearBaseRenderer :: MonadIO m => RenderEnvironment -> Int -> m ()
clearBaseRenderer rEnv z = liftIO $ modifyIORef (activeRenderersMap rEnv) $ IntMap.delete z

updateActiveRenderers :: MonadIO m => RenderEnvironment -> m ()
updateActiveRenderers rEnv = liftIO $ do
  rs <- readIORef (allRenderers rEnv)
  arm <- readIORef (activeRenderersMap rEnv)
  writeIORef (activeRenderers rEnv) $ catMaybes $ fmap (\n -> Map.lookup n rs) $ nub $ IntMap.elems arm

getActiveRenderers :: MonadIO m => RenderEnvironment -> m [Renderer]
getActiveRenderers rEnv = liftIO $ readIORef (activeRenderers rEnv)

getActiveRenderer :: MonadIO m => RenderEnvironment -> Int -> m (Maybe Renderer)
getActiveRenderer rEnv z = liftIO $ do
  brs <- readIORef (activeRenderersMap rEnv)
  case IntMap.lookup z brs of
    Nothing -> pure Nothing
    Just name -> do
      allRs <- readIORef (allRenderers rEnv)
      case Map.lookup name allRs of
        Nothing -> pure Nothing
        Just r ->  pure (Just r)
  
getAllRendererNames :: MonadIO m => RenderEnvironment -> m [TextNotation]
getAllRendererNames rEnv = liftIO $ Map.keys <$> readIORef (allRenderers rEnv)


-- functions to call setters on all cached renderers 
-- for example, when those settings are changed in the client environment
-- TODO: double-check/confirm/reconsider - this should only be when a renderer is active
-- (when inactive or not yet active renderers are (re)activated, they should get all setters then

setTempo :: MonadIO m => RenderEnvironment -> Tempo -> m ()
setTempo rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setTempo $ x)

setBrightness :: MonadIO m => RenderEnvironment -> Double -> m ()
setBrightness rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setBrightness $ x)

setResolution :: MonadIO m => RenderEnvironment -> Punctual.Resolution -> m ()
setResolution rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setResolution $ x)

setValueMap :: MonadIO m => RenderEnvironment -> Tidal.ValueMap -> m ()
setValueMap rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setValueMap $ x)

setAudioInput :: MonadIO m => RenderEnvironment -> IO MusicW.Node -> m ()
setAudioInput rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setAudioInput $ x)

setAudioOutput :: MonadIO m => RenderEnvironment -> MusicW.Node -> m ()
setAudioOutput rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setAudioOutput $ x)

setNchnls :: MonadIO m => RenderEnvironment -> Int -> m ()
setNchnls rEnv x = liftIO $ readIORef (allRenderers rEnv) >>= mapM_ (flip Renderer.setNchnls $ x)

-- a function to call setters with initial values when a new renderer is inserted into the render environment
-- CONTINUE HERE --
{- initializeRenderer :: MonadIO m => RenderEnvironment -> Renderer -> m () -}
