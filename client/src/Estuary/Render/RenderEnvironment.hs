{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.RenderEnvironment where

import Data.IORef
import Data.Map as Map
import Data.Text
import Control.Concurrent.MVar
import Sound.MusicW
import Data.Time (getCurrentTime)
import GHCJS.DOM.Types (HTMLCanvasElement)
import Control.Monad.IO.Class

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
import Estuary.Types.Definition (Definition(..))
import Estuary.Types.TextNotation
import Estuary.Types.AsyncValue (nonBlocking)

data RenderEnvironment = RenderEnvironment {
  mainBus :: MainBus,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  webSerial :: WebSerial.WebSerial,
  resources :: Resources,
  ccMap :: IORef (Map.Map Text Double),
  _settings :: IORef Settings,
  renderOps :: MVar [RenderOp], -- todo: could this be IORef?
  renderInfo :: MVar RenderInfo, -- todo: this could definitely be IORef
  allRenderers :: IORef (Map.Map Text Renderer),
  sharedCanvas :: HTMLCanvasElement -- this will be replaced by a sharedDiv very soon!
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
  renderInfo' <- newMVar emptyRenderInfo
  allRenderers' <- newIORef Map.empty
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
    sharedCanvas = cvs
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

