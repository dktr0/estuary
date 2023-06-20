{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.RenderEnvironment where

import Data.IORef
import Data.Map as Map
import Data.Text
import Control.Concurrent.MVar
import Sound.MusicW
import Data.Time (getCurrentTime)

import Estuary.Render.MainBus
import Estuary.Render.WebDirt as WebDirt
import Estuary.Render.SuperDirt
import Estuary.Render.WebSerial as WebSerial
import Estuary.Resources
import Estuary.Client.Settings as Settings
import Estuary.Types.RenderInfo
import Estuary.Render.RenderOp
import Estuary.Types.ResourceOp
import Estuary.Languages.ExoLang
import Estuary.Types.Live
import Estuary.Types.Definition (Definition(..))
import Estuary.Types.TextNotation (TextNotation(..))
import Estuary.Types.TidalParser (TidalParser(..))

data RenderEnvironment = RenderEnvironment {
  mainBus :: MainBus,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  webSerial :: WebSerial.WebSerial,
  resources :: Resources,
  ccMap :: IORef (Map.Map Text Double),
  _settings :: IORef Settings,
  renderOps :: MVar [RenderOp],
  renderInfo :: MVar RenderInfo
  }

initialRenderEnvironment :: Settings -> IO RenderEnvironment
initialRenderEnvironment s = do
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
                       pure [WriteZone 1 $ TextProgram $ Live (TidalTextNotation MiniTidal,x,now) L3]
  renderOps' <- newMVar iRenderOps
  renderInfo' <- newMVar emptyRenderInfo
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
    renderInfo = renderInfo'
  }
