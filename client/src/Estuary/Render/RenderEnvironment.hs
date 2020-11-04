module Estuary.Render.RenderEnvironment where

import GHCJS.DOM.Types hiding (Node)
import Sound.MusicW (Node)
import Data.Map as Map

import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Render.RenderInfo
import Estuary.Types.ResourceMap
import Estuary.Render.DynamicsMode

data RenderEnvironment = RenderEnvironment {
  mainBus :: (Node,Node,Node,Node,Node), -- ^ main bus input, delay, pregain, compressor, postgain
  mic :: Node,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  cineCer0Div :: HTMLDivElement,
  punctualCanvas :: HTMLCanvasElement,
  hydraCanvas :: HTMLCanvasElement,
  audioMap :: MVar AudioMap,
  ensembleEventsM :: MVar [EnsembleEvent],
  settings :: MVar Settings,
  renderInfo :: MVar RenderInfo
  }

out :: RenderEnvironment -> Node
out (RenderEnvironment (_,_,_,x,_) _ _ _ _ _ _ _ _) = x

newRenderEnvironment :: HTMLDivElement -> HTMLCanvasElement -> HTMLCanvasElement -> IO RenderEnvironment
newRenderEnvironment c0div pCanvas hCanvas = do
  aCtx <- getGlobalAudioContextPlayback
  addWorklets aCtx
  mainBus'@(mainBusIn,_,_,_,_) <- initializeMainBus
  mic' <- liftAudioIO createMicrophone
  webDirt' <- liftAudioIO $ newWebDirt mainBusIn
  initializeWebAudio webDirt'
  superDirt' <- newSuperDirt
  audioMap' <- newMVar Map.empty
  ensembleEventsM' <- newMVar []
  settings' <- newMVar defaultSettings
  renderInfo' <- newMVar emptyRenderInfo
  return $ RenderEnvironment {
    mainBus = mainBus', -- :: (Node,Node,Node,Node,Node), -- ^ main bus input, delay, pregain, compressor, postgain
    mic = mic', -- :: Node,
    webDirt = webDirt', -- :: WebDirt,
    superDirt = superDirt', -- :: SuperDirt,
    cineCer0Div = c0div,
    punctualCanvas = pCanvas,
    hydraCanvas = hCanvas,
    audioMap = audioMap',
    ensembleEventsM = ensembleEventsM',
    settings = settings',
    renderInfo = renderInfo'
  }
