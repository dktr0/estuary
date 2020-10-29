module Estuary.Render.RenderEnvironment where

import GHCJS.DOM.Types hiding (Node)
import Sound.MusicW (Node)

import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Render.RenderInfo
import Estuary.Types.AudioMap

data RenderEnvironment = RenderEnvironment {
  mainBus :: (Node,Node,Node,Node,Node), -- ^ main bus input, delay, pregain, compressor, postgain
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  mic :: Node,
  videoDivElement :: Maybe HTMLDivElement,
  theVideoDiv :: Maybe JSVal,
  audioMap :: AudioMap,
  ensembleEventsM :: MVar [EnsembleEvent],
  settings :: MVar Settings,
  renderInfo :: MVar RenderInfo
  }

out :: RenderEnvironment -> Node
out (RenderEnvironment (_,_,_,x,_) _ _ _ _ _ _ _ _) = x
