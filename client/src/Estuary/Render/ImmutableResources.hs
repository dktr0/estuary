module Estuary.Render.ImmutableResources

import GHCJS.DOM.Types (HTMLCanvasElement,HTMLDivElement)
import Sound.MusicW (Node)

import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt

-- things the render engine needs, but the UI doesn't need, and which never change
data ImmutableResources = ImmutableResources {
  mainBus :: (Node,Node,Node,Node,Node), -- ^ main bus input, delay, pregain, compressor, postgain
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  mic :: Node,
  videoDivElement :: Maybe HTMLDivElement,
  theVideoDiv :: Maybe JSVal
  }

out :: ImmutableRenderContext -> Node
out (ImmutableRenderContext (_,_,_,x,_) _ _ _) = x
