{-# LANGUAGE RecursiveDo #-}
module Estuary.Test.Estuary where

import Reflex.Dom
import Data.Time

import Control.Concurrent.MVar
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt

import Estuary.Protocol.Foreign

import Estuary.WebDirt.SampleEngine
import Estuary.Widgets.Estuary
import Estuary.Widgets.Terminal
import Estuary.Widgets.Navigation
import Estuary.Widgets.WebSocket

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Context
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Terminal
import Estuary.Types.Tempo

import Estuary.Reflex.Router
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Renderer

import Estuary.Test.Reflex

silentEstuaryWithInitialPage :: EstuaryProtocolObject -> Navigation -> IO ()
silentEstuaryWithInitialPage protocol pageId = do
  initialCtx <- initCtx
  ctxVar <- newMVar $ initialCtx

  renderInfoVar <- newMVar $ emptyRenderInfo
  forkRenderThread ctxVar renderInfoVar

  renderSync_ $
    estuaryWidget pageId ctxVar renderInfoVar protocol

silentEstuary :: EstuaryProtocolObject -> IO ()
silentEstuary protocol = silentEstuaryWithInitialPage protocol Splash

initCtx :: IO Context
initCtx = do
  now <- Data.Time.getCurrentTime
  let ctx = initialContext now js_nullWebDirt js_nullSuperDirt
  return $ ctx {
    webDirtOn = False
  }

foreign import javascript safe
  "null"
  js_nullWebDirt :: WebDirt

foreign import javascript safe
  "null"
  js_nullSuperDirt :: SuperDirt
