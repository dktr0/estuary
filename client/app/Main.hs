{-# LANGUAGE JavaScriptFFI #-}

module Main where

import Reflex.Dom
import Data.Time
import Control.Concurrent.MVar

import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Protocol.Foreign
import Estuary.Types.Context
import Estuary.Widgets.Estuary
import Estuary.Widgets.Navigation(Navigation(..))
import Estuary.WebDirt.SampleEngine
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Renderer

main :: IO ()
main = do
  warnBeforeGoingBackInBrowser
  now <- Data.Time.getCurrentTime
  wd <- newWebDirt
  sd <- newSuperDirt
  protocol <- estuaryProtocol
  mv <- newMVar []
  let ic = initialContext now wd sd mv
  c <- newMVar $ ic
  ri <- newMVar $ emptyRenderInfo
  forkRenderThread c ri
  mainWidget $ estuaryWidget Splash c ri protocol

foreign import javascript safe
  "window.addEventListener('beforeunload', function (e) { e.preventDefault(); e.returnValue = ''; });"
  warnBeforeGoingBackInBrowser :: IO ()
