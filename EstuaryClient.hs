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
import Estuary.WebDirt.SampleEngine
import Estuary.RenderState
import Estuary.Renderer

main :: IO ()
main = do
  warnBeforeGoingBackInBrowser
  now <- Data.Time.getCurrentTime
  wd <- newWebDirt
  sd <- newSuperDirt
  protocol <- estuaryProtocol
  let ic = initialContext now wd sd
  c <- newMVar $ ic
  s <- newMVar $ initialRenderState now
  forkRenderThread c s
  mainWidget $ estuaryWidget c s protocol ic

foreign import javascript safe
  "window.addEventListener('beforeunload', function (e) { e.preventDefault(); e.returnValue = ''; });"
  warnBeforeGoingBackInBrowser :: IO ()
