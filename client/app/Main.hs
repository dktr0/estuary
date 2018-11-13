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
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Renderer

import Estuary.Reflex.POC

main :: IO ()
main = do
  warnBeforeGoingBackInBrowser
  now <- Data.Time.getCurrentTime
  wd <- newWebDirt
  sd <- newSuperDirt
  protocol <- estuaryProtocol
  let ic = initialContext now wd sd
  c <- newMVar $ ic
  ri <- newMVar $ emptyRenderInfo
  forkRenderThread c ri
  --mainWidget $ estuaryWidget c ri protocol
  mainWidget $ do
    dynRouterData <- poc
    dynText dynRouterData

foreign import javascript safe
  "window.addEventListener('beforeunload', function (e) { e.preventDefault(); e.returnValue = ''; });"
  warnBeforeGoingBackInBrowser :: IO ()
