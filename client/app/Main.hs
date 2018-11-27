{-# LANGUAGE JavaScriptFFI #-}

module Main where

import Reflex.Dom
import Data.List(intercalate)
import Data.Time
import Control.Concurrent.MVar
import Control.Exception

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

import GHCJS.Prim(toJSString)
import GHCJS.Types(JSVal)

main :: IO ()
main = handle visuallyCrash $ do
  warnBeforeGoingBackInBrowser
  now <- Data.Time.getCurrentTime
  wd <- newWebDirt
  sd <- newSuperDirt
  protocol <- estuaryProtocol
  let ic = initialContext now wd sd
  c <- newMVar $ ic
  ri <- newMVar $ emptyRenderInfo
  forkRenderThread c ri
  mainWidget $ estuaryWidget Splash c ri protocol

visuallyCrash :: SomeException -> IO ()
visuallyCrash e = 
  let lines = [
          "Unhandled exception: ",
          displayException e,
          "Click 'OK' to reload the page or 'Cancel' to remain on the page which will be unresponsive."
        ]
  in js_confirmReload $ toJSString $ intercalate "\n" lines

foreign import javascript unsafe
  "if (window.confirm($1)) window.location.reload();"
  js_confirmReload :: JSVal -> IO ()

foreign import javascript safe
  "window.addEventListener('beforeunload', function (e) { e.preventDefault(); e.returnValue = ''; });"
  warnBeforeGoingBackInBrowser :: IO ()
