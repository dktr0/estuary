{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Main where

import Reflex.Dom
import Data.List(intercalate)
import Data.Time
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad(liftM)

import Sound.MusicW

import Estuary.Render.AudioContext
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Protocol.Foreign
import Estuary.Types.Context
import Estuary.Types.CanvasState
import Estuary.Widgets.Estuary
import Estuary.Widgets.Navigation(Navigation(..))
import Estuary.WebDirt.SampleEngine
import Estuary.RenderInfo
import Estuary.RenderState
import Estuary.Renderer
import Estuary.Render.DynamicsMode

import GHC.Conc.Sync(setUncaughtExceptionHandler, getUncaughtExceptionHandler)

import GHCJS.DOM
import GHCJS.DOM.Types hiding (toJSString)
import GHCJS.Marshal.Pure
import GHCJS.Prim(toJSString)
import GHCJS.Types

import Reflex.Host.Class (HostFrame)

import System.Timeout(timeout)

main :: IO ()
main = do
  warnBeforeGoingBackInBrowser
  existingUncaughtHandler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler $ \e -> do
    existingUncaughtHandler e
    visuallyCrash e

  js_signalEstuaryLoaded
  -- Wait for 10k ms or click, which ever happens first
  waitForInteractionOrTimeout 10000

  mainBusNodes@(mainBusIn,_,_,_) <- initializeMainBus
  wd <- liftAudioIO $ newWebDirt mainBusIn
  initializeWebAudio wd
  sd <- newSuperDirt
  protocol <- estuaryProtocol
  mv <- emptyCanvasState >>= newMVar
  now <- liftAudioIO $ audioUTCTime
  c <- newMVar $ initialContext now mainBusNodes wd sd mv
  ri <- newMVar $ emptyRenderInfo
  forkRenderThread c ri

  -- root <- fmap pFromJSVal js_estuaryMountPoint :: IO HTMLDivElement
  -- mainWidgetAtRoot root $ estuaryWidget Splash c ri protocol
  mainWidgetInElementById "estuary-root" $ estuaryWidget Splash c ri protocol

visuallyCrash :: SomeException -> IO ()
visuallyCrash e =
  let lines = [
          "Unhandled exception: ",
          displayException e,
          "Click 'OK' to reload the page or 'Cancel' to remain on the page which will be unresponsive."
        ]
  in js_confirmReload $ toJSString $ intercalate "\n" lines

waitForInteractionOrTimeout :: Int -> IO ()
waitForInteractionOrTimeout ms = do
  timeout (ms * 1000) js_waitForClickBody
  return ()

{- disactivated temporarily in update to new reflex
mainWidgetAtRoot :: (IsHTMLElement e) => e -> Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
mainWidgetAtRoot root widget = runWebGUI $ \webView -> do
  Just doc <- liftM (fmap castToHTMLDocument) $ webViewGetDomDocument webView
  attachWidget root webView widget -}

foreign import javascript unsafe
  "if (window.confirm($1)) {        \
  \  window.___forcedReload = true; \
  \  window.location.reload();      \
  \}"
  js_confirmReload :: JSVal -> IO ()

foreign import javascript interruptible
  "document.body.addEventListener('click', $c, {once: true});"
  js_waitForClickBody :: IO ()

foreign import javascript safe
  "window.addEventListener('beforeunload', function (e) { \
  \  if (!window.___forcedReload) {                       \
  \    e.preventDefault();                                \
  \    e.returnValue = '';                                \
  \  }                                                    \
  \});"
  warnBeforeGoingBackInBrowser :: IO ()

foreign import javascript safe
  "document.querySelector('#estuary-root')"
  js_estuaryMountPoint :: IO JSVal

foreign import javascript safe
  "document.querySelector('#estuary-splash').classList.add('btn')"
  js_signalEstuaryLoaded :: IO ()
