 {-# LANGUAGE JavaScriptFFI, OverloadedStrings, RecursiveDo #-}

module Main where

import Reflex.Dom
import Data.List(intercalate)
import Data.Time
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad(liftM)
import Control.Monad.IO.Class(liftIO)
import System.IO
import Sound.MusicW
import Data.Text (Text)
import qualified Data.Text as T


import Estuary.Protocol.Peer
import Estuary.Types.Context
import Estuary.Widgets.Estuary
import Estuary.Widgets.Navigation(Navigation(..))
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Render.Renderer
import Estuary.Render.R
import Estuary.Client.Settings

import GHC.Conc.Sync(setUncaughtExceptionHandler, getUncaughtExceptionHandler)

import GHCJS.DOM
import GHCJS.DOM.Types hiding (toJSString,Text,Event)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import GHCJS.Marshal.Pure
import GHCJS.Prim(toJSString)
import GHCJS.Types

-- just for temporary test
import Data.IntMap as IntMap
import Estuary.Widgets.W hiding (theme)
import Estuary.Widgets.MultilingualEditor


import Reflex.Host.Class (HostFrame)

import System.Timeout(timeout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  warnBeforeGoingBackInBrowser
  existingUncaughtHandler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler $ \e -> do
    existingUncaughtHandler e
    visuallyCrash e

  nowUtc <- getCurrentTime
  context <- newMVar $ initialContext nowUtc
  renderInfo <- newMVar $ emptyRenderInfo

  settings <- getSettingsFromURI
  setThemeIO $ theme settings
  rEnv <- initialRenderEnvironment settings
  mainWidgetInElementById "estuary-root" $ keyboardHintsCatcher rEnv settings context renderInfo

  -- Signal the splash page that estuary is loaded.
  -- js_setIconStateLoaded

  -- Resume the audio context after interaction.
  js_waitForClickBody
  mErr <- liftAudioIO $ do
    ac <- audioContext
    liftIO $ resumeSync ac
  case mErr of
    Just err -> putStrLn $ show err
    Nothing -> return ()

visuallyCrash :: SomeException -> IO ()
visuallyCrash e =
  let lines = [
          "Unhandled exception: ",
          displayException e,
          "Click 'OK' to reload the page or 'Cancel' to remain on the page which will be unresponsive."
        ]
  in js_confirmReload $ toJSString $ intercalate "\n" lines

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
  "EstuaryIcon.state = 'loaded';"
  js_setIconStateLoaded :: IO ()

foreign import javascript unsafe
  "window.___setEstuaryAudioDestination = $1"
  js_registerSetEstuaryAudioDestination :: Callback (JSVal -> IO JSVal) -> IO ()
