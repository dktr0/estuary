{-# LANGUAGE TypeFamilies #-}
module Estuary.Test.Reflex where

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Map

import GHCJS.DOM.Types (Element, HTMLDivElement, castToHTMLDivElement, fromJSString)

import Reflex.Dom hiding (link)
import Reflex.Dynamic
import Reflex.Host.Class (HostFrame)

-- The type of widget that `mainWidget` can render.
type SpiderM = Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider))

-- renderSync renders the widget, waits for it to mount, and returns the container
-- element it was mounted in.
renderSync :: (MonadWidget t m, m ~ SpiderM) => m a -> IO (HTMLDivElement)
renderSync widget = do
  resultContainer <- newEmptyMVar
  finishedRender <- async $ takeMVar resultContainer
  link finishedRender -- any exceptions should be thrown here

  mainWidget $ do
    (e, _) <- elAttr' "div" (fromList [("id", "test-container")]) widget

    let containerEl = castToHTMLDivElement $ _el_element e
    postBuildEv <- getPostBuild
    performEvent_ $ ffor postBuildEv $ \_ -> liftIO $
      putMVar resultContainer containerEl

  wait finishedRender

-- renderSync_ renders the widget and waits for it to mount.
renderSync_ :: (MonadWidget t m, m ~ SpiderM) => m a -> IO ()
renderSync_ widget = do
  resultContainer <- newEmptyMVar
  finishedRender <- async $ takeMVar resultContainer
  link finishedRender -- any exceptions should be thrown here

  mainWidget $ do
    elAttr' "div" (fromList [("id", "test-container")]) widget

    postBuildEv <- getPostBuild
    performEvent_ $ ffor postBuildEv $ \_ -> liftIO $
      putMVar resultContainer ()

  wait finishedRender

renderSyncWithEvent :: (MonadWidget t m, m ~ SpiderM) => m a -> IO ()
renderSyncWithEvent widget = do
  resultContainer <- newEmptyMVar
  finishedRender <- async $ takeMVar resultContainer
  link finishedRender -- any exceptions should be thrown here

  mainWidget $ do
    elAttr' "div" (fromList [("id", "test-container")]) widget

    postBuildEv <- getPostBuild
    performEvent_ $ ffor postBuildEv $ \_ -> liftIO $
      putMVar resultContainer ()

  wait finishedRender