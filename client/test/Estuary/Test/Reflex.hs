{-# LANGUAGE TypeFamilies, RankNTypes, OverloadedStrings #-}
module Estuary.Test.Reflex where

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Map

import Estuary.Test.Dom (findMatchingSelectorInDocument)

import GHCJS.DOM.Types (Element, HTMLDivElement, fromJSString)

import Reflex.Dom hiding (link)
import Reflex.Dynamic

-- renderSync renders the widget, waits for it to mount, and returns the container
-- element it was mounted in.
renderSync :: (forall x. Widget x ()) -> IO (HTMLDivElement)
renderSync widget = do
  resultContainer <- newEmptyMVar
  finishedRender <- async $ takeMVar resultContainer
  link finishedRender -- any exceptions should be thrown here

  mainWidgetInElementById "estuary-root" $ do
    widget

    (Just containerEl) <- liftIO $ findMatchingSelectorInDocument ("#estuary-root" :: String)
    postBuildEv <- getPostBuild
    performEvent_ $ ffor postBuildEv $ \_ -> liftIO $
      putMVar resultContainer containerEl

  wait finishedRender

-- renderSync_ renders the widget and waits for it to mount.
renderSync_ :: (forall x. Widget x ()) -> IO ()
renderSync_ widget = do
  resultContainer <- newEmptyMVar
  finishedRender <- async $ takeMVar resultContainer
  link finishedRender -- any exceptions should be thrown here

  mainWidgetInElementById "estuary-root" $ do
    widget

    postBuildEv <- getPostBuild
    performEvent_ $ ffor postBuildEv $ \_ -> liftIO $ do
      putMVar resultContainer ()

  wait finishedRender
