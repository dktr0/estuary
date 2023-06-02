{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.ExoWidget where

import Data.Text
import GHCJS.DOM.Types hiding (Text)
import Data.JSVal.Promise
import Control.Exception.Base (throwIO)

import Estuary.Types.AsyncValue
import Estuary.Types.JSException

{-
export function exoWidget(parentElement,api,args) {
  return new MyWidget(parentElement,api,args);
}

function MyWidget(parentElement,api,args) {
  this.api = api; // store the API used to contact Estuary:
  // keys that are Ints refer to collaborative editing zones, Strings refer to settings, tempo, renderInfo, etc
  // api.set = function(key :: Int or String, value :: Object, renderChange :: Boolean);
  // api.get = function(key :: Int or String);
  // api.subscribe = function(key :: Int or String);
  // api.unsubscribe = function(key :: Int or String);
  // needs some way of signalling an error?

  // probably make a DOM element and add as a child of parentElement...
  // probably set up a bunch of event listeners on that element to capture local editing/interactions...
}

MyWidget.prototype.change = function(key :: Int or String, value :: Object) {
}
-}

foreign import javascript safe
  "$r = importExoLang($1);"
  exoWidgetPromise :: Text -> IO Promise
  
newtype ExoWidgetClass = ExoWidgetClass JSVal

instance PToJSVal ExoWidgetClass where pToJSVal (ExoWidgetClass x) = x

instance PFromJSVal ExoWidgetClass where pFromJSVal = ExoWidgetClass

newtype ExoWidgetObject = ExoWidgetObject JSVal

instance PToJSVal ExoWidgetObject where pToJSVal (ExoWidgetObject x) = x

instance PFromJSVal ExoWidgetObject where pFromJSVal = ExoWidgetObject

type ExoWidget = AsyncValue ExoWidgetClass

  
exoWidget :: Text -> IO ExoWidget
exoWidget path = asyncValue $ do
  p <- exoWidgetPromise path
  r <- await p
  exoWidgetClass <- case r of
    Left j -> throwIO (JSException j)
    Right j -> pure j
  putStrLn $ "loaded exoWidget from " ++ unpack path
  pure $ ExoWidgetClass exoWidgetClass
  
withExoWidget :: ExoWidget -> (ExoWidgetClass -> IO a) -> IO a
withExoWidget x f = blocking x >>= f


-- note: just passing parent argument for now, will add API and args arguments later...
foreign import javascript safe
  "$r = $2.exoWidget($1);"
  _newExoWidget :: HTMLDivElement -> ExoWidgetClass -> IO ExoWidgetObject
  
newExoWidget :: HTMLDivElement -> ExoWidget -> IO ExoWidgetObject
newExoWidget parent x = withExoWidget x $ _newExoWidget parent


