{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}
module Estuary.Widgets.JSoWidget where

import Data.Text
import GHCJS.DOM.Types hiding (Event)
import Reflex.Dom hiding (Element)
import Control.Monad.IO.Class

newtype JSoWidgetFactory = JSoWidgetFactory JSVal


-- step 1 (for now) is to create a Haskell representation of a "JSoWidget object"

foreign import javascript safe
  "_aJSoWidget" -- trivial for now, will be replaced w something remote & asynchronous
  load :: IO JSoWidgetFactory


-- that JSoWidget object has a "create" method/member
-- (for now) that method just takes one argument which is the parent element
-- in the DOM, to which the newly created view should be added
-- later, there will undoubtedly be further arguments in order to provide
-- a callback that the widget can use to have effects on Estuary via
-- a highly specific protocol.

newtype JSoWidget = JSoWidget JSVal

foreign import javascript safe
  "_aJSoWidget.create($1)"
  create :: {- JSoWidgetFactory -> -} Element -> IO JSoWidget


-- here is a placeholder connection of the above functionality into a Reflex
-- environment/context:

jSoWidgetView :: MonadWidget t m => {- JSoWidget -> -} m JSoWidget
jSoWidgetView {- jsw -} = do
  (parentElement,_) <- el' "div" $ return ()
  liftIO $ create {- jsw -} (_el_element parentElement)


{-

untested as of yet but here's an initial proof-of-concept example of what
a JSoWidget might look like:

{
  create: function (parent) {
    var x = document.createElement("button");
    parent.appendChild(x);
    return x;
    }
}
-}
