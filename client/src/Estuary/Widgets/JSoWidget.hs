{-# LANGUAGE JavascriptFFI, OverloadedStrings #-}
module Estuary.Widgets.JSoWidget where

import Data.Text

newtype JSoWidget = JSoWidget JSVal


-- step 1 (for now) is to create a Haskell representation of a "JSoWidget object"

foreign import javascript safe
  "_aJSoWidget" -- trivial for now, will be replaced w something remote & asynchronous
  load :: IO JSoWidget


-- that JSoWidget object has a "create" method/member
-- (for now) that method just takes one argument which is the parent element
-- in the DOM, to which the newly created view should be added
-- later, there will undoubtedly be further arguments in order to provide
-- a callback that the widget can use to have effects on Estuary via
-- a highly specific protocol.

newtype JSoWidgetView = JSoWidgetView JSVal

foreign import javascript safe
  "$1.create($2)"
  create :: JSoWidget -> HTMLDomElement -> IO JSoWidgetView


-- here is a placeholder connection of the above functionality into a Reflex
-- environment/context:

jSoWidgetView :: MonadWidget t m => JSoWidget -> m ()
jSoWidgetView jsw = do
  x <- ??? -- need to figure out what parent widget is...
  liftIO $ create jsw parent


{-

untested as of yet but here's an initial proof-of-concept example of what
a JSoWidget might look like:

{
  create: function (parent) {
    var newDomElement = document.createElement("button");
    parent.appendChild()
    }
}
-}
