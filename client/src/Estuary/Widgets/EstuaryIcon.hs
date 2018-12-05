module Estuary.Widgets.EstuaryIcon where

import Control.Monad.IO.Class(liftIO)

import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Types as Dom
import GHCJS.Types

import GHCJS.Marshal.Pure
import GHCJS.Nullable

import Reflex.Dom

-- Same as how reflex creates an element but take don't create the node
estuaryIcon :: (MonadWidget t m) => m ()
estuaryIcon = do
  mountPoint <- askParent -- get the container dom node of this widget

  jsIconInstance <- liftIO js_splashIconInstance
  let mIconInstance :: Maybe (Dom.HTMLCanvasElement)
      mIconInstance = pFromJSVal jsIconInstance

  Dom.appendChild mountPoint mIconInstance

  return ()

foreign import javascript safe
  "EstuaryIcon != null ? EstuaryIcon.display : null"
  js_splashIconInstance :: IO JSVal