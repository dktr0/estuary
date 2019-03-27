module Estuary.Widgets.EstuaryIcon (
  estuaryIcon
) where

import Control.Monad.IO.Class(liftIO)

import qualified GHCJS.DOM.ChildNode as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Types as Dom
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.DOMTokenList as ClassList

import GHCJS.Types

import GHCJS.Marshal.Pure
import GHCJS.Nullable

import Reflex.Dom

-- m satisfies `DomBuilderSpace m ~ GhcjsDomSpace`, this means that we also
-- have the constraint `DomSpace m` with `type RawElement GhcjsDomSpace = DOM.Element`

-- likewise m satisfies `DomBuilder t m` with `placeRawElement` defaulting to 
-- `placeRawElement` in `GhcjsDomSpace` (`append . toNode`).

estuaryIcon :: (MonadWidget t m) => m ()
estuaryIcon = do
  jsIconInstance <- liftIO $ js_splashIconInstance
  jsCanvas <- liftIO $ (fmap pFromJSVal (js_getCanvas jsIconInstance) :: IO (Dom.Element))
  
  placeRawElement jsCanvas

newtype IconDisplay = IconDisplay JSVal

foreign import javascript safe
  "EstuaryIcon != null ? EstuaryIcon.display : null"
  js_splashIconInstance :: IO IconDisplay

foreign import javascript safe
  "$1.canvas"
  js_getCanvas :: IconDisplay -> IO JSVal