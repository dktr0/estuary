module Estuary.Widgets.EstuaryIcon where

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
  postMountEv <- getPostBuild

  jsIconInstance <- liftIO $ js_splashIconInstance
  jsContainerEl <- liftIO $ (fmap pFromJSVal js_splashScreenContainerEl :: IO (Maybe Dom.Element))
  jsCanvas <- liftIO $ (fmap pFromJSVal (js_getCanvas jsIconInstance) :: IO (Dom.Element))
  
  case jsContainerEl of

    -- Animate, we are transitioning from the splash screen.
    Just screen -> do
      let duration = 3 -- seconds
      startPos <- liftIO $ js_snapshotPosition jsIconInstance
      placeRawElement jsCanvas
      liftIO $ do
        js_animateFrom jsIconInstance startPos duration
        classList <- Element.getClassList screen
        ClassList.add classList ["loaded"]

    -- Don't animate, we are not transitioning from the splash screen.
    Nothing -> placeRawElement jsCanvas

newtype IconDisplay = IconDisplay JSVal
newtype BBox = BBox JSVal

foreign import javascript safe
  "EstuaryIcon != null ? EstuaryIcon.display : null"
  js_splashIconInstance :: IO IconDisplay

foreign import javascript safe
  "$1.canvas"
  js_getCanvas :: IconDisplay -> IO JSVal

foreign import javascript safe
  "document.querySelector('#estuary-splash:not(.loaded)')"
  js_splashScreenContainerEl :: IO JSVal

foreign import javascript safe
  "EstuaryIcons.snapshotPosition($1)"
  js_snapshotPosition :: IconDisplay -> IO BBox

foreign import javascript safe
  "EstuaryIcons.animateFrom($1, $2, $3);"
  js_animateFrom :: IconDisplay -> BBox -> Double -> IO ()