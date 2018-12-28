module Estuary.Widgets.EstuaryIcon where

import Control.Monad.IO.Class(liftIO)

import qualified GHCJS.DOM.ChildNode as Dom
import qualified GHCJS.DOM.Node as Dom
import qualified GHCJS.DOM.Types as Dom
import GHCJS.Types

import GHCJS.Marshal.Pure
import GHCJS.Nullable

import Reflex.Dom

estuaryIcon :: MonadWidget t m => m ()
estuaryIcon = return () -- placeholder during LTS-9.21 transition

{- -- Same as how reflex creates an element but take don't create the node
estuaryIcon :: (MonadWidget t m) => m ()
estuaryIcon = do
  postMountEv <- getPostBuild
  mountPoint <- askParent -- get the container dom node of this widget

  performEvent_ $ ffor postMountEv $ \_ -> do

    liftIO $ do
      jsIconInstance <- js_splashIconInstance
      jsContainerEl <- fmap pFromJSVal js_splashContainerEl :: IO (Maybe Dom.ChildNode)

      case jsContainerEl of
        Just screen -> do
          let duration = 3 -- seconds
          js_animateMount jsIconInstance (pToJSVal mountPoint) duration
          Dom.remove screen
        Nothing -> do
          jsCanvas <- js_getCanvas jsIconInstance
          Dom.appendChild mountPoint (pFromJSVal jsCanvas :: Maybe Dom.HTMLCanvasElement)
          return ()

    return () -}

newtype IconDisplay = IconDisplay { jsval :: JSVal }

foreign import javascript safe
  "EstuaryIcon != null ? EstuaryIcon.display : null"
  js_splashIconInstance :: IO IconDisplay

foreign import javascript safe
  "$1.canvas"
  js_getCanvas :: IconDisplay -> IO JSVal

foreign import javascript safe
  "document.querySelector('#estuary-splash')"
  js_splashContainerEl :: IO JSVal

foreign import javascript safe
  "EstuaryIcons.animateTo($1, $2, $3);"
  js_animateMount :: IconDisplay -> JSVal -> Double -> IO ()
