module Estuary.Widgets.WebDirt where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import GHCJS.Types
import Control.Monad
import Control.Monad.IO.Class

webDirtWidget :: MonadWidget t m => JSVal -> m (Dynamic t ())
webDirtWidget webDirt = el "div" $ do
  el "div" $ do
    text "WebDirt: "
    text "more info here soon"
  el "div" $ do
    text "Esp URL: "
    t <- textInput def
    let t' = _textInput_input t
    performEvent_ $ fmap (liftIO . (syncWithEsp webDirt)) t'
  return $ constDyn ()
