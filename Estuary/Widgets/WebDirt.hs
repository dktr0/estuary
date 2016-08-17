module Estuary.Widgets.WebDirt where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream

webDirtWidget :: MonadWidget t m => T.JSVal -> m (Dynamic t ())
webDirtWidget webDirt = el "div" $ do
  el "div" $ do
    text "WebDirt: "
    text "more info here soon"
  el "div" $ do
    text "Esp URL: "
    t <- liftM _textInput_input textInput def
    performEvent_ $ fmap (liftIO . (syncWithEsp webDirt)) t
