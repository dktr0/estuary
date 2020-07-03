{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Reference where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

referenceWidget :: MonadWidget t m => m ()
referenceWidget = divClass "reference" $ do
  divClass "reference-title" $ text "estuary"
  divClass "reference-description" $ text "Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser."
  el "h3" $ text "Languages"
  el "ul" $ do
    el "li" (text "MiniTidal")
    el "li" (text "Punctual")
    el "li" (text "CineCer0")
  el "h3" $ text "Using the Terminal"
  el "h3" $ text "Keyboard Shortcuts"
  el "h3" $ text "Tips and Tricks"
  return ()
