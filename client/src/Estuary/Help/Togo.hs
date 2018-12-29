{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Togo where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
togoHelpFile :: MonadWidget t m => m ()
togoHelpFile = divClass "languageHelp" $ do
  about
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about" $ text "Togo"
 divClass "aboutText" $ text "A mini live coding esolang developed in Hamilton, Canada by David Ogborn and others."
