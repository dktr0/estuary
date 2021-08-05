{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Togo where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex

-- import Estuary.Types.Language

--render multiple sub-help files
togoHelpFile :: MonadWidget t m => m ()
togoHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
  about
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about primary-color code-font" $ text "Togo"
 divClass "about primary-color code-font" $ text "A mini live coding esolang developed in Hamilton, Canada by David Ogborn and others."
