{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.SuperContinent where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
superContinentHelpFile :: MonadWidget t m => m ()
superContinentHelpFile = divClass "languageHelp" $ do
  about
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
 divClass "about primary-color code-font" $ text "SuperContinent"
 divClass "about primary-color code-font" $ text "A mini live coding esolang developed in Hamilton, Canada by David Ogborn and others."
