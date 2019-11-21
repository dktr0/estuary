{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.NoHelpFile where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
noHelpFile :: MonadWidget t m => m ()
noHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
    about
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about primary-color code-font" $ text "No help file available"
