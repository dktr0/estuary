{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Header where

import Reflex
import Reflex.Dom hiding (Request,Response)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Types.Hint
import qualified Estuary.Types.Term as Term
import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Widgets.Reflex

header :: MonadWidget t m => W t m ()
header = divClass "header primary-color primary-borders" $ mdo

  let headerEvent = leftmost [() <$ headerButton]
  headerVisible <- toggle True headerEvent
  headerButton <- clickableDiv "header-area" $ do
    hideableWidget' headerVisible $ do
      divClass "header-title" $ text "estuary"

  hideableWidget' headerVisible $ do
    divClass "config-toolbar" $ do

      divClass "config-entry display-inline-block primary-color ui-font" $ do
        term Term.Theme >>= dynText
        let styleMap = fromList [("../css-custom/classic.css", "Classic"),("../css-custom/dark.css", "Dark" ),("../css-custom/inverse.css","Inverse"),("../css-custom/grayscale.css","Grayscale"),("../css-custom/bubble.css","Bubble"),("../css-custom/minimalist.css","Minimalist")]
        t <- theme
        styleChange <- dropdownW styleMap t
        setTheme styleChange

      divClass "config-entry display-inline-block primary-color ui-font" $ do
        term Term.Language >>= dynText
        let langMap = fromList $ zip languages (fmap (T.pack . show) languages)
        l <- language
        langChange <- dropdownW langMap l
        setLanguage langChange

      sidebarButtonEvent <- divClass "config-entry display-inline-block primary-color ui-font" $ dynButton "?"
      hint $ fmap (const ToggleSidebar) sidebarButtonEvent
