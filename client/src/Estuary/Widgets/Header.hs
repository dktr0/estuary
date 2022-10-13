{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Header where

import Reflex
import Reflex.Dom hiding (Request,Response)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict

import Estuary.Types.Language
import Estuary.Types.Hint
import qualified Estuary.Types.Term as Term
import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Types.TranslatableText


header :: MonadWidget t m => W t m ()
header = divClass "header primary-color primary-borders" $ mdo

  hv <- headerVisible

  headerButton <- clickableDiv "header-area" $
    hideableWidget' hv $ divClass "header-title" $ text "estuary"
  toggleHeaderVisible headerButton

  hideableWidget' hv $ do
    divClass "config-toolbar" $ do

      divClass "config-entry display-inline-block primary-color ui-font" $ do
        term Term.Theme >>= dynText
        let styleMap = fromList [("../css-custom/classic.css", "classic"),("../css-custom/dark.css", "Dark" ),("../css-custom/inverse.css","Inverse"),("../css-custom/grayscale.css","Grayscale"),("../css-custom/bubble.css","Bubble"),("../css-custom/minimalist.css","Minimalist")]
        theme >>= dropdownW styleMap >>= setTheme

      divClass "config-entry display-inline-block primary-color ui-font" $ do
        term Term.Language >>= dynText
        let langMap = fromList $ zip languages (fmap (T.pack . show) languages)
        language >>= dropdownW langMap >>= setLanguage

      sideBarButton <- divClass "config-entry display-inline-block primary-color ui-font" $ dynButton "?"
      toggleSideBarVisible sideBarButton
