{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Header where

import Reflex
import Reflex.Dom hiding (Request,Response)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map

import Estuary.Types.Language
import Estuary.Types.Hint
import qualified Estuary.Types.Term as Term
import Estuary.Reflex.Utility
import Estuary.Widgets.W
import Estuary.Widgets.Generic

styleMap :: Map Text Text
styleMap = fromList [
  ("../css-custom/classic.css", "Classic"),
  ("../css-custom/dark.css", "Dark" ),
  ("../css-custom/inverse.css","Inverse"),
  ("../css-custom/grayscale.css","Grayscale"),
  ("../css-custom/bubble.css","Bubble"),
  ("../css-custom/memorias.css","Memorias")
  ]

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
        term Term.Theme
        styleChange <- _dropdown_change <$> dropdown "../css-custom/classic.css" (constDyn styleMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" )) -- Event t String
        changeSettings $ fmap (\x s -> s { theme=x }) styleChange

      divClass "config-entry display-inline-block primary-color ui-font" $ do
        term Term.Language
        let langMap = fromList $ zip languages (fmap (T.pack . show) languages)
        langChange <- _dropdown_change <$> dropdown English (constDyn langMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
        changeSettings $ fmap (\x s -> c { language=x }) langChange

      sidebarButtonEvent <- divClass "config-entry display-inline-block primary-color ui-font" $ dynButton "?"
      hint (ToggleSideBar <$ sideBarButtonEvent)
