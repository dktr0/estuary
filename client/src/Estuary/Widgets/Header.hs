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
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor
import Estuary.Widgets.Generic

header :: MonadWidget t m => Editor t m (Event t ContextChange)
header = divClass "header primary-color primary-borders" $ mdo

  let headerEvent = leftmost [() <$ headerButton]
  headerVisible <- toggle True headerEvent
  headerButton <- clickableDiv "header-area" $ do
    hideableWidget' headerVisible $ do
      divClass "header-title" $ text "estuary"

  hideableWidget' headerVisible $ do
    divClass "config-toolbar" $ do

      themeChangeEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
        let styleMap =  fromList [("../css-custom/classic.css", "Classic"),("../css-custom/dark.css", "Dark" ),("../css-custom/inverse.css","Inverse"), ("../css-custom/grayscale.css","Grayscale"), ("../css-custom/bubble.css","Bubble"), ("../css-custom/minimalist.css","Minimalist")]
        term Term.Theme >>= dynText
        styleChange <- _dropdown_change <$> dropdown "../css-custom/classic.css" (constDyn styleMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" )) -- Event t String
        return $ fmap (\x c -> c {theme = x}) styleChange -- Event t (Context -> Context)

      langChangeEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
        term Term.Language >>= dynText
        let langMap = fromList $ zip languages (fmap (T.pack . show) languages)
        langChange <- _dropdown_change <$> dropdown English (constDyn langMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
        return $ fmap (\x c -> c { language = x }) langChange

      sidebarButtonEvent <- divClass "config-entry display-inline-block primary-color ui-font" $ dynButton "?"
      hint $ fmap (const ToggleSidebar) sidebarButtonEvent
      return $ mergeWith (.) [themeChangeEv, langChangeEv]
