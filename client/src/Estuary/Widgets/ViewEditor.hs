{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.ViewEditor where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Data.Map.Strict

import Estuary.Reflex.Utility

viewEditor :: MonadWidget t m => m (Event t ())
viewEditor = do
  divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Create"
  divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Publish"
  let viewList = fromList [(1, "default"), (2, "cybernetic"), (3, "blackbox"), (4, "memorias")]
  viewChange <- _dropdown_change <$> dropdown 1.0 (constDyn viewList) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
  divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Load"
