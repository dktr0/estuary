{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.ViewEditor where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Data.Map.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad(liftM)
import Text.Parsec

import Estuary.Reflex.Utility
import Estuary.Types.Context
import Estuary.Types.EnsembleC
import Estuary.Types.View.Parser
import Estuary.Types.View
import Estuary.Widgets.Text

viewParseResultToText :: Either ParseError View -> Text
viewParseResultToText v = do
  case v of
    Left errMsg -> "Error"
    Right view -> "OK"

-- viewToContextChange :: Either ParseError View -> Maybe ContextChange
-- viewToContextChange v c = do
--   x = x { field = ... }

viewEditor :: MonadWidget t m => Dynamic t Context -> m (Event t ())
viewEditor ctx = do
  
  let viewList = fromList [(1, "default"), (2, "cybernetic"), (3, "blackbox"), (4, "memorias")]
  viewChange <- _dropdown_change <$> dropdown 1.0 (constDyn viewList) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
  runViewButton <- divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Run"

  currentView <- holdUniqDyn $ fmap (activeView . ensembleC) ctx
  let incomingView = updated $ fmap dumpView currentView
  initialView <- sample $ current currentView
  let initialText = dumpView initialView
  (textValue,_,shiftEnter) <- textWidget 0 initialText incomingView
  let runViewEvent = leftmost [runViewButton, shiftEnter]
  let evaledText = tag (current textValue) runViewEvent
  let parsedInput = fmap (parse viewParser "") evaledText
  let viewParseResult = fmap viewParseResultToText parsedInput
  -- viewCtxChange <- fmapMaybe viewToContextChange parsedInput
  -- fmap fromRight $ fmapMaybe isRight viewParseResult

  text "View name: "
  let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
  liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  buttonWithClass "Publish"
