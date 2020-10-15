{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

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
import Estuary.Widgets.Editor

viewParseResultToText :: Either ParseError View -> Text
viewParseResultToText v = do
  case v of
    Left errMsg -> "Error"
    Right view -> ""

viewToContextChange :: Either ParseError View -> Maybe ContextChange
viewToContextChange v = do
  case v of
    Left _ -> Nothing
    Right view -> Just $ modifyEnsembleC $ selectLocalView view

buildError :: MonadWidget t m => Text -> Editor t m ()
buildError x = if x == "" then return () else syntaxErrorWidget x

viewEditor :: MonadWidget t m => Dynamic t Context -> Editor t m (Event t ContextChange)
viewEditor ctx = mdo

  -- let viewList = fromList [(1, "default"), (2, "cybernetic"), (3, "blackbox"), (4, "memorias")]
  -- viewChange <- _dropdown_change <$> dropdown 1.0 (constDyn viewList) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
  runViewButton <- divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Run"

  let y = fmap buildError viewParseResult
  widgetHold (return ()) y

  currentView <- holdUniqDyn $ fmap (activeView . ensembleC) ctx
  let incomingView = updated $ fmap dumpView currentView
  initialView <- sample $ current currentView
  let initialText = dumpView initialView
  (textValue,_,shiftEnter) <- textWidget 5 initialText incomingView
  let runViewEvent = leftmost [runViewButton, shiftEnter]
  let evaledText = tag (current textValue) runViewEvent
  let parsedInput = fmap (parse viewParser "") evaledText
  let viewParseResult = fmap viewParseResultToText parsedInput
  let viewCtxChange = fmapMaybe viewToContextChange parsedInput

  -- text "View name: "
  -- let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
  -- liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  -- buttonWithClass "Publish"

  return viewCtxChange
