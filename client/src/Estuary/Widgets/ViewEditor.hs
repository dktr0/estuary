{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.ViewEditor where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Data.Map.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad(liftM)
import Text.Parsec

import Estuary.Widgets.Reflex
import Estuary.Types.EnsembleC
import Estuary.Types.View.Parser
import Language.Haskellish as LH
import qualified Language.Haskell.Exts as Exts


import Estuary.Types.View
import Estuary.Widgets.Text
import Estuary.Widgets.W

-- viewParseResultToText :: Either String ParseError -> Text
viewParseResultToText :: Either String View -> Text
viewParseResultToText v = do
  case v of
    Left errMsg -> "Error"
    Right view -> ""

-- viewToContextChange :: Either String ParseError -> Maybe ContextChange
viewToContextChange :: Either String View -> Maybe ContextChange
viewToContextChange v = do
  case v of
    Left _ -> Nothing
    Right view -> Just $ modifyEnsembleC $ selectLocalView view

buildError :: MonadWidget t m => Text -> W t m ()
buildError x = if x == "" then return () else syntaxErrorWidget x

viewEditor :: MonadWidget t m => W t m (Event t ContextChange)
viewEditor = mdo
  ctx <- context

  -- let viewList = fromList [(1, "default"), (2, "cybernetic"), (3, "blackbox"), (4, "memorias")]
  -- viewChange <- _dropdown_change <$> dropdown 1.0 (constDyn viewList) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
  runViewButton <- divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Run"

  let y = fmap buildError viewParseResult
  widgetHold (return ()) y

  currentView <- holdUniqDyn $ fmap (activeView . ensembleC) ctx
  let incomingView = updated $ fmap dumpView currentView
  initialView <- sample $ current currentView
  let initialText = dumpView initialView
  (textValue,_,shiftEnter) <- textWidget 5 (constDyn False) initialText incomingView
  let runViewEvent = leftmost [runViewButton, shiftEnter]
  let evaledText = tag (current textValue) runViewEvent
  let parsedInput = fmap parseViewParser evaledText -- fmap (parse viewParser "") evaledText
  let viewParseResult = fmap viewParseResultToText parsedInput
  let viewCtxChange = fmapMaybe viewToContextChange parsedInput

  -- text "View name: "
  -- let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
  -- liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  -- buttonWithClass "Publish"

  return viewCtxChange


parseViewParser :: T.Text -> Either String View -- Text -> Either ParseError Command
parseViewParser s = (f . Exts.parseExp) $ T.unpack s
    where
      f (Exts.ParseOk x) = fmap fst $ runHaskellish viewParser () x -- Either String a
      f (Exts.ParseFailed l s) = Left s
