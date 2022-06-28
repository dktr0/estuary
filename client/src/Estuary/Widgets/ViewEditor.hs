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
import Estuary.Types.View.Parser
import Language.Haskellish as LH
import qualified Language.Haskell.Exts as Exts

import Estuary.Types.View
import Estuary.Widgets.Text
import Estuary.Widgets.W


viewEditor :: MonadWidget t m => W t m ()
viewEditor = mdo

  runViewButton <- divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Run"

  widgetHold (return ()) $ fmap buildError viewParseResult

  currentView <- activeView
  let incomingView = updated $ fmap dumpView currentView
  initialView <- sample $ current currentView
  let initialText = dumpView initialView
  (textValue,_,shiftEnter) <- textWidget 5 (constDyn False) initialText incomingView
  let runViewEvent = leftmost [runViewButton, shiftEnter]
  let evaledText = tag (current textValue) runViewEvent
  let parsedInput = fmap parseViewParser evaledText -- Event t (Either String View)
  let viewParseResult = fmap viewParseResultToText parsedInput

  -- WORKING HERE: need to map parsedInput from Event t Either... to Event t Maybe... and use fmapMaybe
  -- setLocalView $ fmapMaybe ...

  pure ()


viewParseResultToText :: Either String View -> Text
viewParseResultToText v = do
  case v of
    Left errMsg -> "Error"
    Right view -> ""


buildError :: MonadWidget t m => Text -> W t m ()
buildError x = if x == "" then return () else syntaxErrorWidget x


parseViewParser :: T.Text -> Either String View
parseViewParser s = (f . Exts.parseExp) $ T.unpack s
    where
      f (Exts.ParseOk x) = fmap fst $ runHaskellish viewParser () x -- Either String a
      f (Exts.ParseFailed l s) = Left s
