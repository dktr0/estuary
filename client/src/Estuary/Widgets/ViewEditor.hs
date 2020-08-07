{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.ViewEditor where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Data.Map.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad(liftM)

import Estuary.Reflex.Utility

textBox :: MonadWidget t m =>  Int -> Text -> m (Dynamic t Text, Event t Text)
textBox rows i = do
  let attrs = case rows of 0 -> constDyn $ ("class" =: "textInputToEndOfLine  primary-color code-font")
                           _ -> constDyn $ ("class" =: "textInputToEndOfLine  primary-color code-font" <> "rows" =: T.pack (show rows) <> "style" =: "height: auto")
  x <- textArea $ def & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits)

viewEditor :: MonadWidget t m => m (Event t ())
viewEditor = do
  let viewList = fromList [(1, "default"), (2, "cybernetic"), (3, "blackbox"), (4, "memorias")]
  viewChange <- _dropdown_change <$> dropdown 1.0 (constDyn viewList) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
  divClass "config-entry display-inline-block primary-color ui-font" $ buttonWithClass "Run"
  _ <- textBox 5 ""
  text "View name: "
  let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
  liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs
  buttonWithClass "Publish"
