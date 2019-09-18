{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom hiding (getKeyEvent,preventDefault)
import Reflex.Dom.Contrib.KeyEvent
import Control.Monad
import Control.Monad.Trans
import GHCJS.DOM.EventM
import Data.Maybe
import Data.Map (fromList)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Container
import Estuary.Widgets.GeneralPattern
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Utility (lastOrNothing)
import Estuary.Types.Definition
import Estuary.Types.Hint
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Help.LanguageHelp
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Widgets.Editor
import Estuary.Types.Context
import Estuary.Types.Variable

textWidget :: MonadWidget t m => Int -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget rows i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine coding-textarea primary-color code-font" <> "rows" =: T.pack (show rows) <> "style" =: "height: auto")
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let e = _textArea_element x
  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if keyPressWasShiftEnter y then (preventDefault >> return True) else return False
  let evalEvent = fmap (const ()) $ ffilter (==True) e'
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits,evalEvent)
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)


textNotationParsers :: [TextNotation]
textNotationParsers = [Punctual, CineCer0, Escuchar, TimeNot, Ver, Oir] ++ (fmap TidalTextNotation tidalParsers)

textProgramEditor :: MonadWidget t m => Int -> Dynamic t (Maybe Text) -> Dynamic t TextProgram
  -> Editor t m (Variable t TextProgram)
textProgramEditor nRows errorDyn updates = do
  ctx <- askContext
  reflexWidgetToEditor updates $ textProgramWidget ctx errorDyn nRows


textProgramWidget :: forall t m. MonadWidget t m => Dynamic t Context -> Dynamic t (Maybe Text) -> Int
  -> TextProgram -> Event t TextProgram -> m (Event t TextProgram,Event t [Hint])
textProgramWidget ctx e rows i delta = divClass "textPatternChain" $ do -- *** TODO: change css class
  let deltaFuture = fmap forEditing delta
  let parserFuture = fmap fst deltaFuture
  let textFuture = fmap snd deltaFuture

  (d,evalButton,infoButton) <- divClass "fullWidthDiv" $ do
    let initialParser = fst $ forEditing i
    let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers
    d' <- dropdown initialParser parserMap $ ((def :: DropdownConfig t TidalParser) & attributes .~ constDyn ("class" =: "code-font primary-color primary-borders" <> "style" =: "background-color: transparent")) & dropdownConfig_setValue .~ parserFuture
    evalButton' <- divClass "textInputLabel" $ do
      x <- dynButton =<< translateDyn Term.Eval ctx
      e' <- holdUniqDyn e
      dynText =<< (return $ fmap (maybe "" (const "!")) e')
      return x
    infoButton' <- divClass "referenceButton" $ dynButton "?"
    return (d',evalButton',infoButton')

  (edit,eval) <- divClass "labelAndTextPattern" $ do
    let parserValue = _dropdown_value d -- Dynamic t TidalParser
    let parserEvent = _dropdown_change d
    let initialText = snd $ forEditing i
    textVisible <- toggle True infoButton
    helpVisible <- toggle False infoButton
    (textValue,textEvent,shiftEnter) <- hideableWidget textVisible "width-100-percent" $ textWidget rows initialText textFuture
    let languageToDisplayHelp = ( _dropdown_value d)
    hideableWidget helpVisible "width-100-percent" $ languageHelpWidget languageToDisplayHelp
    let v' = (,) <$> parserValue <*> textValue
    let editEvent = tagPromptlyDyn v' $ leftmost [() <$ parserEvent,() <$ textEvent]
    let evalEvent = tagPromptlyDyn v' $ leftmost [evalButton,shiftEnter]
    return (editEvent,evalEvent)
  let deltaPast = fmap forRendering delta
  pastValue <- holdDyn (forRendering i) $ leftmost [deltaPast,eval]
  futureValue <- holdDyn (forEditing i) $ leftmost [deltaFuture,edit]
  let value = f <$> pastValue <*> futureValue
  let deltaUpEdit = tagPromptlyDyn value edit
  let deltaUpEval = tagPromptlyDyn value eval
  let deltaUp = leftmost [deltaUpEdit,deltaUpEval]
  return (deltaUp,never)
  where
    f p x | p == x = Live p L3 -- *** TODO: this looks like it is a general pattern that should be with Live definitions
          | otherwise = Edited p x


labelEditor :: MonadWidget t m => Dynamic t Text -> Editor t m (Variable t Text)
labelEditor delta = do
  let attrs = constDyn $ ("class" =: "name-tag-textarea code-font primary-color")
  y <- liftR $ divClass "textPatternChain" $ divClass "labelWidgetDiv" $ do
    i <- (sample . current) delta
    textInput $ def & textInputConfig_setValue .~ (updated delta) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  return $ Variable (_textInput_value y) (_textInput_input y)
