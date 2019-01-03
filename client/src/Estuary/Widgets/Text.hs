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

import Estuary.Types.Context

textWidgetForPatternChain :: MonadWidget t m => String -> Event t String -> m (Dynamic t String, Event t String)
textWidgetForPatternChain i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine")
  x <- textInput $ def & textInputConfig_setValue .~ (fmap T.pack delta) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ (T.pack i)
  let edits = fmap T.unpack $ _textInput_input x
  let value = fmap T.unpack $ _textInput_value x
  return (value,edits)

textAreaWidgetForPatternChain :: MonadWidget t m => Int -> String -> Event t String -> m (Dynamic t String, Event t String,Event t ())
textAreaWidgetForPatternChain rows i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine" <> "rows" =: T.pack (show rows) <> "style" =: "height: auto")
  x <- textArea $ def & textAreaConfig_setValue .~ (fmap T.pack delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ (T.pack i)
  --let keys = _textArea_keypress x
  let e = _textArea_element x
  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if keyPressWasShiftEnter y then (preventDefault >> return True) else return False
  -- let evalEvent = fmap (const ()) $ ffilter (==True) $ fmap keyPressWasShiftEnter e'
  let evalEvent = fmap (const ()) $ ffilter (==True) e'
  let edits = fmap T.unpack $ _textArea_input x
  let value = fmap T.unpack $ _textArea_value x
  return (value,edits,evalEvent)
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)

textNotationParsers :: [TextNotation]
textNotationParsers = [PunctualAudio,PunctualVideo,SuperContinent,SvgOp,CanvasOp] ++ (fmap TidalTextNotation tidalParsers)

textNotationWidget :: forall t m. MonadWidget t m => Dynamic t Context -> Dynamic t (Maybe String) ->
  Int -> Live (TextNotation,String) -> Event t (Live (TextNotation,String)) ->
  m (Dynamic t (Live (TextNotation,String)),Event t (Live (TextNotation,String)),Event t Hint)
textNotationWidget ctx e rows i delta = divClass "textPatternChain" $ do -- *** TODO: change css class
  let deltaFuture = fmap forEditing delta
  let parserFuture = fmap fst deltaFuture
  let textFuture = fmap snd deltaFuture

  (d,evalButton,infoButton) <- divClass "fullWidthDiv" $ do
    let initialParser = fst $ forEditing i
    let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers
    d' <- dropdown initialParser parserMap $ (def :: DropdownConfig t TidalParser) & dropdownConfig_setValue .~ parserFuture
    evalButton' <- divClass "textInputLabel" $ do
      x <- button "eval"
      dynText =<< mapDyn (maybe "" (const "!")) (nubDyn e)
      return x
    infoButton' <- divClass "referenceButton" $ button "?"
    return (d',evalButton',infoButton')

  (edit,eval) <- divClass "labelAndTextPattern" $ do
    let parserValue = _dropdown_value d -- Dynamic t TidalParser
    let parserEvent = _dropdown_change d
    let initialText = snd $ forEditing i
    textVisible <- toggle True infoButton
    helpVisible <- toggle False infoButton
    (textValue,textEvent,shiftEnter) <- hideableWidget textVisible "visibleArea" $ textAreaWidgetForPatternChain rows initialText textFuture
    let languageToDisplayHelp = ( _dropdown_value d)
    hideableWidget helpVisible "visibleArea" $ languageHelpWidget languageToDisplayHelp
    v' <- combineDyn (,) parserValue textValue
    let editEvent = tagDyn v' $ leftmost [() <$ parserEvent,() <$ textEvent]
    let evalEvent = tagDyn v' $ leftmost [evalButton,shiftEnter]
    return (editEvent,evalEvent)
  let deltaPast = fmap forRendering delta
  pastValue <- holdDyn (forRendering i) $ leftmost [deltaPast,eval]
  futureValue <- holdDyn (forEditing i) $ leftmost [deltaFuture,edit]
  value <- combineDyn f pastValue futureValue
  let deltaUpEdit = tagDyn value edit
  let deltaUpEval = tagDyn value eval
  let deltaUp = leftmost [deltaUpEdit,deltaUpEval]
  return (value,deltaUp,never)
  where
    f p x | p == x = Live p L3 -- *** TODO: this looks like it is a general pattern that should be with Live definitions
          | otherwise = Edited p x

labelWidget :: MonadWidget t m => String -> Event t [String] -> m (Event t Definition)
labelWidget i delta = divClass "textPatternChain" $ divClass "labelWidgetDiv" $ do
  let delta' = fmap T.pack $ fmapMaybe lastOrNothing delta
  let attrs = constDyn $ ("class" =: "labelWidgetTextInput")
  y <- textInput $ def & textInputConfig_setValue .~ delta' & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ (T.pack i)
  return $ fmap (LabelText . T.unpack) $ _textInput_input y
