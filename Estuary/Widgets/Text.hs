{-# LANGUAGE ScopedTypeVariables #-}

module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom
import Control.Monad
import GHCJS.DOM.EventM
import Data.Maybe
import Data.Map (fromList)
import Data.Monoid

import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Container
import Estuary.Widgets.GeneralPattern
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Utility (lastOrNothing)
import Estuary.Types.Definition
import Estuary.Types.EditOrEval
import Estuary.Types.Hint
import Estuary.Languages.TidalParser
import Estuary.Types.Live
import Estuary.Types.TextNotation

textWidgetForPatternChain :: MonadWidget t m => String -> Event t String -> m (Dynamic t String, Event t String)
textWidgetForPatternChain i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine")
  x <- textInput $ def & textInputConfig_setValue .~ delta & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  let edits = _textInput_input x
  let value = _textInput_value x
  return (value,edits)

textAreaWidgetForPatternChain :: MonadWidget t m => Int -> String -> Event t String -> m (Dynamic t String, Event t String)
textAreaWidgetForPatternChain rows i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine" <> "rows" =: show rows)
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits)

tidalTextWidget :: forall t m. MonadWidget t m =>
  Int -> Live (TextNotation,String) -> Event t (Live (TextNotation,String)) ->
  m (Dynamic t (Live (TextNotation,String)),Event t (Live (TextNotation,String)),Event t Hint)
tidalTextWidget rows i delta = divClass "textPatternChain" $ do -- *** TODO: css class name should be tidalTextWidget (in CSS also)
  let deltaFuture = fmap forEditing delta
  let parserFuture = fmap fst deltaFuture
  let textFuture = fmap snd deltaFuture
  (edit,eval) <- divClass "labelAndTextPattern" $ do
    let initialParser = fst $ forEditing i
    let parserMap = constDyn $ fromList $ fmap (\x -> (TidalTextNotation x,show x)) tidalParsers
    d <- dropdown initialParser parserMap $ (def :: DropdownConfig t TidalParser) & dropdownConfig_setValue .~ parserFuture
    let parserValue = _dropdown_value d -- Dynamic t TidalParser
    let parserEvent = _dropdown_change d
    b <- divClass "textInputLabel" $ button "eval"
    let initialText = snd $ forEditing i
    -- helpButton <- divClass "textInputLabel" $ button "?"
    textVisible <- toggle True never -- really: toggle True helpButton
    -- helpVisible <- toggle False helpButton
    (textValue,textEvent) <- hideableWidget textVisible "someclass" $ textAreaWidgetForPatternChain rows initialText textFuture
    -- hideableWidget helpVisible "someclass" $ text "here is something helpful"
    v' <- combineDyn (,) parserValue textValue
    let editEvent = tagDyn v' $ leftmost [() <$ parserEvent,() <$ textEvent]
    let evalEvent = tagDyn v' b
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


evaluableTextWidget :: MonadWidget t m => String -> Event t [String] -> m (Event t (EditOrEval Definition))
evaluableTextWidget i delta = divClass "textWidget" $ do
  let delta' = fmapMaybe lastOrNothing delta
  let attrs = constDyn $ fromList [  ("class","textWidgetTextArea"), ("rows","5")]
  y <- textArea $ def & textAreaConfig_setValue .~ delta' & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = fmap (Edit . EvaluableText) $ _textArea_input y
  evals <- button "eval"
  let evals' = fmap (Evaluate . EvaluableText) $ tagDyn (_textArea_value y) evals
  return $ leftmost [edits,evals']

labelWidget :: MonadWidget t m => String -> Event t [String] -> m (Event t (EditOrEval Definition))
labelWidget i delta = divClass "textPatternChain" $ divClass "labelWidgetDiv" $ do
  let delta' = fmapMaybe lastOrNothing delta
  let attrs = constDyn $ ("class" =: "labelWidgetTextInput")
  y <- textInput $ def & textInputConfig_setValue .~ delta' & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  return $ fmap (Edit . LabelText) $ _textInput_input y
