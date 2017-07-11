module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Container
import Estuary.Widgets.GeneralPattern
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Control.Monad
import GHCJS.DOM.EventM
import Data.Maybe
import Data.Map (fromList)

import Estuary.Utility (lastOrNothing)
import Estuary.Types.Definition
import Estuary.Types.EditOrEval
import Estuary.Types.Hint

textWidgetForPatternChain :: MonadWidget t m => Event t String -> m (Dynamic t String, Event t String)
textWidgetForPatternChain delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine")
  x <- textInput $ def & textInputConfig_setValue .~ delta & textInputConfig_attributes .~ attrs
  let edits = _textInput_input x
  let value = _textInput_value x
  return (value,edits)


textPatternChainWidget :: MonadWidget t m => Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
textPatternChainWidget delta = divClass "textPatternChain" $ do
  let delta' = fmapMaybe f $ fmapMaybe lastOrNothing delta
  let deltaA = fmap (\(x,_,_)->x) delta'
  let deltaB = fmap (\(_,x,_)->x) delta'
  let deltaC = fmap (\(_,_,x)->x) delta'
  (aValue,aEvent) <- divClass "labelAndTextPattern" $ do
    divClass "textInputLabel" $ text "sound"
    textWidgetForPatternChain deltaA
  (bValue,bEvent) <- divClass "labelAndTextPattern" $ do
    divClass "textInputLabel" $ text "up"
    textWidgetForPatternChain deltaB
  (cValue,cEvent) <- divClass "labelAndTextPattern" $ do
    divClass "textInputLabel" $ text "vowel"
    textWidgetForPatternChain deltaC
  value <- combineDyn TextPatternChain aValue bValue
  value' <- combineDyn ($) value cValue
  let deltaUp = tagDyn value' $ leftmost [aEvent,bEvent,cEvent]
  return (value',deltaUp,never)
  where f (TextPatternChain x y z) = Just (x,y,z)
        f _ = Nothing


evaluableTextWidget :: MonadWidget t m => Event t [String] -> m (Event t (EditOrEval Definition))
evaluableTextWidget delta = divClass "textWidget" $ do
  let delta' = fmapMaybe lastOrNothing delta
  let attrs = constDyn $ fromList [  ("class","textWidgetTextArea"), ("rows","5")]
  y <- textArea $ def & textAreaConfig_setValue .~ delta' & textAreaConfig_attributes .~ attrs
  let edits = fmap (Edit . EvaluableText) $ _textArea_input y
  evals <- button "eval"
  let evals' = fmap (Evaluate . EvaluableText) $ tagDyn (_textArea_value y) evals
  return $ leftmost [edits,evals']


labelWidget :: MonadWidget t m => Event t [String] -> m (Event t (EditOrEval Definition))
labelWidget delta = divClass "textPatternChain" $ divClass "labelWidgetDiv" $ do
  let delta' = fmapMaybe lastOrNothing delta
  let attrs = constDyn $ ("class" =: "labelWidgetTextInput")
  y <- textInput $ def & textInputConfig_setValue .~ delta' & textInputConfig_attributes .~ attrs
  return $ fmap (Edit . LabelText) $ _textInput_input y
