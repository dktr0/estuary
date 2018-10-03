{-# LANGUAGE ScopedTypeVariables #-}

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
import Estuary.Languages.TidalParser

textWidgetForPatternChain :: MonadWidget t m => String -> Event t String -> m (Dynamic t String, Event t String)
textWidgetForPatternChain i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine")
  x <- textInput $ def & textInputConfig_setValue .~ delta & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  let edits = _textInput_input x
  let value = _textInput_value x
  return (value,edits)

textAreaWidgetForPatternChain :: MonadWidget t m => String -> Event t String -> m (Dynamic t String, Event t String)
textAreaWidgetForPatternChain i delta = do
  let attrs = constDyn $ ("class" =: "textInputToEndOfLine")
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits)

tidalTextWidget :: forall t m. MonadWidget t m =>
  TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
tidalTextWidget i delta = divClass "tidalTextWidget" $ do -- *** TODO: css class name should be tidalTextWidget (in CSS also)
  let i' = transformedPatternToTidalTextPatternContents i
  let delta' = fmap transformedPatternToTidalTextPatternContents $ fmapMaybe lastOrNothing delta
  let deltaFuture = fmap forEditing delta'
  let parserFuture = fmap fst deltaFuture
  let textFuture = fmap snd deltaFuture
  (edit,eval) <- divClass "labelAndTextPattern" $ do
    let initialParser = fst $ forEditing i'
    let parserMap = constDyn $ fromList $ fmap (\x -> (x,show x)) tidalParsers
    d <- dropdown initialParser parserMap $ (def :: DropdownConfig t TidalParser) & dropdownConfig_setValue .~ parserFuture
    let parserValue = _dropdown_value d
    let parserEvent = _dropdown_change d
    b <- divClass "textInputLabel" $ button "eval"
    let initialText = snd $ forEditing i'
    (textValue,textEvent) <- textAreaWidgetForPatternChain initialText textFuture
    v' <- combineDyn (,) parserValue textValue
    let editEvent = tagDyn v' $ leftmost [() <$ parserEvent,() <$ textEvent]
    let evalEvent = tagDyn v' b
    return (editEvent,evalEvent)
  let deltaPast = fmap forRendering delta'
  pastValue <- holdDyn (forRendering i') $ leftmost [deltaPast,eval]
  futureValue <- holdDyn (forEditing i') $ leftmost [deltaFuture,edit]
  value <- combineDyn (\p f -> TidalTextPattern (g p f)) pastValue futureValue
  let deltaUpEdit = tagDyn value edit
  let deltaUpEval = tagDyn value eval
  let deltaUp = leftmost [deltaUpEdit,deltaUpEval]
  return (value,deltaUp,never)
  where
    g p x | p == x = Live p L3 -- *** TODO: this looks like it is a general pattern that should be with Live definitions
          | otherwise = Edited p x

transformedPatternToTidalTextPatternContents :: TransformedPattern -> Live (TidalParser,String)
transformedPatternToTidalTextPatternContents (TidalTextPattern x) = x
transformedPatternToTidalTextPatternContents _ = Live (MiniTidal,"") L3

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
labelWidget i delta = divClass "labelWidget" $ divClass "labelWidgetDiv" $ do
  let delta' = fmapMaybe lastOrNothing delta
  let attrs = constDyn $ ("class" =: "labelWidgetTextInput")
  y <- textInput $ def & textInputConfig_setValue .~ delta' & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  return $ fmap (Edit . LabelText) $ _textInput_input y
