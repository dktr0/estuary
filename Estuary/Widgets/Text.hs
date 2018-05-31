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

textPatternChainWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
textPatternChainWidget i delta = divClass "textPatternChain" $ do
  let (iA,iB,iC) = g i
  let delta' = fmapMaybe f $ fmapMaybe lastOrNothing delta
  let deltaA = fmap (\(x,_,_)->x) delta'
  let deltaB = fmap (\(_,x,_)->x) delta'
  let deltaC = fmap (\(_,_,x)->x) delta'
  (aValue,aEvent) <- divClass "labelAndTextPattern" $ do
    divClass "textInputLabel" $ text "sound"
    textWidgetForPatternChain iA deltaA
  (bValue,bEvent) <- divClass "labelAndTextPattern" $ do
    divClass "textInputLabel" $ text "up"
    textWidgetForPatternChain iB deltaB
  (cValue,cEvent) <- divClass "labelAndTextPattern" $ do
    divClass "textInputLabel" $ text "vowel"
    textWidgetForPatternChain iC deltaC
  value <- combineDyn TextPatternChain aValue bValue
  value' <- combineDyn ($) value cValue
  let deltaUp = tagDyn value' $ leftmost [aEvent,bEvent,cEvent]
  return (value',deltaUp,never)
  where f (TextPatternChain x y z) = Just (x,y,z)
        f _ = Nothing
        g (TextPatternChain x y z) = (x,y,z)
        g _ = ("","","")

cqenzeWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
cqenzeWidget = miniLanguageWidget "CQenze " f CQenzePattern
  where
    f (CQenzePattern x) = Just x
    f _ = Nothing

miniTidalWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
miniTidalWidget = miniLanguageWidget "MiniTidal " f MiniTidalPattern
  where
    f (MiniTidalPattern x) = Just x
    f _ = Nothing

moreliaWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
moreliaWidget = miniLanguageWidget "Morelia " f MoreliaPattern
  where
    f (MoreliaPattern x) = Just x
    f _ = Nothing

test1Widget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
test1Widget = miniLanguageWidget "Test1 " f Test1Pattern
  where
    f (Test1Pattern x) = Just x
    f _ = Nothing

saludosWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
saludosWidget = miniLanguageWidget "Saludos" f SaludosPattern
  where
    f (SaludosPattern x) = Just x
    f _ = Nothing

colombiaWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
colombiaWidget = miniLanguageWidget "Colombia" f ColombiaPattern
  where
    f (ColombiaPattern x) = Just x
    f _ = Nothing

siWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
siWidget = miniLanguageWidget "Si" f SiPattern
  where
    f (SiPattern x) = Just x
    f _ = Nothing


sentidosWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
sentidosWidget = miniLanguageWidget "Sentidos" f SentidosPattern
  where
    f (SentidosPattern x) = Just x
    f _ = Nothing


sabortsWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
sabortsWidget = miniLanguageWidget "Saborts" f SabortsPattern
  where
    f (SabortsPattern x) = Just x
    f _ = Nothing

puntoyyaWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
puntoyyaWidget = miniLanguageWidget "Puntoyya" f PuntoyyaPattern
  where
    f (PuntoyyaPattern x) = Just x
    f _ = Nothing

sucixxxWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
sucixxxWidget = miniLanguageWidget "Sucixxx" f SucixxxPattern
  where
    f (SucixxxPattern x) = Just x
    f _ = Nothing

vocesotrevezWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
vocesotrevezWidget = miniLanguageWidget "Vocesotrevez" f VocesotrevezPattern
  where
    f (VocesotrevezPattern x) = Just x
    f _ = Nothing

imaginaWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
imaginaWidget = miniLanguageWidget "Imagina" f ImaginaPattern
  where
    f (ImaginaPattern x) = Just x
    f _ = Nothing

alobestiaWidget :: MonadWidget t m => TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
alobestiaWidget = miniLanguageWidget "Alobestia" f AlobestiaPattern
  where
    f (AlobestiaPattern x) = Just x
    f _ = Nothing

miniLanguageWidget :: MonadWidget t m =>
  String -> (TransformedPattern -> Maybe (Live String)) -> (Live String -> TransformedPattern) ->
  TransformedPattern -> Event t [TransformedPattern] ->
  m (Dynamic t TransformedPattern,Event t TransformedPattern,Event t Hint)
miniLanguageWidget label f h i delta = divClass "textPatternChain" $ do
  let i' = maybe (Live "" L3) (id) $ f i
  let delta' = fmapMaybe f $ fmapMaybe lastOrNothing delta
  let deltaPast = fmap forRendering delta'
  let deltaFuture = fmap forEditing delta'
  (editText,editEvent,evalButton) <- divClass "labelAndTextPattern" $ do
    b <- divClass "textInputLabel" $ button label
    (v,e) <- textAreaWidgetForPatternChain (forEditing i') deltaFuture
    return (v,e,b)
  let evalEvent = tagDyn editText evalButton
  pastValue <- holdDyn (forRendering i') $ leftmost [deltaPast,evalEvent]
  futureValue <- holdDyn (forEditing i') $ leftmost [deltaFuture,editEvent]
  value <- combineDyn (\p f -> h (g p f)) pastValue futureValue
  let deltaUpEdit = tagDyn value editEvent
  let deltaUpEval = tagDyn value evalEvent
  let deltaUp = leftmost [deltaUpEdit,deltaUpEval]
  return (value,deltaUp,never)
  where
    g p f | p == f = Live p L3
          | otherwise = Edited p f

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
