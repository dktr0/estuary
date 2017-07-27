module Estuary.Widgets.View where

import qualified Data.Map as Map
import Control.Monad
import Reflex
import Reflex.Dom

import Estuary.Types.Response
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.View
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Hint
import Estuary.Types.EditOrEval

import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text


viewInEnsembleWidget :: MonadWidget t m => String -> View -> Event t [Response Definition] ->
  m (Dynamic t DefinitionMap, Event t (Request Definition), Event t Hint)

viewInEnsembleWidget spaceName view deltasDown = do
  divClass "ensembleName" $ text $ "Ensemble: " ++ spaceName
  pwd <- divClass "ensemblePassword" $ do
    text "   Password:"
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    pwdInput <- textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
    return $ fmap AuthenticateInEnsemble $ _textInput_input pwdInput
  let deltasDown' = fmap (justSited spaceName . justEnsembleResponses) deltasDown
  (zones,edits,hints) <- viewWidget view deltasDown'
  let edits' = fmap (EnsembleRequest  . Sited spaceName) $ leftmost [edits,pwd]
  join <- liftM (JoinEnsemble spaceName <$) $ getPostBuild
  let requests = leftmost [edits',join]
  return (zones,requests,hints)

viewInSoloWidget :: MonadWidget t m => View -> m (Dynamic t DefinitionMap, Event t Hint)

viewInSoloWidget view = do
  (zones,edits,hints) <- viewWidget view never
  return (zones,hints)


viewWidget :: MonadWidget t m => View -> Event t [EnsembleResponse Definition] ->
  m (Dynamic t DefinitionMap, Event t (EnsembleRequest Definition), Event t Hint)

viewWidget (Views xs) deltasDown = foldM f i xs
  where
    i = (constDyn (Map.empty :: DefinitionMap), never, never)
    f b a = do
      let (prevZoneMap,prevEdits,prevHints) = b
      (zoneMap,edits,hints) <- viewWidget a deltasDown
      newZoneMap <- combineDyn Map.union prevZoneMap zoneMap
      let newEdits = leftmost [prevEdits,edits]
      let newHints = leftmost [prevHints,hints]
      return (newZoneMap,newEdits,newHints)

viewWidget (ViewDiv c v) deltasDown = divClass c $ viewWidget v deltasDown

viewWidget (StructureView n) deltasDown = do
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown -- Event t TransformedPattern
  (value,edits,hints) <- topLevelTransformedPatternWidget deltasDown'  -- m (Dynamic t TransformedPattern, Event t TransformedPattern, Event t Hint)
  value' <- mapDyn (Map.singleton n . Structure) value
  let edits' = fmap (ZoneRequest . Sited n . Edit . Structure) edits
  return (value',edits',hints)

viewWidget (TidalTextView n) deltasDown = do
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown -- Event t TransformedPattern
  (value,edits,hints) <- textPatternChainWidget deltasDown' -- m (Dynamic t TransformedPattern, Event t TransformedPattern, Event t Hint)
  value' <- mapDyn (Map.singleton n . Structure) value
  let edits' = fmap (ZoneRequest . Sited n . Edit . Structure) edits
  return (value',edits',hints)

viewWidget (LabelView n) deltasDown = do
  let deltasDown' = fmap (justLabelTexts . justEditsInZone n) deltasDown -- Event t [String]
  edits <- labelWidget deltasDown'
  let edits' = fmap (ZoneRequest . Sited n) edits
  return (constDyn Map.empty,edits',never)

viewWidget (EvaluableTextView n) deltasDown = do
  let deltasDown' = fmap (justEvaluableTexts . justEditsInZone n) deltasDown -- Event t [String]
  editsOrEvals <- evaluableTextWidget deltasDown'
  let editsOrEvals' = fmap (ZoneRequest . Sited n) editsOrEvals
  return (constDyn Map.empty,editsOrEvals',never)
