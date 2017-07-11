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
import Estuary.Types.Action
import Estuary.Types.Hint
import Estuary.Types.EditOrEval

import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text


viewInSpaceWidget :: MonadWidget t m => String -> View -> Event t [Response Definition] ->
  m (Dynamic t DefinitionMap, Event t (Request Definition), Event t Hint)

viewInSpaceWidget spaceName view deltasDown = do
  let deltasDown' = fmap (justSited spaceName . justSpaceResponses) deltasDown
  (zones,edits,hints) <- viewWidget view deltasDown'
  let edits' = fmap (SpaceRequest  . Sited spaceName) edits
  return (zones,edits',hints)


viewWidget :: MonadWidget t m => View -> Event t [Action Definition] ->
  m (Dynamic t DefinitionMap, Event t (Action Definition), Event t Hint)

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

viewWidget (StructureView n) deltasDown = do
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown -- Event t TransformedPattern
  (value,edits,hints) <- topLevelTransformedPatternWidget deltasDown'  -- m (Dynamic t TransformedPattern, Event t TransformedPattern, Event t Hint)
  value' <- mapDyn (Map.singleton n . Structure) value
  let edits' = fmap (ZoneAction . Sited n . Edit . Structure) edits
  return (value',edits',hints)

viewWidget (TidalTextView n) deltasDown = do
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown -- Event t TransformedPattern
  (value,edits,hints) <- textPatternChainWidget deltasDown' -- m (Dynamic t TransformedPattern, Event t TransformedPattern, Event t Hint)
  value' <- mapDyn (Map.singleton n . Structure) value
  let edits' = fmap (ZoneAction . Sited n . Edit . Structure) edits
  return (value',edits',hints)

viewWidget (LabelView n) deltasDown = do
  let deltasDown' = fmap (justLabelTexts . justEditsInZone n) deltasDown -- Event t [String]
  edits <- labelWidget deltasDown'
  let edits' = fmap (ZoneAction . Sited n) edits
  return (constDyn Map.empty,edits',never)

viewWidget (EvaluableTextView n) deltasDown = do
  let deltasDown' = fmap (justEvaluableTexts . justEditsInZone n) deltasDown -- Event t [String]
  editsOrEvals <- evaluableTextWidget deltasDown'
  let editsOrEvals' = fmap (ZoneAction . Sited n) editsOrEvals
  return (constDyn Map.empty,editsOrEvals',never)
