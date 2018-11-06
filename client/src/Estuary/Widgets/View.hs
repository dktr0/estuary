{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.View where

import qualified Data.IntMap.Strict as Map
import Control.Monad
import Reflex
import Reflex.Dom
import Text.Read
import Data.Time.Clock

import Estuary.Types.Response
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.View
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleState
import Estuary.Types.Hint
import Estuary.Types.EditOrEval
import Estuary.Types.Terminal
import Estuary.Tidal.Types
import Estuary.Utility
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Estuary.Widgets.Terminal
import Estuary.Widgets.DynSvg
import Estuary.Types.TidalParser
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Types.Context
import Estuary.RenderInfo


viewWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> View -> DefinitionMap -> Event t [EnsembleResponse] -> m (Dynamic t DefinitionMap, Event t EnsembleRequest, Event t Hint)

viewWidget ctx renderInfo (Views xs) initialDefs deltasDown = foldM f i xs
  where
    i = (constDyn (Map.empty :: DefinitionMap), never, never)
    f b a = do
      let (prevZoneMap,prevEdits,prevHints) = b
      (zoneMap,edits,hints) <- viewWidget ctx renderInfo a initialDefs deltasDown
      newZoneMap <- combineDyn Map.union prevZoneMap zoneMap
      let newEdits = leftmost [prevEdits,edits]
      let newHints = leftmost [prevHints,hints]
      return (newZoneMap,newEdits,newHints)

viewWidget ctx renderInfo (ViewDiv c v) i deltasDown = divClass c $ viewWidget ctx renderInfo v i deltasDown

viewWidget ctx renderInfo (StructureView n) i deltasDown = do
  let i' = f $ Map.findWithDefault (Structure EmptyTransformedPattern) n i
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown
  (value,edits,hints) <- topLevelTransformedPatternWidget i' deltasDown'
  value' <- mapDyn (Map.singleton n . Structure) value
  let edits' = fmap (ZoneRequest . Sited n . Edit . Structure) edits
  return (value',edits',hints)
  where f (Structure x) = x
        f _ = EmptyTransformedPattern

viewWidget ctx renderInfo (TidalTextView n rows) i deltasDown = do
  let i' = f $ Map.findWithDefault (TextProgram (Live (TidalTextNotation MiniTidal,"") L3)) n i
  let deltasDown' = fmapMaybe (lastOrNothing . justTextPrograms . justEditsInZone n) deltasDown
  e <- mapDyn (Map.lookup n . errors) renderInfo
  (value,edits,hints) <- tidalTextWidget ctx e rows i' deltasDown'
  value' <- mapDyn (Map.singleton n . TextProgram) value
  let edits' = fmap (ZoneRequest . Sited n . Edit . TextProgram) edits
  return (value',edits',hints)
  where f (TextProgram x) = x
        f _ = Live (TidalTextNotation MiniTidal,"") L3

viewWidget _ _ (LabelView n) i deltasDown = do
  let i' = f $ Map.findWithDefault (LabelText "") n i
  let deltasDown' = fmap (justLabelTexts . justEditsInZone n) deltasDown
  edits <- labelWidget i' deltasDown'
  let edits' = fmap (ZoneRequest . Sited n) edits
  return (constDyn Map.empty,edits',never)
  where f (LabelText x) = x
        f _ = ""

viewWidget _ _ (EvaluableTextView n) i deltasDown = do
  let i' = f $ Map.findWithDefault (EvaluableText "") n i
  let deltasDown' = fmap (justEvaluableTexts . justEditsInZone n) deltasDown
  editsOrEvals <- evaluableTextWidget i' deltasDown'
  let editsOrEvals' = fmap (ZoneRequest . Sited n) editsOrEvals
  return (constDyn Map.empty,editsOrEvals',never)
  where f (EvaluableText x) = x
        f _ = ""

viewWidget _ _ SvgDisplayView _ _ = do
  testOurDynSvg
  return (constDyn Map.empty, never, never)
