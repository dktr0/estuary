{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.View where

import qualified Data.IntMap.Strict as Map
import Control.Monad
import Reflex
import Reflex.Dom
import Text.Read
import Data.Time.Clock
import Data.Map as M (fromList)

import Estuary.Types.Response
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.View
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleState
import Estuary.Types.Hint
import Estuary.Types.Terminal
import Estuary.Tidal.Types
import Estuary.Utility
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Estuary.Widgets.Terminal
import Estuary.Widgets.SvgDisplay
import Estuary.Widgets.CanvasDisplay
import Estuary.Types.TidalParser
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Types.Context
import Estuary.RenderInfo
import Estuary.Widgets.Sequencer


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
  let edits' = fmap (ZoneRequest n . Structure) edits
  return (value',edits',hints)
  where f (Structure x) = x
        f _ = EmptyTransformedPattern

viewWidget ctx renderInfo (TextView n rows) i deltasDown = do
  let i' = f $ Map.findWithDefault (TextProgram (Live (TidalTextNotation MiniTidal,"") L3)) n i
  let deltasDown' = fmapMaybe (lastOrNothing . justTextPrograms . justEditsInZone n) deltasDown
  e <- mapDyn (Map.lookup n . errors) renderInfo
  (value,edits,hints) <- textNotationWidget ctx e rows i' deltasDown'
  value' <- mapDyn (Map.singleton n . TextProgram) value
  let edits' = fmap (ZoneRequest n . TextProgram) edits
  return (value',edits',hints)
  where f (TextProgram x) = x
        f _ = Live (TidalTextNotation MiniTidal,"") L3



viewWidget ctx renderInfo (SequenceView n) i deltasDown = do
  let i' = f $ Map.findWithDefault (Sequence defaultValue) n i
  let deltasDown' = fmapMaybe (lastOrNothing . justSequences . justEditsInZone n) deltasDown
  v <- sequencer i' deltasDown'
  value <- mapDyn (\(a,_,_) -> a) v
  edits <- liftM switchPromptlyDyn $ mapDyn (\(_,a,_)-> a) v
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,a)-> a) v
  value' <- mapDyn (Map.singleton n . Sequence) value
  let edits' = fmap (ZoneRequest n . Sequence) edits
  return (value',edits',hints)
  where f (Sequence x) = x
        f _ = defaultValue
        defaultValue = M.fromList [(0,("",replicate 8 False))]

viewWidget _ _ (LabelView n) i deltasDown = do
  let i' = f $ Map.findWithDefault (LabelText "") n i
  let deltasDown' = fmap (justLabelTexts . justEditsInZone n) deltasDown
  edits <- labelWidget i' deltasDown'
  let edits' = fmap (ZoneRequest n) edits
  return (constDyn Map.empty,edits',never)
  where f (LabelText x) = x
        f _ = ""

viewWidget _ rInfo (SvgDisplayView z) _ _ = svgDisplay z rInfo >> return (constDyn Map.empty, never, never)

viewWidget ctx _ (CanvasDisplayView z) _ _ = do
  mv <- fmap canvasOpsQueue $ sample $ current ctx
  canvasDisplay z mv
  return (constDyn Map.empty, never, never)

viewWidget ctx renderInfo (StructureView n) i deltasDown = do
  let i' = f $ Map.findWithDefault (Structure EmptyTransformedPattern) n i
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown
  (value,edits,hints) <- topLevelTransformedPatternWidget i' deltasDown'
  value' <- mapDyn (Map.singleton n . Structure) value
  let edits' = fmap (ZoneRequest n . Structure) edits
  return (value',edits',hints)
  where f (Structure x) = x
        f _ = EmptyTransformedPattern
