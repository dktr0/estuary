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

viewInEnsembleWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo ->
  String -> UTCTime -> Event t Command -> Event t [ServerResponse] ->
  m (Dynamic t DefinitionMap, Event t ServerRequest, Event t Hint)

viewInEnsembleWidget ctx renderInfo ensemble now commands deltasDown = mdo

  -- UI for global ensemble parameters
  (hdl,pwdRequest,tempoRequest) <- divClass "ensembleHeader" $ do
    divClass "ensembleName" $ text $ "Ensemble: " ++ ensemble
    hdl' <- divClass "ensembleHandle" $ do
      text "Name:"
      let attrs = constDyn ("class" =: "ensembleHandle")
      handleInput <- textInput $ def & textInputConfig_attributes .~ attrs
      return $ _textInput_input handleInput
    pwdRequest' <- divClass "ensemblePassword" $ do
      text "password:"
      let attrs = constDyn ("class" =: "ensemblePassword")
      pwdInput <- textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
      return $ fmap AuthenticateInEnsemble $ _textInput_input pwdInput
    tempoRequest' <- divClass "ensembleTempo" $ do
      text "Ensemble Tempo:"
      let attrs = constDyn ("class" =: "ensembleTempo")
      tempoInput <- textInput $ def & textInputConfig_attributes .~ attrs
      let newTempo = fmapMaybe (readMaybe :: String -> Maybe Double) $ _textInput_input tempoInput
      return $ fmap TempoChange newTempo
    return (hdl',pwdRequest',tempoRequest')

  -- management of EnsembleState
  let initialState = newEnsembleState ensemble now
  let commandChanges = fmap commandsToStateChanges commands
  let ensembleResponses = fmap (justSited ensemble . justEnsembleResponses) deltasDown
  let responseChanges = fmap ((foldl (.) id) . fmap responsesToStateChanges) ensembleResponses
  let handleChanges = fmap (\x es -> es { userHandle = x}) hdl
  let requestChanges = fmap requestsToStateChanges edits
  ensembleState <- foldDyn ($) initialState $ mergeWith (.) [commandChanges,responseChanges,handleChanges,requestChanges]

  tempoHints <- liftM (fmap TempoHint . updated . nubDyn) $ mapDyn Estuary.Types.EnsembleState.tempo ensembleState

  -- dynamic View UI
  let initialWidget = viewWidget ctx renderInfo emptyView Map.empty ensembleResponses
  currentView <- liftM nubDyn $ mapDyn getActiveView ensembleState
  let newView = updated currentView
  currentDefs <- mapDyn zones ensembleState
  let newDefsAndView = attachDyn currentDefs newView
  let rebuildWidget = fmap (\(ds,v) -> viewWidget ctx renderInfo v ds ensembleResponses) newDefsAndView
  ui <- widgetHold initialWidget rebuildWidget
  defMap <- liftM joinDyn $ mapDyn (\(y,_,_) -> y) ui
  edits <- liftM switchPromptlyDyn $ mapDyn (\(_,y,_) -> y) ui
  hintsUi <- liftM switchPromptlyDyn $ mapDyn (\(_,_,y) -> y) ui
  let hints = leftmost [tempoHints,hintsUi] -- *** note: might this occasionally lose a hint?

  -- form requests to send to server
  joinRequest <- liftM (JoinEnsemble ensemble <$) $ getPostBuild
  let commandRequests = attachDynWithMaybe commandsToRequests ensembleState commands
  let ensembleRequests = fmap (EnsembleRequest . Sited ensemble) $ leftmost [edits,pwdRequest,tempoRequest,commandRequests]
  let requests = leftmost [joinRequest,ensembleRequests]
  return (defMap,requests,hints)


viewInSoloWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> View -> m (Dynamic t DefinitionMap, Event t Hint)
viewInSoloWidget ctx renderInfo view = do
  (zones,edits,hints) <- viewWidget ctx renderInfo view Map.empty never
  return (zones,hints)


viewWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> View -> DefinitionMap -> Event t [EnsembleResponse Definition] -> m (Dynamic t DefinitionMap, Event t (EnsembleRequest Definition), Event t Hint)

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
