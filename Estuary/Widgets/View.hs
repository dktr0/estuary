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
import Estuary.Types.EnsembleState
import Estuary.Types.Hint
import Estuary.Types.EditOrEval
import Estuary.Types.Terminal
import Estuary.Utility

import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Estuary.Widgets.Terminal

viewInEnsembleWidget :: MonadWidget t m =>
  String -> Event t Command -> Event t [ServerResponse] ->
  m (Dynamic t DefinitionMap, Event t ServerRequest, Event t Hint)

viewInEnsembleWidget ensemble commands deltasDown = do

  -- UI for global ensemble parameters
  divClass "ensembleName" $ text $ "Ensemble: " ++ ensemble
  hdl <- divClass "handleInEnsemble" $ do
    text "   Name/Handle:"
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    handleInput <- textInput $ def & textInputConfig_attributes .~ attrs
    return $ _textInput_input handleInput
  pwdRequest <- divClass "ensemblePassword" $ do
    text "   Ensemble Password:"
    let attrs = constDyn ("class" =: "webSocketTextInputs")
    pwdInput <- textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
    return $ fmap AuthenticateInEnsemble $ _textInput_input pwdInput

  -- management of EnsembleState
  let initialState = newEnsembleState ensemble
  let commandChanges = fmap commandsToStateChanges commands
  let ensembleResponses = fmap (justSited ensemble . justEnsembleResponses) deltasDown
  let responseChanges = fmap ((foldl (.) id) . fmap responsesToStateChanges) ensembleResponses
  let handleChanges = fmap (\x es -> es { userHandle = x}) hdl
  ensembleState <- foldDyn ($) initialState $ mergeWith (.) [commandChanges,responseChanges,handleChanges]

  -- dynamic View UI
  let initialWidget = viewWidget (Views []) ensembleResponses
  let newViews = fmapMaybe (lastOrNothing . justViews) ensembleResponses
  let anyNewView = fmap thing newViews
  let rebuildWidget = fmap (flip viewWidget $ ensembleResponses) anyNewView
  x <- widgetHold initialWidget rebuildWidget
  zones <- liftM joinDyn $ mapDyn (\(y,_,_) -> y) x
  edits <- liftM switchPromptlyDyn $ mapDyn (\(_,y,_) -> y) x
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,y) -> y) x

  -- form requests to send to server
  joinRequest <- liftM (JoinEnsemble ensemble <$) $ getPostBuild
  let commandRequests = attachDynWithMaybe commandsToRequests ensembleState commands
  let ensembleRequests = fmap (EnsembleRequest . Sited ensemble) $ leftmost [edits,pwdRequest,commandRequests]
  let requests = leftmost [joinRequest,ensembleRequests]
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

viewWidget (CQenzeView n) deltasDown = do
  let deltasDown' = fmap (justStructures . justEditsInZone n) deltasDown -- Event t TransformedPattern
  (value,edits,hints) <- cqenzeWidget deltasDown' -- m (Dynamic t TransformedPattern, Event t TransformedPattern, Event t Hint)
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
