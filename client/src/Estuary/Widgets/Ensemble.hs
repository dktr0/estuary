{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Ensemble (
  ensembleView,
  soloView
) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Time.Clock
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.Context
import Estuary.Types.Terminal
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Definition
import Estuary.RenderInfo
import Estuary.Types.Tempo
import Estuary.Widgets.View
import Estuary.Widgets.Tempo
import Estuary.Types.Hint
import Estuary.Types.EnsembleState
import Estuary.Types.Request
import Estuary.Types.EnsembleRequest
import Estuary.Types.Sited
import Estuary.Types.View
import Estuary.Render.AudioContext

extractInitialDefs :: (MonadWidget t m) => String -> Dynamic t Context -> m (DefinitionMap)
extractInitialDefs ensemble dynCtx = do
  ctx <- sample $ current dynCtx
  if ensemble == activeDefsEnsemble ctx then
    return $ definitions ctx
  else
    return $ emptyDefinitionMap

ensembleView :: MonadWidget t m
  => Dynamic t Context -> Dynamic t RenderInfo -> String -> Event t Command -> Event t [Response] ->
  m (Dynamic t DefinitionMap, Event t Request, Event t Hint,Event t Tempo)
ensembleView ctx renderInfo ensemble commands deltasDown = mdo

  let initialView = if ensemble == soloEnsembleName then standardView else emptyView
  -- *** is it dangerous to have initialView exist separate from initialState (below)?

  -- management of EnsembleState
  let initialState = (newEnsembleState ensemble) { defaultView = initialView }
  let commandChanges = fmap commandsToStateChanges commands
  let ensembleResponses = fmap justEnsembleResponses deltasDown
  let responseChanges = fmap ((foldl (.) id) . fmap responsesToStateChanges) ensembleResponses
  let handleChanges = fmap (\x es -> es { userHandle = T.unpack x}) hdl
  let requestChanges = fmap requestsToStateChanges edits
  ensembleState <- foldDyn ($) initialState $ mergeWith (.) [commandChanges,responseChanges,handleChanges,requestChanges]

  -- Ensemble name and password UI (created only if ensemble is not "")
  (hdl,pwdRequest) <- if ensemble == soloEnsembleName then return (never,never) else divClass "ensembleHeader" $ do
    divClass "ensembleName" $ text $ "Ensemble: " `T.append` T.pack ensemble
    hdl' <- divClass "ensembleHandle" $ do
      text "Name:"
      let attrs = constDyn ("class" =: "ensembleHandle")
      handleInput <- textInput $ def & textInputConfig_attributes .~ attrs
      return $ _textInput_input handleInput
    pwdRequest' <- divClass "ensemblePassword" $ do
      text "password:"
      let attrs = constDyn ("class" =: "ensemblePassword")
      pwdInput <- textInput $ def & textInputConfig_inputType .~ "password" & textInputConfig_attributes .~ attrs
      return $ fmap AuthenticateInEnsemble $ fmap T.unpack $ _textInput_input pwdInput
    return (hdl',pwdRequest')

  -- management of tempo including tempoWidget
  (tempoChanges,tempoEdits) <- tempoWidget ctx ensembleResponses
  let tempoHints = fmap TempoHint tempoChanges
  let tempoRequest = fmap SetTempo tempoEdits

  -- dynamic View UI
  initialDefs <- extractInitialDefs ensemble ctx
  let initialWidget = do
        (newInitialDefs, y, z) <- viewWidget ctx renderInfo initialView initialDefs ensembleResponses
        -- Skip the first rendered def and use the initialDefs to prevent audio pause when
        -- first rejoining an ensemble as the newInitialDefs are temporarily empty.
        replaceDefsEv <- tailE $ updated newInitialDefs
        newDefs <- holdDyn initialDefs $ replaceDefsEv
        return (newDefs, y, z)
  currentView <- liftM nubDyn $ mapDyn getActiveView ensembleState
  let newView = updated currentView
  currentDefs <- mapDyn zones ensembleState
  let newDefsAndView = attachDyn currentDefs newView
  let rebuildWidget = fmap (\(ds,v) -> viewWidget ctx renderInfo v ds ensembleResponses) newDefsAndView
  ui <- widgetHold initialWidget rebuildWidget
  defMap <- liftM joinDyn $ mapDyn (\(y,_,_) -> y) ui
  edits <- liftM switchPromptlyDyn $ mapDyn (\(_,y,_) -> y) ui
  hintsUi <- liftM switchPromptlyDyn $ mapDyn (\(_,_,y) -> y) ui
  let commandHints = attachDynWithMaybe commandToHint ensembleState commands
  let hints = leftmost [commandHints,tempoHints,hintsUi] -- *** note: might this occasionally lose a hint?

  -- form requests to send to server (but only if we are in a collaborative ensemble, ie. ensemble is not "")
  requests <- if ensemble == soloEnsembleName then return never else do
    joinRequest <- liftM (JoinEnsemble ensemble <$) $ getPostBuild
    let commandRequests = attachDynWithMaybe commandsToRequests ensembleState commands
    let ensembleRequests = fmap EnsembleRequest $ leftmost [edits,pwdRequest,tempoRequest,commandRequests]
    return $ leftmost [joinRequest,ensembleRequests]

  return (defMap,requests,hints,tempoChanges)


-- | A solo view is just an ensemble view that doesn't send or respond to
-- messages to/from the server.

soloView :: MonadWidget t m
  => Dynamic t Context -> Dynamic t RenderInfo -> Event t Command
  -> m (Dynamic t DefinitionMap, Event t Hint,Event t Tempo)
soloView ctx renderInfo commands = do
  (defMap,_,hints,tempoEvents) <- ensembleView ctx renderInfo soloEnsembleName commands never
  return (defMap,hints,tempoEvents)
