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


ensembleView :: MonadWidget t m
  => Dynamic t Context -> Dynamic t RenderInfo -> Event t Command -> Event t [Response] ->
  m (Dynamic t DefinitionMap, Event t Request, Event t Hint,Event t Tempo)
ensembleView ctx renderInfo commands deltasDown = mdo

  iCtx <- sample $ current ctx
  let initialState = ensembleState iCtx
  let initialDefs = zones initialState
  let eName = ensembleName initialState
  let uName = userHandle initialState

  -- Ensemble name and username UI (created only if ensemble is not "", ie. not in solo mode)
  when (ensemble /= "") $ divClass "ensembleHeader primary-color ui-font" $ do
    divClass "ensembleName ui-font primary-color" $ text $ "Ensemble: " <> eName
    divClass "ensembleHandle ui-font primary-color" $ text $ "UserName: " <> uName

  -- management of tempo including tempoWidget
  let ensembleResponses = fmap justEnsembleResponses deltasDown
  (tempoChanges,tempoEdits) <- tempoWidget ctx ensembleResponses
  let tempoHints = fmap TempoHint tempoChanges
  let tempoRequest = fmap SetTempo tempoEdits

  -- dynamic View UI
  let initialWidget = do
        -- *** WORKING HERE (below): viewWidget shouldn't need initalDefs because they are in ctx
        (newInitialDefs, y, z) <- viewWidget ctx renderInfo initialView initialDefs ensembleResponses
        -- Skip the first rendered def and use the initialDefs to prevent audio pause when
        -- first rejoining an ensemble as the newInitialDefs are temporarily empty.
        replaceDefsEv <- tailE $ updated newInitialDefs
        newDefs <- holdDyn initialDefs $ replaceDefsEv
        return (newDefs, y, z)
  currentView <- holdUniqDyn $ fmap getActiveView ensembleState
  let newView = updated currentView
  let currentDefs = fmap zones ensembleState
  let newDefsAndView = attachPromptlyDyn currentDefs newView
  let rebuildWidget = fmap (\(ds,v) -> viewWidget ctx renderInfo v ds ensembleResponses) newDefsAndView
  ui <- widgetHold initialWidget rebuildWidget
  let defMap = join $ fmap (\(y,_,_) -> y) ui
  let edits = switchPromptlyDyn $ fmap (\(_,y,_) -> y) ui
  let hintsUi = switchPromptlyDyn $ fmap (\(_,_,y) -> y) ui
  let commandHints = attachPromptlyDynWithMaybe commandToHint ensembleState commands
  let hints = leftmost [commandHints,tempoHints,hintsUi] -- *** note: might this occasionally lose a hint?

  -- form requests to send to server (but only if we are in a collaborative ensemble, ie. ensemble is not "")
  requests <- if ensemble == soloEnsembleName then return never else do
    let commandRequests = attachPromptlyDynWithMaybe commandsToRequests ensembleState commands
    return $ fmap EnsembleRequest $ leftmost [edits,tempoRequest,commandRequests]

  return (defMap,requests,hints,tempoChanges)


-- | A solo view is just an ensemble view that doesn't send or respond to
-- messages to/from the server.

soloView :: MonadWidget t m
  => Dynamic t Context -> Dynamic t RenderInfo -> Event t Command
  -> m (Dynamic t DefinitionMap, Event t Hint,Event t Tempo)
soloView ctx renderInfo commands = do
  (defMap,_,hints,tempoEvents) <- ensembleView ctx renderInfo commands never
  return (defMap,hints,tempoEvents)
