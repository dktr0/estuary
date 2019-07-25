{-# LANGUAGE OverloadedStrings #-}

-- The type EnsembleC represents the state of an Ensemble from the perspective
-- of an Estuary client.

module Estuary.Types.EnsembleC where

import Data.Map
import qualified Data.IntMap.Strict as IntMap
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import TextShow

import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Sited
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Tempo
import Estuary.Types.Hint
import Estuary.Types.ViewsParser
import Estuary.Render.AudioContext
import Estuary.Types.Participant
import Estuary.Types.Tempo

import Estuary.Types.Ensemble
import Estuary.Types.Chat

data EnsembleC = EnsembleC {
  ensemble :: Ensemble,
  userHandle :: Text, -- how the user is logged in/appears to others in the ensemble; "" = anonymous
  view :: Either View Text -- Rights are from preset views, Lefts are local views
}

emptyEnsembleC :: UTCTime -> EnsembleC
emptyEnsembleC t = EnsembleC {
  ensemble = emptyEnsemble t,
  userHandle = "",
  view = Right "default"
  }

leaveEnsembleC :: EnsembleC -> EnsembleC
leaveEnsembleC x = x {
  ensemble = leaveEnsemble (ensemble x),
  userHandle = ""
  }

modifyEnsemble :: (Ensemble -> Ensemble) -> EnsembleC -> EnsembleC
modifyEnsemble f e = e { ensemble = f (ensemble e) }

activeView :: EnsembleC -> View
activeView e = either id f (view e)
  where f x = maybe emptyView id $ lookupView x (ensemble e)

nameOfActiveView :: EnsembleC -> Text
nameOfActiveView e = either (const "(local view)") id $ view e

selectPresetView :: Text -> EnsembleC -> EnsembleC
selectPresetView t e = e { view = Right t }

selectLocalView :: View -> EnsembleC -> EnsembleC
selectLocalView v e = e { view = Left v }

-- replaceStandardView selects a standard view while also redefining it
-- according to the provided View argument. (To be used when a custom view is
-- republished as a standard view in an ensemble.)
replaceStandardView :: Text -> View -> EnsembleC -> EnsembleC
replaceStandardView t v e = e {
  ensemble = writeView t v (ensemble e),
  view = Right t
  }

commandToHint :: EnsembleC -> Terminal.Command -> Maybe Hint
commandToHint _ (Terminal.LocalView _) = Just $ LogMessage "local view changed"
commandToHint _ (Terminal.PresetView x) = Just $ LogMessage $ "preset view " <> x <> " selected"
commandToHint _ (Terminal.PublishView x) = Just $ LogMessage $ "active view published as " <> x
commandToHint es (Terminal.ActiveView) = Just $ LogMessage $ nameOfActiveView es
commandToHint es (Terminal.ListViews) = Just $ LogMessage $ showt $ listViews $ ensemble es
commandToHint es (Terminal.DumpView) = Just $ LogMessage $ dumpView (activeView es)
commandToHint _ _ = Nothing

commandsToStateChanges :: Terminal.Command -> EnsembleC -> EnsembleC
commandsToStateChanges (Terminal.LocalView v) es = selectLocalView v es
commandsToStateChanges (Terminal.PresetView t) es = selectPresetView t es
commandsToStateChanges (Terminal.PublishView t) es = replaceStandardView t (activeView es) es
commandsToStateChanges _ es = es

responsesToStateChanges :: EnsembleResponse -> EnsembleC -> EnsembleC
responsesToStateChanges (TempoRcvd t) es = modifyEnsemble (writeTempo t) es
responsesToStateChanges (ZoneRcvd n v) es = modifyEnsemble (writeZone n v) es
responsesToStateChanges (ViewRcvd t v) es = modifyEnsemble (writeView t v) es
responsesToStateChanges (ChatRcvd c) es = modifyEnsemble (appendChat c) es
responsesToStateChanges (ParticipantJoins n x) es = modifyEnsemble (writeParticipant n x) es
responsesToStateChanges (ParticipantUpdate n x) es = modifyEnsemble (writeParticipant n x) es
responsesToStateChanges (ParticipantLeaves n) es = modifyEnsemble (deleteParticipant n) es
responsesToStateChanges (AnonymousParticipants n) es = modifyEnsemble (writeAnonymousParticipants n) es
responsesToStateChanges _ es = es

commandsToRequests :: EnsembleC -> Terminal.Command -> Maybe EnsembleRequest
commandsToRequests es (Terminal.PublishView x) = Just (WriteView x (activeView es))
commandsToRequests es (Terminal.Chat x) = Just (WriteChat x)
commandsToRequests _ _ = Nothing

messageForEnsembleResponse :: EnsembleResponse -> Maybe Text
messageForEnsembleResponse (ChatRcvd c) = Just $ showChatMessage c
messageForEnsembleResponse (ParticipantJoins n _) = Just $ n <> " has joined the ensemble"
messageForEnsembleResponse (ParticipantLeaves n) = Just $ n <> " has left the ensemble"
messageForEnsembleResponse _ = Nothing
