{-# LANGUAGE OverloadedStrings #-}

-- The type EnsembleC represents the state of an Ensemble from the perspective
-- of an Estuary client.

module Estuary.Types.EnsembleC where

import Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Control.Applicative

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
import Estuary.ViewPresets

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

-- if a specific named view is in the ensemble's map of views we get that
-- or if not but a view with that names is in Estuary's presets we get that
-- so ensembles can have a different default view than solo mode simply by
-- defining a view at the key "default"

inAnEnsemble :: EnsembleC -> Bool
inAnEnsemble e = ensembleName (ensemble e) /= ""

lookupView :: Text -> Ensemble -> Maybe View
lookupView t e = Map.lookup t (views e) <|> Map.lookup t presetViews

listViews :: Ensemble -> [Text]
listViews e = Map.keys $ Map.union (views e) presetViews

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

commandToStateChange :: Terminal.Command -> EnsembleC -> EnsembleC
commandToStateChange (Terminal.LocalView v) es = selectLocalView v es
commandToStateChange (Terminal.PresetView t) es = selectPresetView t es
commandToStateChange (Terminal.PublishView t) es = replaceStandardView t (activeView es) es
commandToStateChange _ es = es

requestToStateChange :: EnsembleRequest -> EnsembleC -> EnsembleC
requestToStateChange (WriteTempo x) es = modifyEnsemble (writeTempo x) es
requestToStateChange (WriteZone n v) es = modifyEnsemble (writeZone n v) es
requestToStateChange (WriteView t v) es = modifyEnsemble (writeView t v) es
requestToStateChange _ es = es
-- note: WriteChat and WriteStatus don't directly affect the EnsembleC and are thus
-- not matched here. Instead, the server responds to these requests to all participants
-- and in this way the information "comes back down" from the server.

responseToStateChange :: EnsembleResponse -> EnsembleC -> EnsembleC
responseToStateChange (TempoRcvd t) es = modifyEnsemble (writeTempo t) es
responseToStateChange (ZoneRcvd n v) es = modifyEnsemble (writeZone n v) es
responseToStateChange (ViewRcvd t v) es = modifyEnsemble (writeView t v) es
responseToStateChange (ChatRcvd c) es = modifyEnsemble (appendChat c) es
responseToStateChange (ParticipantJoins n x) es = modifyEnsemble (writeParticipant n x) es
responseToStateChange (ParticipantUpdate n x) es = modifyEnsemble (writeParticipant n x) es
responseToStateChange (ParticipantLeaves n) es = modifyEnsemble (deleteParticipant n) es
responseToStateChange (AnonymousParticipants n) es = modifyEnsemble (writeAnonymousParticipants n) es
responseToStateChange _ es = es

commandToRequest :: EnsembleC -> Terminal.Command -> Maybe EnsembleRequest
commandToRequest es (Terminal.PublishView x) = Just (WriteView x (activeView es))
commandToRequest es (Terminal.Chat x) = Just (WriteChat x)
commandToRequest _ _ = Nothing

responseToMessage :: EnsembleResponse -> Maybe Text
responseToMessage (ChatRcvd c) = Just $ showChatMessage c
responseToMessage (ParticipantJoins n _) = Just $ n <> " has joined the ensemble"
responseToMessage (ParticipantLeaves n) = Just $ n <> " has left the ensemble"
responseToMessage _ = Nothing
