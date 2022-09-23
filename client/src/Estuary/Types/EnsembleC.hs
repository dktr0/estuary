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
import Control.Monad.IO.Class
import Data.Sequence as Seq

import Estuary.Types.Response
import Estuary.Types.ResourceType
import Estuary.Types.ResourceOp
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.View.Parser
import Estuary.Types.View.Presets
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Tempo
import Estuary.Types.Hint
import Estuary.Types.Tempo
import Estuary.Types.Participant
import Estuary.Resources
import Estuary.Types.TranslatableText
import Estuary.Types.Language
import Estuary.Types.Ensemble
import Estuary.Types.Chat
import Estuary.Client.Settings

data EnsembleC = EnsembleC {
  ensemble :: Ensemble,
  userHandle :: Text, -- how the user is logged in/appears to others in the ensemble; "" = anonymous
  location :: Text, -- the user's location in the ensemble (cached for re-authentication scenarios)
  password :: Text, -- the participant password for the ensemble (cached for re-authentication scenarios)
  view :: Either View Text -- Rights are from preset views, Lefts are local views
}

emptyEnsembleC :: UTCTime -> EnsembleC
emptyEnsembleC t = EnsembleC {
  ensemble = emptyEnsemble t,
  userHandle = "",
  Estuary.Types.EnsembleC.location = "",
  Estuary.Types.EnsembleC.password = "",
  view = Right "def"
  }

joinEnsembleC :: Text -> Text -> Text -> Text -> EnsembleC -> EnsembleC
joinEnsembleC eName uName loc pwd es = modifyEnsemble (\x -> x { ensembleName = eName } ) $ es {  userHandle = uName, Estuary.Types.EnsembleC.location = loc, Estuary.Types.EnsembleC.password = pwd, view = Right "def" }

leaveEnsembleC :: EnsembleC -> EnsembleC
leaveEnsembleC x = x {
  ensemble = leaveEnsemble (ensemble x),
  userHandle = "",
  Estuary.Types.EnsembleC.location = "",
  Estuary.Types.EnsembleC.password = ""
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
  where f x = maybe EmptyView id $ lookupView x (ensemble e)

nameOfActiveView :: EnsembleC -> Text
nameOfActiveView e = either (const "(local view)") id $ view e

selectPresetView :: Text -> EnsembleC -> EnsembleC
selectPresetView t e = e { view = Right t }

{- factored out already into W monad
selectLocalView :: View -> EnsembleC -> EnsembleC
selectLocalView v e = e { view = Left v } -}

-- replaceStandardView selects a standard view while also redefining it
-- according to the provided View argument. (To be used when a custom view is
-- republished as a standard view in an ensemble.)
replaceStandardView :: Text -> View -> EnsembleC -> EnsembleC
replaceStandardView t v e = e {
  ensemble = writeView t v (ensemble e),
  view = Right t
  }

readableTempo:: Tempo -> Text
readableTempo tempo =
  let f = realToFrac (freq tempo) :: Double
      t = (time tempo)
      c = realToFrac (count tempo) :: Double
  in "Freq: " <> showt f <> "Time: " <> (T.pack $ show t) <> "Count: " <> showt c


requestToEnsembleC :: MonadIO m => Resources -> Request -> EnsembleC -> m EnsembleC
requestToEnsembleC _ LeaveEnsemble e = pure $ leaveEnsembleC e
requestToEnsembleC _ (DeleteThisEnsemble _) e = pure $ leaveEnsembleC e
requestToEnsembleC _ (WriteTempo x) e = pure $ modifyEnsemble (writeTempo x) e
requestToEnsembleC _ (WriteZone n v) e = pure $ modifyEnsemble (writeZone n v) e
requestToEnsembleC _ (WriteView t v) e = pure $ modifyEnsemble (writeView t v) e
requestToEnsembleC rs (WriteResourceOps x) e = do
  setResourceOps rs x
  pure $ modifyEnsemble (\y -> y { resourceOps = x } ) e
requestToEnsembleC ResetZones e = pure $ modifyEnsemble (\e -> e { zones = IntMap.empty } ) e
requestToEnsembleC ResetViews e = pure $ modifyEnsemble (\e -> e { views = Map.empty } ) $ selectPresetView "def" e
requestToEnsembleC (Reset t) e = pure $ modifyEnsemble (\e -> e { zones = IntMap.empty }) $ modifyEnsemble (writeTempo t) e
requestToEnsembleC _ _ e = pure e


ensembleResponseToStateChange :: EnsembleResponse -> EnsembleC -> EnsembleC
ensembleResponseToStateChange (TempoRcvd t) es = modifyEnsemble (writeTempo t) es
ensembleResponseToStateChange (ZoneRcvd n v) es = modifyEnsemble (writeZone n v) es
ensembleResponseToStateChange (ViewRcvd t v) es = modifyEnsemble (writeView t v) es
ensembleResponseToStateChange (ChatRcvd c) es = modifyEnsemble (appendChat c) es
ensembleResponseToStateChange (ParticipantJoins x) es = modifyEnsemble (writeParticipant (name x) x) es
ensembleResponseToStateChange (ParticipantUpdate x) es = modifyEnsemble (writeParticipant (name x) x) es
ensembleResponseToStateChange (ParticipantLeaves n) es = modifyEnsemble (deleteParticipant n) es
ensembleResponseToStateChange (AnonymousParticipants n) es = modifyEnsemble (writeAnonymousParticipants n) es
ensembleResponseToStateChange ResetZonesResponse es = modifyEnsemble (\e -> e { zones = IntMap.empty } ) es
ensembleResponseToStateChange ResetViewsResponse es = modifyEnsemble (\e -> e { views = Map.empty } ) $ selectPresetView "def" es
ensembleResponseToStateChange (ResetTempoResponse t) es = modifyEnsemble (writeTempo t) es
ensembleResponseToStateChange (ResetResponse t) es = modifyEnsemble (\e -> e { zones = IntMap.empty }) $ modifyEnsemble (writeTempo t) es
ensembleResponseToStateChange (ResourceOps s) es = modifyEnsemble (\e -> e { resourceOps = s} ) es
ensembleResponseToStateChange _ es = es

ensembleResponseIO :: MonadIO m => Resources -> EnsembleResponse -> m ()
ensembleResponseIO rs (ResourceOps s) = setResourceOps rs s
ensembleResponseIO _ _ = return ()

responseToStateChange :: Response -> EnsembleC -> EnsembleC
responseToStateChange (JoinedEnsemble eName uName loc pwd) es = joinEnsembleC eName uName loc pwd es
responseToStateChange _ es = es


responseToMessage :: Response -> Maybe Text
responseToMessage (ResponseError e) = Just $ "error: " <> e
responseToMessage (ResponseOK m) = Just m
responseToMessage (EnsembleResponse (ChatRcvd c)) = Just $ showChatMessage c
responseToMessage (EnsembleResponse (ParticipantJoins x)) = Just $ name x <> " has joined the ensemble"
responseToMessage (EnsembleResponse (ParticipantLeaves n)) = Just $ n <> " has left the ensemble"
responseToMessage (EnsembleResponse (TempoRcvd _)) = Just $ "received new tempo"
responseToMessage (EnsembleResponse ResetZonesResponse) = Just $ "received ResetZones"
responseToMessage (EnsembleResponse ResetViewsResponse) = Just $ "received ResetViews"
responseToMessage (EnsembleResponse (ResetTempoResponse _)) = Just $ "received ResetTempo"
responseToMessage (EnsembleResponse (ResetResponse _)) = Just $ "received Reset (resetting zones and tempo)"
responseToMessage (EnsembleResponse (AnonymousParticipants n)) = Just $ showt n <>  " anonymous participants"
-- the cases below are for debugging only and can be commented out when not debugging:
-- responseToMessage (ZoneRcvd n _) = Just $ "received zone " <> showt n
-- responseToMessage (ViewRcvd n _) = Just $ "received view " <> n
-- responseToMessage (ParticipantUpdate n _) = Just $ "received ParticipantUpdate about " <> n
-- don't comment out the case below, of course!
responseToMessage _ = Nothing

-- a hack for the sole purpose of supporting resets ahead of the big refactor completing
ensembleRequestsToResponses :: EnsembleRequest -> Maybe EnsembleResponse
ensembleRequestsToResponses ResetZonesRequest = Just ResetZonesResponse
ensembleRequestsToResponses (ResetRequest t) = Just (ResetResponse t)
ensembleRequestsToResponses _ = Nothing
