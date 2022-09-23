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

import Estuary.Types.Response as Response
import Estuary.Types.Request as Request
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
import Estuary.Types.LogEntry

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
requestToEnsembleC _ Request.LeaveEnsemble e = pure $ leaveEnsembleC e
requestToEnsembleC _ (Request.DeleteThisEnsemble _) e = pure $ leaveEnsembleC e
requestToEnsembleC _ (Request.WriteTempo x) e = pure $ modifyEnsemble (writeTempo x) e
requestToEnsembleC _ (Request.WriteZone n v) e = pure $ modifyEnsemble (writeZone n v) e
requestToEnsembleC _ (Request.WriteView t v) e = pure $ modifyEnsemble (writeView t v) e
requestToEnsembleC rs (Request.WriteResourceOps x) e = do
  setResourceOps rs x
  pure $ modifyEnsemble (\y -> y { resourceOps = x } ) e
requestToEnsembleC _ Request.ResetZones e = pure $ modifyEnsemble (\e -> e { zones = IntMap.empty } ) e
requestToEnsembleC _ Request.ResetViews e = pure $ modifyEnsemble (\e -> e { views = Map.empty } ) $ selectPresetView "def" e
requestToEnsembleC _ (Request.Reset t) e = pure $ modifyEnsemble (\e -> e { zones = IntMap.empty }) $ modifyEnsemble (writeTempo t) e
requestToEnsembleC _ _ e = pure e


-- some responses from the server are mapped into one or more hints, for the purpose
-- of updating rendering, updating ensembleC, or printing log messages
-- (the resulting requests are not re-sent to the server, of course)
responseToHints :: Response -> [Hint]
responseToHints (Response.OK m) = pure $ LogMessage $ english m
responseToHints (Response.Error e) = pure $ LogMessage $ english $ "error: " <> e
responseToHints (Response.WriteZone n d) = pure $ Request $ Request.WriteZone n d
responseToHints (Response.LogEntry (LogChat c)) = pure $ LogMessage $ english $ showChatMessage c
responseToHints (Response.LogEntry (EnsembleEvent t txt)) = pure $ LogMessage $ english "placeholder (EnsembleEvent not handled in responseToHints)"
responseToHints (Response.WriteView n v) = pure $ Request $ Request.WriteView n v
responseToHints (Response.WriteTempo t) = [
  Request $ Request.WriteTempo t,
  LogMessage $ english "received new tempo"
  ]
responseToHints (Response.ParticipantLeaves n) = pure $ LogMessage $ english $ n <> " has left the ensemble"
responseToHints (Response.AnonymousParticipants n) = pure $ LogMessage $ english $ showt n <>  " anonymous participants"
responseToHints (Response.WriteResourceOps x) = pure $ Request $ Request.WriteResourceOps x
responseToHints Response.ResetZones = [
  Request $ Request.ResetZones,
  LogMessage $ english "received ResetZones"
  ]
responseToHints Response.ResetViews = [
  Request $ Request.ResetViews,
  LogMessage $ english "received ResetViews"
  ]
responseToHints (Response.Reset t) = [
  Request $ Request.Reset t,
  LogMessage $ english "received Reset (resetting zones and tempo)"
  ]
responseToHints _ = []


-- while other responses from the server are mapped directly onto EnsembleC state changes
-- (because their semantics are not represented by the Request type)
responseToEnsembleC :: Response -> EnsembleC -> EnsembleC
responseToEnsembleC (JoinedEnsemble eName uName loc pwd) e = joinEnsembleC eName uName loc pwd e
responseToEnsembleC (ParticipantUpdate x) e = modifyEnsemble (writeParticipant (name x) x) e
responseToEnsembleC (ParticipantLeaves n) e = modifyEnsemble (deleteParticipant n) e
responseToEnsembleC (AnonymousParticipants n) e = modifyEnsemble (writeAnonymousParticipants n) e
responseToEnsembleC _ e = e
