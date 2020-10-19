{-# LANGUAGE OverloadedStrings #-}

-- The type EnsembleC represents the state of an Ensemble from the perspective
-- of the Estuary client (ie. the widgets/UI)

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
import Reflex

import Estuary.Types.EnsembleEvent
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.View.Parser
import Estuary.Types.View.Presets
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Tempo
import Estuary.Types.Hint
import Estuary.Types.Tempo
import Estuary.Types.Participant
import Estuary.Types.Chat

-- each field of the EnsembleC record is Dynamic t a, so that any of them can
-- change independently without triggering computation related to the others.

data EnsembleC t = EnsembleC {
  ensembleName :: Dynamic t Text,
  userHandle :: Dynamic t Text, -- how the user appears to others in the ensemble; "" == anonymous
  location :: Dynamic t Text, -- the user's location (cached for re-authentication scenarios)
  password :: Dynamic t Text, -- the participant password (cached for re-authentication scenarios)
  tempo :: Dynamic t Tempo,
  zones :: Dynamic t (IntMap.IntMap Definition), -- refactor later to make individual zones Dynamic
  views :: Dynamic t (Map.Map Text View),
  chats :: Dynamic t [Chat],
  view :: Dynamic t (Either View Text), -- Rights are from preset views, Lefts are local views
  ensembleAudioMap :: Dynamic t AudioMap,
  participants :: Dynamic t (Map.Map Text Participant),
  anonymousParticipants :: Dynamic t Int
}

eventsToEnsembleC :: MonadWidget t m => Event t EnsembleEvent -> m EnsembleC
eventsToEnsembleC rqs = do
  ensembleName' <- holdDyn "" $ fmapMaybe ensembleNameF rqs
  userHandle' <- holdDyn "" $ fmapMaybe userHandleF rqs
  location' <- holdDyn "" $ fmapMaybe locationF rqs
  password' <- holdDyn "" $ fmapMaybe passwordF rqs
  tempo' <- holdDyn (Tempo { time=t, count=0.0, freq=0.5 }) $ fmapMaybe tempoF rqs
  zones' <- foldDyn (.) IntMap.empty $ fmapMaybe zonesF rqs
  views' <- foldDyn (.) Map.empty $ fmapMaybe viewsF rqs
  chats' <- foldDyn (.) [] $ fmapMaybe chatsF rqs
  view' <- holdDyn (Right "default") $ fmapMaybe viewF rqs
  ensembleAudioMapIO <- performEvent $ fmap liftIO $ fmapMaybe ensembleAudioMapF rqs
  ensembleAudioMap' <- foldDyn (.) Map.empty $ ensembleAudioMapIO
  participants' <- foldDyn (.) Map.empty $ fmapMaybe participantsF rqs
  anonymousParticipants' <- holdDyn 0 $ fmapMaybe anonymousParticipantsF rqs
  return $ EnsembleC {
    ensembleName = ensembleName',
    userHandle = userHandle',
    location = location',
    password = password',
    tempo = tempo',
    zones = zones',
    views = views',
    chats = chats',
    view = view',
    ensembleAudioMap = ensembleAudioMap',
    participants = participants',
    anonymousParticipants = anonymousParticipants'
  }

ensembleNameF :: EnsembleEvent -> Maybe Text
ensembleNameF (JoinEvent x _ _ _) = Just x
ensembleNameF LeaveEvent = Just ""
ensembleNameF _ = Nothing

userHandleF :: EnsembleEvent -> Maybe Text
userHandleF (JoinEvent _ x _ _) = Just x
userHandleF LeaveEvent = Just ""
userHandleF _ = Nothing

locationF :: EnsembleEvent -> Maybe Text
locationF (JoinEvent _ _ x _) = Just x
locationF LeaveEvent = Just ""
locationF _ = Nothing

passwordF  :: EnsembleEvent -> Maybe Text
passwordF (JoinEvent _ _ _ x) = Just x
passwordF LeaveEvent = Just ""
passwordF _ = Nothing

tempoF :: EnsembleEvent -> Maybe Tempo
tempoF (TempoEvent x) = Just x
tempoF _ = Nothing

zonesF :: EnsembleEvent -> Maybe (IntMap.IntMap Definition -> IntMap.IntMap Definition)
zonesF (ZoneEvent k v) = Just (IntMap.insert k v)
zonesF ClearZones = Just (const $ IntMap.empty)
zonesF LeaveEvent = Just (const $ IntMap.empty)
zonesF _ = Nothing

viewsF :: EnsembleEvent -> Maybe (Map.Map Text View -> Map.Map Text View)
viewsF (ViewsEvent k v) = Just (Map.insert k v)
viewsF LeaveEvent = Just (const $ Map.empty)
viewsF _ = Nothing

chatsF :: EnsembleEvent -> Maybe ([Chat] -> [Chat])
chatsF (ChatEvent x) = Just (\y -> y ++ [x])
chatsF LeaveEvent = Just (const [])
chatsF _ = Nothing

viewF :: EnsembleEvent -> Maybe (Either View Text)
viewF (ViewEvent x) = Just x
viewF _ = Nothing

ensembleAudioMapF :: EnsembleEvent -> Maybe (IO (AudioMap -> AudioMap))
ensembleAudioMapF (InsertAudioResource url bankName n) = Just $ return $ Map.insert (bankName,n) url
ensembleAudioMapF (DeleteAudioResource bankName n) = Just $ return $ Map.delete (bankName,n)
ensembleAudioMapF (AppendAudioResource url bankName) = Just $ do
  aMeta <- audioResourceFromMeta $ AudioMeta url 0
  return $ ResourceMap.append bankName aMeta
ensembleAudioMapF _ = Nothing

participantsF :: EnsembleEvent -> Maybe (Map.Map Text Participant -> Map.Map Text Participant)
participantsF (ParticipantJoins p) = Just $ Map.insert (name p) p
participantsF (ParticipantUpdate p) = Just $ Map.insert (name p) p
participantsF (ParticipantLeaves n) = Just $ Map.delete n
participantsF _ = Nothing

anonymousParticipantsF :: EnsembleEvent -> Maybe Int
anonymousParticipantsF (AnonymousParticipants n) = Just n
anonymousParticipantsF _ = Nothing


-- *** WORKING BELOW HERE figuring out what stays, what goes, what gets refactored...

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
