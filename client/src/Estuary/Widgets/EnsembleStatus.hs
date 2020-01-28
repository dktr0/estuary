{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.EnsembleStatus where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import Control.Monad.Trans (liftIO)
import Data.Map.Strict

import Estuary.Types.Context
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.Participant
import Estuary.Widgets.Editor

ensembleStatusWidget :: MonadWidget t m => Editor t m ()
ensembleStatusWidget = do

  -- extract data about ensemble from the context, filtering out all duplicate events
  -- TODO: still need to add filtering of duplicate events...
  ctx <- askContext
  let ensC = fmap ensembleC ctx
  let ens = fmap ensemble ensC
  let ensName = fmap ensembleName ens -- Dynamic t Text
  let ensParticipants = fmap participants ens -- Dynamic t (Map.Map Text Participant)
  let status = fmap wsStatus ctx --ensemble status
  let anonymous = fmap anonymousParticipants ens -- Dynamic t Int

  -- display name of ensemble
  liftR $ divClass "ensemble-name code-font" $ do
    text "Ensemble"
    dynText ensName

    -- display list of non-anonymous participants
  liftR2 (divClass "statusTableWrapper") $ do
      liftR $ divClass "statusElementWrapper code-font" $ do
         divClass "statusElementName" $ text "Name"
         divClass "statusElement" $ listWithKey ensParticipants participantNameWidget

    -- display list of locations for non-anonymous participants
      liftR $ divClass "statusElementWrapper code-font" $ do
         divClass "statusElementName" $ text "Location"
         divClass "statusElement" $ listWithKey ensParticipants participantLocationWidget

    -- display the status of the ensParticipants
      liftR $ divClass "statusElementWrapper code-font" $ do
         divClass "statusElementName" $ text "Status"
         divClass "statusElement" $ listWithKey ensParticipants participantStatusWidget

  -- display the activity of the ensParticipants
      liftR $ divClass "statusElementWrapper code-font" $ do
        divClass "statusElementName" $ text "Activity"
        mapActivities <- pollParticipantActivity ensParticipants -- :: Dynamic t Map Text  Text
        listWithKey mapActivities participantActivityWidget -- m (Dynamic t (Map k a))
        --where the first Texte is the name of particp and the 2nd Text is the

      -- display count of anonymous participants
  liftR2 (divClass "statusElementsWrapper") $ do
      liftR $ divClass "statusElementWrapper code-font" $ do
        divClass "statusElementName" $ do
          text "Anonymous Participants: "
          dynText $ fmap showt anonymous

  return ()

ensembleParticipantWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
ensembleParticipantWidget name part = elClass "div" "" $ do
  text name
  text " ("
  dynText $ fmap location part
  text ") "

participantNameWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameWidget name part = elClass "div" "" $ do
  text name

participantLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLocationWidget name part = elClass "div" "" $ do
  dynText $ fmap location part

participantStatusWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantStatusWidget name part = divClass "" $ do
  dynText $ fmap status part -- from where is this status going to be settable?

participantActivityWidget :: MonadWidget t m => Text -> Dynamic t Text -> m ()
participantActivityWidget name part = divClass "" $ dynText part

pollParticipantActivity :: MonadWidget t m => Dynamic t (Map Text Participant) ->  m (Dynamic t (Map Text Text))  -- :: Dynamic t Map Text  Text
pollParticipantActivity ensParticipants = do
  now <- liftIO getCurrentTime -- this time is measured before building the widget
  evTick <- tickLossy 10.13 now  -- m (Event t TickInfo)
  currTime <- performEvent $ fmap (\_ -> liftIO getCurrentTime) evTick
  let ev = attachWith generateActivityMessages (current ensParticipants) currTime
  ip <- sample $ current ensParticipants
  let im = generateActivityMessages ip now
  holdDyn im ev

generateActivityMessages :: Map Text Participant -> UTCTime -> Map Text Text
generateActivityMessages m t = fmap (flip generateActivityMessage $ t) m

generateActivityMessage :: Participant -> UTCTime -> Text
generateActivityMessage p t = f (diffUTCTime t (lastEdit p))
  where
    f x | x < 60 = "< 1m"
        | x < 120 = "< 2m"
        | x < 180 = "< 3m"
        | x < 240 = "< 4m"
        | x < 300 = "< 5m"
        | otherwise = "inactive"
