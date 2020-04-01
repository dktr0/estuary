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
import Control.Monad

import Estuary.Types.Context
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleRequest
import Estuary.Types.Participant
import Estuary.Widgets.Editor


ensembleStatusWidget :: MonadWidget t m => Editor t m (Event t EnsembleRequest)
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
    text "Ensemble: "
    dynText ensName

    -- display list of known participants
  liftR2 (divClass "tableContainer") $ do
    liftR2 (el "table") $ do
       liftR $ el "tr" $ do
          el "th" $ text ""
          el "th" $ text ""
          el "th" $ text ""
          el "th" $ text ""
          el "th" $ text ""

    -- display list of locations for known participants
       liftR $ el "tr"  $ do
          elClass "td"  "statusWidgetNameAndLocation" $ listWithKey ensParticipants participantAndLocationWidget
          statusMonster <- elClass "td" "statusWidgetStatusInput" $ listWithKey ensParticipants participantStatusWidget
          let statusEvent = switchDyn $ fmap (leftmost . elems) statusMonster --Event t EnsembleRequest
          mapActivities <- pollParticipantActivity ensParticipants -- :: Dynamic t Map Text  Text
          elClass "td" "statusWidgetActivity" $ listWithKey mapActivities participantActivityWidget -- m (Dynamic t (Map k a))
          elClass "td" "statusWidgetFPS" $ listWithKey ensParticipants participantFPSWidget
          elClass "td" "statusWidgetLatency" $ listWithKey ensParticipants participantLatencyWidget
          return statusEvent
  --
  -- liftR2 (divClass "statusElementsWrapper") $ do
  --     liftR $ divClass "statusElementWrapper code-font" $ do
  --       divClass "statusElementName" $ do
  --         text "Anonymous Participants: "
  --         dynText $ fmap showt anonymous


participantStatusWidget :: MonadWidget t m  => Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
participantStatusWidget _ part = do
  initialStatus <- status <$> sample (current part)
  let updatedStatus = fmap status (updated part)  --Event t Participant -> Event t Text -- and event that changes the text
  s <- textInput $ def & textInputConfig_setValue .~ updatedStatus & textInputConfig_initialValue .~ initialStatus & attributes .~ constDyn ("class" =: "code-font")
  let writeStatusToServer = fmap (\x -> WriteStatus x) $ _textInput_input s --msg only sent when they press a key
  return writeStatusToServer

participantAndLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantAndLocationWidget name part = do
  text $ name <> " @ "
  dynText $ fmap location part
  el "br" $ blank

participantNameWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameWidget name part = text name

participantLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLocationWidget name part = dynText $ fmap location part

participantFPSWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantFPSWidget name part = do
  let a = fmap (showt . animationLoad) part
  dynText a
  text "FPS "

participantLatencyWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLatencyWidget name part = elClass "div" "" $ do
  let a = fmap (T.pack. show . latency) part
  dynText a


participantActivityWidget :: MonadWidget t m => Text -> Dynamic t Text -> m ()
participantActivityWidget name part = divClass "" $ do
  dynText part
  el "br" $ blank


pollParticipantActivity :: MonadWidget t m => Dynamic t (Map Text Participant) ->  m (Dynamic t (Map Text Text))  -- :: Dynamic t Map Text  Text
pollParticipantActivity ensParticipants = do
  now <- liftIO getCurrentTime -- this time is measured before building the widget
  evTick <- tickLossy 10.13 now  -- m (Event t TickInfo)
  currentTime <- performEvent $ fmap (\_ -> liftIO getCurrentTime) evTick
  let ev = attachWith generateActivityMessages (current ensParticipants) currentTime
  ip <- sample $ current ensParticipants
  let im = generateActivityMessages ip now
  holdDyn im ev

generateActivityMessages :: Map Text Participant -> UTCTime -> Map Text Text
generateActivityMessages m t = fmap (flip generateActivityMessage $ t) m

generateActivityMessage :: Participant -> UTCTime -> Text
generateActivityMessage p t = do
  f (diffUTCTime t (lastEdit p))
  where
    f x | x < 60 = "<1m"
        | x < 120 = "<2m"
        | x < 180 = "<3m"
        | x < 240 = "<4m"
        | x < 300 = "<5m"
        | otherwise = "inact."
