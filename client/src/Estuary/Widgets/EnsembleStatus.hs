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

    -- display list of locations for known participants
       liftR $ el "tr" $ do
          el "td" $ listWithKey ensParticipants participantAndLocationWidget
          mapActivities <- pollParticipantActivity ensParticipants -- :: Dynamic t Map Text  Text
          el "td" $ listWithKey mapActivities participantActivityWidget -- m (Dynamic t (Map k a))
          el "td" $ listWithKey ensParticipants participantFPSWidget
          el "td" $ listWithKey ensParticipants participantLatencyWidget
          statusMonster <- elClass "td" "participantStatusInput" $ listWithKey ensParticipants participantStatusWidget
          let statusEvent = switchDyn $ fmap (leftmost . elems) statusMonster --Event t EnsembleRequest
          return statusEvent
  --
  -- liftR2 (divClass "statusElementsWrapper") $ do
  --     liftR $ divClass "statusElementWrapper code-font" $ do
  --       divClass "statusElementName" $ do
  --         text "Anonymous Participants: "
  --         dynText $ fmap showt anonymous


participantStatusWidget :: MonadWidget t m  => Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
participantStatusWidget _ part = do
  initialStatus <- sample (current part)
  -- initialStatus <- sample <$> status (current part)  -- Text -- reflex widget
  let updatedStatus = fmap status (updated part)  --Event t Participant -> Event t Text -- and event that changes the text
  s <- textInput $ def & textInputConfig_setValue .~ updatedStatus & textInputConfig_initialValue .~ ""
  let writeStatusToServer = fmap (\x -> WriteStatus x) $ _textInput_input s --msg only sent when they press a key
  return writeStatusToServer

-- sample :: Behavior a -> m a  ---- Behavior to Behavior by sampling current values
-- current :: Dynamic a -> Behavior a
-- updated :: Dynamic a -> Event a


-- participantStatusWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
-- participantStatusWidget name part = divClass "" $ do
--   -- s <- dynText $ fmap status part -- from where is this status going to be settable?
--   -- t <- textInput $ def & attributes .~ constDyn ( "placeholder" =: "edit me") --"class" =: "statusInputWidget code-font" <>
--   -- let a =  fmap ( T.pack . (++) "luis". T.unpack . status)  part
--     s <- el "div" $ do
--       let attrs = constDyn ("class" =: "background primary-color primary-borders ui-font")
--       liftM _textInput_value $ textInput $ def & textInputConfig_attributes .~ attrs --aqui me devuelve el
--     let a = fmap (flip s . status) part
--     return ()

participantAndLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantAndLocationWidget name part = elClass "div" "" $ do
  text $ name <> " @ "
  dynText $ fmap location part


participantNameWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameWidget name part = elClass "div" "" $ do
  text name

participantLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLocationWidget name part = elClass "div" "" $ dynText $ fmap location part

participantFPSWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantFPSWidget name part = elClass "div" "" $ do
  let a = fmap (showt . animationLoad) part
  dynText a
  text "FPS "

participantLatencyWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLatencyWidget name part = elClass "div" "" $ do
  let a = fmap (T.pack. show . latency) part
  dynText a






participantActivityWidget :: MonadWidget t m => Text -> Dynamic t Text -> m ()
participantActivityWidget name part = divClass "" $ dynText part

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
generateActivityMessage p t = f (diffUTCTime t (lastEdit p))
  where
    f x | x < 60 = "< 1m"
        | x < 120 = "< 2m"
        | x < 180 = "< 3m"
        | x < 240 = "< 4m"
        | x < 300 = "< 5m"
        | otherwise = "inact."
