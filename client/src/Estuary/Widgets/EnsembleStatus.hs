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
  ctx <- askContext
  let ensC = fmap ensembleC ctx
  let ens = fmap ensemble ensC
  let ensName = fmap ensembleName ens -- Dynamic t Text
  let ensParticipants = fmap participants ens -- Dynamic t (Map.Map Text Participant)
  let status = fmap wsStatus ctx --ensemble status
  let anonymous = fmap anonymousParticipants ens -- Dynamic t Int

  liftR $ divClass "ensemble-name code-font" $ do
    text "Ensemble: "
    dynText ensName

  liftR2 (divClass "tableContainer") $ do
    status <- liftR2 (el "table") $ do
        liftR $ do
          -- get indiviual handler and pass it down as a pure value
          now <- liftIO getCurrentTime -- this time is measured before building the widget
          evTick <- tickLossy 10.13 now  -- m (Event t TickInfo)
          currentTime <- performEvent $ fmap (\_ -> liftIO getCurrentTime) evTick
          x <- listWithKey ensParticipants (row currentTime)
          let statusEvent = switchDyn $ fmap (leftmost . elems) x --Event t EnsembleRequest
          return statusEvent

    liftR $ divClass "statusWidgetAnonymousPart" $ do
      text "Anonymous Participants: "
      dynText $ fmap showt anonymous

    return status

row ::  MonadWidget t m  => Event t UTCTime -> Text -> Dynamic t Participant ->  m (Event t EnsembleRequest)
row t name part = do
  row <- el "tr" $ do
    elClass "td" "statusWidgetNameAndLocation" $ participantNameAndLocationWidget name part
    status <- elClass "td" "statusWidgetStatusInput" $ participantStatusWidget name part
    elClass "td" "statusWidgetActivity" $ participantActivityWidget t name part
    elClass "td" "statusWidgetFPS" $ participantFPSWidget name part
    elClass "td" "statusWidgetLatency" $ participantLatencyWidget name part
    return status
  return (row)

-- participantStatusWidget :: MonadWidget t m  => Text -> Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
participantStatusWidget :: MonadWidget t m  => Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
-- participantStatusWidget thisUserhandle _ part = do
participantStatusWidget _ part = do
  initialStatus <- status <$> sample (current part)
  updatedStatus <- fmap updated $ holdUniqDyn $ fmap status part -- event issued only when status changes
  -- style <- -- dynamic style -- compare their handle in their ensemble and this user, current user's handle is in the initial value context (do this function somewhere else)
  s <- textInput $ def & textInputConfig_setValue .~ updatedStatus & textInputConfig_initialValue .~ initialStatus & attributes .~ constDyn ("class" =: "code-font")
  -- ("style" =: ".avoid-clicks { pointer-events: none;}")
  let writeStatusToServer = fmap (\x -> WriteStatus x) $ _textInput_input s --msg only sent when they press a key
  return writeStatusToServer

participantNameAndLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameAndLocationWidget name part = do
  x <- holdUniqDyn $ constDyn name <> fmap location' part
  dynText x

location' :: Participant -> Text
location' p = f (location p)
  where
    f x | x == "" = x
        | otherwise = "@" <> x

participantFPSWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantFPSWidget name part = do
  a <- holdUniqDyn $ fmap (showt . animationFPS) part
  dynText $ a <> (constDyn "FPS")

participantLatencyWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLatencyWidget name part = elClass "div" "" $ do
  a <- holdUniqDyn $ fmap (T.pack . show . floor . realToFrac . (*) 1000 . latency) part
  dynText $ a <> (constDyn "ms")

participantActivityWidget :: MonadWidget t m => Event t UTCTime -> Text -> Dynamic t Participant -> m ()
participantActivityWidget t name part = divClass "" $ pollParticipantActivity t part >>= holdUniqDyn >>= dynText

pollParticipantActivity :: MonadWidget t m => Event t UTCTime -> Dynamic t Participant -> m (Dynamic t Text)
pollParticipantActivity e part = do
  now <- liftIO getCurrentTime
  iv <- fmap (\x -> generateActivityMessage x now) $ sample $ current part -- initial v of part
  let x = attachWith generateActivityMessage (current part) e -- :: Event t Text
  holdDyn iv x -- :: event a -> m (Dyn a)

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
