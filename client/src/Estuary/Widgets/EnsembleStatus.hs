{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE RecursiveDo #-}


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
import Estuary.Widgets.Generic
import qualified Estuary.Types.Term as Term


ensembleStatusWidget :: MonadWidget t m => Editor t m (Event t EnsembleRequest)
ensembleStatusWidget = divClass "ensembleStatusWidget" $ do

  ctx <- context
  let ensC = fmap ensembleC ctx
  let ens = fmap ensemble ensC
  let uHandle = fmap userHandle ensC -- Dynamic Text
  ensName <- holdUniqDyn $ fmap ensembleName ens -- Dynamic t Text
  let ensParticipants = fmap participants ens -- Dynamic t (Map.Map Text Participant)
  let status = fmap wsStatus ctx --ensemble status
  anonymous <- holdUniqDyn $ fmap anonymousParticipants ens -- Dynamic t Int

  divClass "ensemble-name-container code-font" $ do
    divClass "ensemble-name" $ do
      term Term.Ensemble >>= dynText
      text ": "
      dynText ensName

  divClass "infoContainer" $ do
    status <- divClass "tableContainer code-font" $ do
      status' <- el "table" $ do
        now <- liftIO getCurrentTime -- this time is measured before building the widget
        evTick <- tickLossy 10.13 now  -- m (Event t TickInfo)
        currentTime <- performEvent $ fmap (\_ -> liftIO getCurrentTime) evTick
        x <- listWithKey ensParticipants  (row uHandle currentTime)
        return $ switchDyn $ fmap (leftmost . elems) x --Event t EnsembleRequest

      rec
        evClick <- clickableDiv "tableContainerButtonDiv" $ do
          hideableWidget'' dynBool "infoClass" (listWithKey ensParticipants participantFPSLatencyAndLoad)
        dynBool <- toggle False evClick

      return status'

    divClass "statusWidgetAnonymousPart code-font" $ do
      term Term.AnonymousParticipants >>= dynText
      text ": "
      dynText $ fmap showt anonymous

    return status


row ::  MonadWidget t m  => Dynamic t Text -> Event t UTCTime -> Text -> Dynamic t Participant ->  m (Event t EnsembleRequest)
row uHandle t name part = el "tr" $ do
    elClass "td" "statusWidgetNameAndLocation" $ participantNameAndLocationWidget name part
    status <- elClass "td" "statusWidgetStatusInput" $ participantStatusWidget uHandle name part
    elClass "td" "statusWidgetActivity" $ participantActivityWidget t name part
    return status

participantFPSLatencyAndLoad :: MonadWidget t m => Text ->  Dynamic t Participant -> m ()
participantFPSLatencyAndLoad name part = divClass "statusWidgetFPSAndLatency" $ do
  let latency' = fmap (T.pack . show . floor . realToFrac . (*) 1000 . latency) part
  let load' = fmap (showt . mainLoad) part
  let fps' = fmap (showt . animationLoad) part
  dynText $ latency' <> (constDyn "ms ") <> load' <> (constDyn "% ") <> fps' <>(constDyn "FPS")

participantStatusWidget :: MonadWidget t m  => Dynamic t Text -> Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
participantStatusWidget thisUserHandle _ part = do
  initialStatus <- status <$> sample (current part)
  updatedStatus <- fmap updated $ holdUniqDyn $ fmap status part -- event issued only when status changes
  let dynBool = compareHandles <$> thisUserHandle <*> part
  let dynAttrs = attrs <$> dynBool
  s <- textInput $ def & textInputConfig_setValue .~ updatedStatus & textInputConfig_initialValue .~ initialStatus & attributes .~ dynAttrs
  let writeStatusToServer = fmap (\x -> WriteStatus x) $ _textInput_input s --msg only sent when they press a key
  return writeStatusToServer

participantNameAndLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameAndLocationWidget name part = divClass "pNameLocationAndTooltop" $ do
  let child = dynText $ constDyn name <> fmap location' part
  let popup = dynText $ fmap browser part
  tooltip child popup

compareHandles ::  Text -> Participant -> Bool -- -> Dynamic t Text -> Bool
compareHandles uHandle part = uHandle == (name part)

attrs :: Bool -> Map Text Text
attrs b = "class" =: "code-font" <> "style" =: ("pointer-events: " <> pevents b <> bevents b)
  where
    pevents True  = "auto; "
    pevents False = "none; "
    bevents True = "box-shadow: inset 0 0 3px var(--primary-color); "
    bevents False = ""

location' :: Participant -> Text
location' p = f (location p)
  where
    f x | x == "" = x
        | otherwise = "@" <> x

participantNameWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantNameWidget name part = text name

participantLocationWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLocationWidget name part = dynText $ fmap location part


participantActivityWidget :: MonadWidget t m => Event t UTCTime -> Text -> Dynamic t Participant -> m ()
participantActivityWidget t name part =  do
   x <- pollParticipantActivity t part
   dynText x

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
