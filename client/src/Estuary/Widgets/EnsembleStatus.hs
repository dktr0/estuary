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
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term


ensembleStatusWidget :: MonadWidget t m => Editor t m (Event t EnsembleRequest)
ensembleStatusWidget = do

  -- extract data about ensemble from the context, filtering out all duplicate events
  ctx <- context
  let ensC = fmap ensembleC ctx
  let ens = fmap ensemble ensC
  let uHandle = fmap userHandle ensC -- Dynamic Text
  ensName <- holdUniqDyn $ fmap ensembleName ens -- Dynamic t Text
  let ensParticipants = fmap participants ens -- Dynamic t (Map.Map Text Participant)
  let status = fmap wsStatus ctx --ensemble status
  anonymous <- holdUniqDyn $ fmap anonymousParticipants ens -- Dynamic t Int

  divClass "ensemble-name code-font" $ do
    term Term.Ensemble >>= dynText
    text ":"
    dynText ensName

  divClass "tableContainer code-font" $ do
    status <- el "table" $ do
      now <- liftIO getCurrentTime -- this time is measured before building the widget
      evTick <- tickLossy 10.13 now  -- m (Event t TickInfo)
      currentTime <- performEvent $ fmap (\_ -> liftIO getCurrentTime) evTick
      x <- listWithKey ensParticipants (row uHandle currentTime)
      return $ switchDyn $ fmap (leftmost . elems) x --Event t EnsembleRequest

    divClass "statusWidgetAnonymousPart code-font" $ do
      term Term.AnonymousParticipants >>= dynText
      text ": "
      dynText $ fmap showt anonymous

    return status

-- -- help files for samples
-- functionRef :: MonadWidget t m => Text -> m ()
-- functionRef x = divClass "helpWrapper" $ do
-- switchToReference <- buttonWithClass' x
-- exampleVisible <- toggle True switchToReference
-- referenceVisible <- toggle False switchToReference
-- hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
-- hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
-- return ()
-- --hideableWidget :: MonadWidget t m => Dynamic t Bool -> Text -> m a -> m a


row ::  MonadWidget t m  => Dynamic t Text -> Event t UTCTime -> Text -> Dynamic t Participant ->  m (Event t EnsembleRequest)
row uHandle t name part = do
  row <- el "tr" $ do
    elClass "td" "statusWidgetNameAndLocation" $ participantNameAndLocationWidget name part
    status <- elClass "td" "statusWidgetStatusInput" $ participantStatusWidget uHandle name part
    elClass "td" "statusWidgetActivity" $ participantActivityWidget t name part
    evClick <- buttonForCollapsable
    dynBool <- toggle False evClick
    let dynAttrs = collapseFPSandLatency <$> dynBool
    elDynAttr "td" dynAttrs $ participantFPSWidget name part
    elClass "td" "test" $ participantLatencyWidget name part
    return status
  return (row)

buttonForCollapsable :: MonadWidget t m =>  m (Event t ())
buttonForCollapsable = do
  rec
    let dynButtonLabel = fmap buttonLabel evClick
    button <- dynButton dynButtonLabel
    evClick <- toggle True button
  return button

buttonLabel :: Bool -> Text
buttonLabel b = label b
  where
    label True = "+"
    label False = "-"

collapseFPSandLatency :: Bool -> Map Text Text
collapseFPSandLatency b = "style" =: (display b)
  where
    display True  = ""
    display False = "display: none;"

-- participantStatusWidget :: MonadWidget t m  => Text -> Text -> Dynamic t Participant -> m (Event t EnsembleRequest)
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
participantNameAndLocationWidget name part = do
  dynText $ constDyn name <> fmap location' part

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

participantFPSWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantFPSWidget name part = do
  let a = fmap (showt . animationLoad) part
  dynText $ a <> (constDyn "FPS")

participantLatencyWidget :: MonadWidget t m => Text -> Dynamic t Participant -> m ()
participantLatencyWidget name part = elClass "div" "test2" $ do
  let a = fmap (T.pack . show . floor . realToFrac . (*) 1000 . latency) part
  dynText $ a <> (constDyn "ms")

participantActivityWidget :: MonadWidget t m => Event t UTCTime -> Text -> Dynamic t Participant -> m ()
participantActivityWidget t name part = divClass "" $ do
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
