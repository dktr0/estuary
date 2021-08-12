{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Scheduler where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import GHCJS.DOM.EventM
import qualified Data.Fixed as F
import Data.Map as Map
import Control.Monad.IO.Class

import Estuary.Types.Context
import Estuary.Widgets.Editor
import Estuary.Types.Variable
import Estuary.Types.Definition

-- ZonedTime for a LocalTime with a TimeZone
-- date ZonedTime = ZonedTime {
-- zonedTimeToLocalTime :: LocalTime,
-- zonedTimeZone :: TimeZone}

-- type RehearsalTime = (Text, ZonedTime) -- still be able to converted, but also know the timeZone of the last person who changed the time in the widgets.
-- operations:
changeDescription :: Text -> RehearsalTime -> RehearsalTime -- same for the others
changeDescription t (details, dateAndTime) = (t, dateAndTime)

changeDate :: Day -> RehearsalTime -> RehearsalTime -- given a day the func will return the new rehearsalTime --attachWith
changeDate d (details, ZonedTime (LocalTime day timeOfDay) timeZone) = (details, ZonedTime (LocalTime d timeOfDay) timeZone)

changeTimeOfDay :: TimeOfDay -> RehearsalTime -> RehearsalTime
changeTimeOfDay td (details, ZonedTime (LocalTime day timeOfDay) timeZone) = (details, ZonedTime (LocalTime day td) timeZone)

changeTimeZone :: TimeZone -> RehearsalTime ->   RehearsalTime --
changeTimeZone tz (details, ZonedTime (LocalTime day timeOfDay) timeZone) = (details, ZonedTime (LocalTime day timeOfDay) tz)
-- keep track of current value of RehearsalTime and what kin
--data_picker time_picker -> change things of the currently displayed time
-- 3 widgets next to each other, with some component of rehearsalTime time
descriptionWidget ::  MonadWidget t m => Dynamic t Text -> m (Event t Text) -- similar to our text editors ---- displays a text
descriptionWidget t = do
  x <- textArea $ def
  let y = _textArea_value x
  return $ updated y

-- when you click a picker with two modes : 1) displays the day only/ current scheduled day; when you click it shifts to 2) picker so you can select; we will use dateWidgetMode1 and 2.
-- Day =
dateWidget ::  MonadWidget t m => Dynamic t Day -> m (Event t Day)
dateWidget d = do
  dateWidgetMode1 d
  dateWidgetMode2 d

-- only displays; cant change the date
dateWidgetMode1 ::  MonadWidget t m =>  Dynamic t Day -> m (Event t ())
dateWidgetMode1 d = do
  let d' = fmap (T.pack . show) d
  (openPicker, _) <- el' "div" $ dynText d'
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t RehearsalTime)
  return openPicker'

--utctDay :: UTCTime -> Day -- 2021-08-12
-- (close event, pick day) or when people press okay -> only changes the server when you confirm it.
dateWidgetMode2 :: MonadWidget t m =>  Dynamic t Day -> m (Event t Day)
dateWidgetMode2 d  = do
  (confirmDate, _) <- el' "div" $ text "confirm"
  confirmDateEv <- wrapDomEvent (_element_raw confirmDate) (elementOnEventName Click) (mouseXY)
  let confirmDate' = tagPromptlyDyn d confirmDateEv -- Dynamic a -> Event b -> Event a
  return confirmDate'

-- fromGregorian year month day -- Day
-- f :: -- Dynamic t Day
-- f  = fromGregorian <$> (fst dateInput ) <*> (snd dateInput) <*> (trd dateInput)--

dateInput ::  MonadWidget t m => m (Dynamic t Integer, Dynamic t Int, Dynamic t Int)
dateInput = do
  -- do
  --  el "tr" $ do
  --    el "th" $ do text "Su"
  --    el "th" $ text "Mo"
  --    el "th" $ text "Tu"
  --    el "th" $ text "We"
  --    el "th" $ text "Th"
  --    el "th" $ text "Fr"
  --    el "th" $ text "Sa"
  --  -- row1 <- el "tr" $ do
  -- d <- holdDyn 1 (dateEv 1) -- Dynamic t Int
  return $ (constDyn 2021, constDyn 04, (constDyn 22))


dateEv :: MonadWidget t m => Int -> m (Event t Int)
dateEv selectedDay = do
    (pickDate, _) <- el' "div" $ text $ T.pack $ show selectedDay
    pickDateEv <- wrapDomEvent (_element_raw pickDate) (elementOnEventName Click) (mouseXY)
    let pickDate' = selectedDay <$ pickDateEv -- (Event t (Dynamic t RehearsalTime)
    return pickDate'

-- timeWidget ::  MonadWidget t m => Dynamic t (TimeOfDay, TimeZone) -> m (Event t (TimeOfDay, TimeZone))

-- zonedTimeToTimeOfDayAndTimeZone :: ZonedTime -> (TimeOfDay, TimeZone)
-- keep track of provided day and zone and when the user changes them, it will produce events.
-- text boxes for now.
-- the time zone; hard code our time zone -> The initial time zone is what it is, this widget will just display the UTC (and come back to the timezone picker? later). This widget displays the hours and minutes and if they are valid (i.e. only numbers).

rehearsalTimeWidget :: MonadWidget t m => Dynamic t RehearsalTime -> Editor t m (Variable t RehearsalTime)
rehearsalTimeWidget delta = mdo
  descEv <- descriptionWidget $ fmap fst delta -- Event t Text, representing a local edit
  dateEv <- dateWidget $ fmap (localDay . zonedTimeToLocalTime . snd) delta -- Event t Day
  -- timeEv <- timeWidget $ fmap (zonedTimeToTimeOfDayAndTimeZone . snd) delta -- Event t (TimeOfDay, TimeZone)
  let descF = fmap changeDescription descEv -- Event t (RehearsalTime -> RehearsalTime)
  let dateF = fmap changeDate dateEv-- Event t (RehearsalTime -> RehearsalTime)
  -- let timeF = fmap changeTimeZone dateEv -- Event t (RehearsalTime -> RehearsalTime)
  let localF = mergeWith (.) [descF, dateF] -- Event t (RehearsalTime -> RehearsalTime)--
  let localUpdates = attachWith (flip ($)) (current $ currentValue v) localF
  v <- variable delta localUpdates
  return v


-- test functions
--test (Text, UTCTIme)
-- scheduleRehearsalTest :: (Text, UTCTime)
-- scheduleRehearsalTest = do
--   let utcT = convertDateAndTimeToUTCTime (2021, 04, 27) (15, 30, 0.5)
--   ("My next rehearsal", utcT)
--
--
-- scheduleRehearsalTest2 :: (Text, UTCTime)
-- scheduleRehearsalTest2 = do
--   let utcT = convertDateAndTimeToUTCTime (2022, 06, 30) (10, 25, 00)
--   ("My next rehearsal", utcT)

-- data UTCTime = UTCTime
--   { utctDay     :: Day       -- calendar day
--   , utctDayTime :: DiffTime  -- seconds from midnight
--   }
--
-- data TimeOfDay = TimeOfDay
--   { todHour :: Int
--   , todMin  :: Int
--   , todSec  :: Pico
--   }

-- myDatetoDay = fromGregorian
-- (timeOfDayToTime  (TimeOfDay 15 20 10)) :: DiffTime
