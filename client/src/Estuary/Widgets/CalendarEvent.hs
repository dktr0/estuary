{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.CalendarEvent where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import GHCJS.DOM.EventM
import qualified Data.Fixed as F
import Data.Map as Map
import Data.Maybe as Maybe
import Control.Monad.IO.Class
import qualified Data.Char as C


import Estuary.Types.Context
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Widgets.Reflex

  --
  -- currentValue v -- Dynamic t a
  -- localEdits v -- Event t a

-- data CalendarEvent = CalendarEvent Text CalendarTime
-- data CalendarTime = CalendarTime { startingDate :: ZonedTime, recurrence :: Maybe Recurrence }
-- data Recurrence = Recurrence { periodicity :: Periodicity, endDate :: ZonedTime}
-- data Periodicity = Daily | Weekly | Monthly | Yearly
--

-- ZonedTime for a LocalTime with a TimeZone
-- date ZonedTime = ZonedTime {
-- zonedTimeToLocalTime :: LocalTime,
-- zonedTimeZone :: TimeZone}

-- type calendarEvent = (Text, ZonedTime) -- still be able to converted, but also know the timeZone of the last person who changed the time in the widgets.
-- operations:

enableRecurrence :: Bool -> CalendarEvent -> CalendarEvent
enableRecurrence True (CalendarEvent details (CalendarTime startingDate recurrence)) = CalendarEvent details (CalendarTime startingDate (Just (Recurrence Daily startingDate)))
enableRecurrence False (CalendarEvent details (CalendarTime startingDate recurrence)) = (CalendarEvent details (CalendarTime startingDate Nothing))

changePeriodicity :: Periodicity -> CalendarEvent -> CalendarEvent
changePeriodicity newPeriodicity (CalendarEvent details (CalendarTime startingDate Nothing)) = CalendarEvent details (CalendarTime startingDate Nothing)
changePeriodicity newPeriodicity (CalendarEvent details (CalendarTime startingDate (Just (Recurrence periodicity endDate)))) =   CalendarEvent details (CalendarTime startingDate (Just (Recurrence newPeriodicity endDate)))
--
changeEndDate :: Day -> CalendarEvent -> CalendarEvent
changeEndDate newDay (CalendarEvent details (CalendarTime startingDate Nothing)) = CalendarEvent details (CalendarTime startingDate Nothing)
-- (Just (Recurrence Daily (ZonedTime (LocalTime newDay (localTimeOfDay $ zonedTimeToLocalTime startingDate)) (zonedTimeZone  startingDate)))))
changeEndDate newDay (CalendarEvent details (CalendarTime startingDate (Just (Recurrence periodicity (ZonedTime (LocalTime day timeOfDay) timeZone))))) = CalendarEvent details (CalendarTime startingDate (Just (Recurrence periodicity (ZonedTime (LocalTime newDay timeOfDay) timeZone))))

startingDayAutoUpdate :: ZonedTime -> CalendarEvent -> CalendarEvent
startingDayAutoUpdate now (CalendarEvent details (CalendarTime startingDay Nothing)) = CalendarEvent details (CalendarTime startingDay Nothing)
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Just (Recurrence Daily (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let newStartingDay = addDays 1 startingDay
  let nextStartingDate = (ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay)
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) = nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Just (Recurrence Daily (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))

startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Just (Recurrence Weekly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let newStartingDay = addDays 7 startingDay
  let nextStartingDate = (ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay)
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) =  nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Just (Recurrence Weekly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))


  --repeat on same day number as opposed to the same week day :: eg. will always update to the 3rd of the next month
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Just (Recurrence Monthly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let numberOfDaysOfMonth  =  gregorianMonthLength (getYearFromDay startingDay) (getMonthFromDay startingDay)
  let newStartingDay = addDays (fromIntegral numberOfDaysOfMonth) startingDay
  let nextStartingDate = (ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay)
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) = nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Just (Recurrence Monthly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))

startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Just (Recurrence Yearly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let yearLength year | (isLeapYear year) == True = 366
                      | otherwise = 365
  let newStartingDay = addDays (fromIntegral $ yearLength (getYearFromDay startingDay)) startingDay
  let nextStartingDate = (ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay)
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) =  nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Just (Recurrence Yearly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay))))

diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

changeDescription :: Text -> CalendarEvent -> CalendarEvent -- same for the others
changeDescription newDetails (CalendarEvent details calendarTime) = CalendarEvent newDetails calendarTime

changeDate :: Day -> CalendarEvent -> CalendarEvent -- given a day the func will return the new calendarEvent --attachWith
changeDate d (CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) timeZone) recurrence))  = CalendarEvent details  (CalendarTime (ZonedTime (LocalTime d timeOfDay) timeZone) recurrence)

changeTimeOfDay :: TimeOfDay -> CalendarEvent -> CalendarEvent
changeTimeOfDay td (CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) timeZone) recurrence)) = CalendarEvent details (CalendarTime (ZonedTime (LocalTime day td) timeZone) recurrence)

changeTimeZone :: TimeZone -> CalendarEvent -> CalendarEvent --
changeTimeZone tz (CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) timeZone) recurrence)) = CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) tz) recurrence)


descriptionWidget ::  MonadWidget t m => Dynamic t Text -> m (Event t Text) -- similar to our text editors ---- displays a text
descriptionWidget t = divClass "descriptionWidgetInput" $ do
  description <- textInputW t
  return $ description


getYearFromDay :: Day -> Integer
getYearFromDay d = do
  let (year, month, day) = toGregorian d
  year

getMonthFromDay :: Day -> Int
getMonthFromDay d = do
  let (year, month, day) = toGregorian d
  month

getDayFromDay :: Day -> Int
getDayFromDay d = do
  let (year, month, day) = toGregorian d
  day

-- when you click a picker with two modes : 1) displays the day only/ current scheduled day; when you click it shifts to 2) picker so you can select; we will use dateWidgetMode1 and 2.
-- Day =

timeZoneWidgetEv ::  MonadWidget t m => Event t TimeOfDay -> Dynamic t TimeZone -> m (Event t TimeZone)
timeZoneWidgetEv tEv dynCurrentZone = mdo
  currentZone <- sample $ current dynCurrentZone
  thisClientTimeZone <- liftIO getCurrentTimeZone -- Dyn TimeZone -- new zone
  dynBool <- toggle False tEv
  let zDyn = (utcORutc5 currentZone thisClientTimeZone) <$> dynBool -- Dynamic t TimeZone
  return (updated zDyn)

utcORutc5 ::TimeZone -> TimeZone -> Bool -> TimeZone
utcORutc5 currentTimeZone updatedTz False  = currentTimeZone
utcORutc5 currentTimeZone updatedTz True = updatedTz

showZoneWidget ::  MonadWidget t m => Dynamic t TimeZone -> m ()
showZoneWidget updatedTz = do
  let showZDyn = fmap (T.pack . show) updatedTz
  dynText showZDyn -- <> " " <> "(the local time of the last person who edited the time)"

timeZoneWidget ::  MonadWidget t m => Event t TimeOfDay -> Dynamic t TimeZone -> m (Event t TimeZone)
timeZoneWidget changedTimeEv currentDynTimeZone = mdo
  currentZone <- sample $ current currentDynTimeZone
  timeZoneWidgetEv' <- timeZoneWidgetEv changedTimeEv currentDynTimeZone
  let updatedZoneEvs = leftmost [timeZoneWidgetEv', updated currentDynTimeZone]
  dynTimeZone <- holdDyn currentZone updatedZoneEvs
  showZoneWidget dynTimeZone
  return timeZoneWidgetEv'


enableRecurrenceWidget :: MonadWidget t m => Dynamic t (Maybe Recurrence) -> m (Event t Bool)
enableRecurrenceWidget r = do
  let checkboxStatus = fmap deductBoolFromRecurrence r -- Dynamic t Bool
  cb <- checkboxW checkboxStatus
  return $ cb


deductBoolFromRecurrence :: Maybe Recurrence -> Bool
deductBoolFromRecurrence Nothing = False
deductBoolFromRecurrence (Just r) = True

printRecurrence :: MonadWidget t m => Event t CalendarEvent -> Dynamic t (Maybe Recurrence) -> m ()
printRecurrence calendarEv serverR = do
  let calendarEv' = fmap getRecurrenceFromCalendarEvent calendarEv
  recurrence <- holdDyn Nothing (leftmost [updated serverR, calendarEv'])
  dynText $ fmap (T.pack . show) recurrence

periodicityAndEndDateToRecurrence ::  Maybe ZonedTime -> Maybe Periodicity -> Maybe Recurrence
periodicityAndEndDateToRecurrence Nothing Nothing = Nothing
periodicityAndEndDateToRecurrence Nothing (Just p) = Nothing
periodicityAndEndDateToRecurrence (Just z) Nothing = Nothing
periodicityAndEndDateToRecurrence (Just z) (Just p) = Just (Recurrence p z)

getPeriodicityFromRecurrence :: Maybe Recurrence -> Maybe Periodicity
getPeriodicityFromRecurrence Nothing = Nothing
getPeriodicityFromRecurrence (Just r) = Just (periodicity r)

getPeriodicityFromRecurrence' :: Maybe Recurrence -> Periodicity
getPeriodicityFromRecurrence' Nothing = Daily
getPeriodicityFromRecurrence' (Just r) = periodicity r

getRecurrenceFromCalendarEvent :: CalendarEvent -> Maybe Recurrence
getRecurrenceFromCalendarEvent (CalendarEvent details (CalendarTime startingDate recurrence)) = recurrence

selectPeriodicityWidget :: MonadWidget t m => Dynamic t (Periodicity) -> m (Event t Periodicity)
selectPeriodicityWidget p = mdo
  let p' = fmap periodicityToKey p -- Dynamic Int -- (updated p) -- Event t Int
  dd <- dropdownW periodicities p' -- Event t Int -- $ def & dropdownConfig_setValue .~ currentPeriodicity
  let selItem = fmap lookupPeriodicity dd -- event t Periodicity
  return $ selItem


periodicityToKey :: Periodicity -> Int
periodicityToKey Daily = 1
periodicityToKey Weekly = 2
periodicityToKey Monthly = 3
periodicityToKey Yearly = 4

periodicities :: Map.Map Int Text
periodicities = Map.fromList [(1, "Daily"), (2, "Weekly"), (3, "Monthly"), (4, "Yearly")]

lookupPeriodicity :: Int  -> Periodicity
lookupPeriodicity key = textToPeriodicity $ Maybe.fromJust (Map.lookup key periodicities)

textToPeriodicity :: Text -> Periodicity
textToPeriodicity "Daily" =  Daily
textToPeriodicity "Weekly" = Weekly
textToPeriodicity "Monthly" = Monthly
textToPeriodicity "Yearly" = Yearly

changeEndDateWidget ::  MonadWidget t m => Dynamic t Day -> m (Event t Day)
changeEndDateWidget d = mdo
  let openCloseEvs = leftmost [mode1Ev, () <$ mode2Ev]
  dynBool <- toggle False openCloseEvs
  let mode1Attrs = calendaContainerAttrs <$> dynBool
  let mode2Attrs = dateLabelAttrs <$> dynBool
  mode2Ev <- dateWidgetMode2 mode1Attrs d
  mode1Ev <- elDynAttr "div" mode2Attrs $ dateWidgetMode1 selDate -- m (Event t ())
  today <- (sample $ current d)
  let updatedDay = leftmost [mode2Ev, updated d]
  selDate <- holdDyn today updatedDay -- mode2Ev -- Dynamic t Day
  return mode2Ev

autoUpdateStartingDate :: MonadWidget t m => Dynamic t ZonedTime -> m (Event t ZonedTime)
autoUpdateStartingDate defTime = do
  defTime' <- sample $ current defTime
  let defZonedTimeToUTC = zonedTimeToUTC defTime'
  nowUTC <- liftIO getCurrentTime
  evTick <- tickLossy 1 nowUTC
  let evTime = _tickInfo_lastUTC <$> evTick
  nowUTC' <- holdDyn defZonedTimeToUTC evTime
  let nowUTCtoZonedTime = fmap (utcToZonedTime (zonedTimeZone defTime')) nowUTC' -- ZonedTime
  return $ updated nowUTCtoZonedTime


dateWidget ::  MonadWidget t m => Dynamic t Day -> m (Event t Day)
dateWidget d = mdo
  let openCloseEvs = leftmost [mode1Ev, () <$ mode2Ev]
  dynBool <- toggle False openCloseEvs
  let mode1Attrs = calendaContainerAttrs <$> dynBool
  let mode2Attrs = dateLabelAttrs <$> dynBool
  mode2Ev <- dateWidgetMode2 mode1Attrs d
  mode1Ev <- elDynAttr "div" mode2Attrs $ dateWidgetMode1 selDate -- m (Event t ())
  today <- (sample $ current d)
  let updatedDay = leftmost [mode2Ev, updated d]
  selDate <- holdDyn today updatedDay -- mode2Ev -- Dynamic t Day
  return mode2Ev

-- only displays; cant change the date
dateWidgetMode1 ::  MonadWidget t m =>  Dynamic t Day -> m (Event t ())
dateWidgetMode1 d = mdo
  let d' = fmap (T.pack . show) d
  (openPicker, _) <- elClass' "div" "code-font background selectedDate" $ dynText d'
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t calendarEvent)
  return openPicker'

monthIndex :: Int -> Int
monthIndex i
  | i == (-1) = 11
  |otherwise = i

monthNames :: MonadWidget t m => Dynamic t Int -> m ()
monthNames i = do
  let i' = fmap monthIndex $ fmap ((+) (-1)) i
  let monthName = ((!!) ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]) <$> i' -- Dynamic t Text
  el "tr" $ do
    el "th" $ dynText monthName
  return ()

navMonthCount :: Int -> Int
navMonthCount c
  |mod c 12 == 0 = 12
  |otherwise = mod c 12

selectMonth :: MonadWidget t m => Dynamic t Int -> m (Dynamic t Int)
selectMonth month = divClass "selectYear" $ mdo
  leftArrow <- clickableDivClass' "<" "CalendarLeftArrow" (-1)
  dynMonthName <- monthNames dynMonth -- $ fmap ((+) (-1)) dynMonth
  rightArrow <- clickableDivClass' ">" "CalendarRightArrow" 1
  let a = leftmost [leftArrow, rightArrow] -- Event t Int
  currentMonth <- sample $ current month
  count <- foldDyn (+) (currentMonth :: Int)  a -- Dynamic Int
  let dynMonth = fmap ((flip mod) 12) count -- monthCount <$> month <*> count   -- Dynamic Int
  let dynMonth' = fmap navMonthCount dynMonth -- monthCount <$> month <*> count   -- Dynamic Int
  -- divClass "" $ dynText (fmap (T.pack . show) dynMonth)
  return dynMonth'

selectYear :: MonadWidget t m => Dynamic t Integer -> m (Dynamic t Integer)
selectYear year = divClass "selectMonth" $ mdo
  leftArrow <- clickableDivClass' "<" "CalendarLeftArrow" (-1)
  showDynYear <- divClass "" $ dynText (fmap (T.pack . show) year')
  rightArrow <- clickableDivClass' ">" "CalendarRightArrow" 1
  let a = leftmost [leftArrow, rightArrow] -- Event t Int
  currentYear <- sample $ current year
  year' <- foldDyn (+) (currentYear :: Integer)  a -- Dynamic Int
  -- divClass "" $ dynText (fmap (T.pack . show) dynMonth)
  return year'
--utctDay :: UTCTime -> Day -- 2021-08-12
-- (close event, pick day) or when people press okay -> only changes the server when you confirm it.
dateWidgetMode2 :: MonadWidget t m => Dynamic t (Map Text Text) -> Dynamic t Day -> m (Event t Day)
dateWidgetMode2 dynAttrs dynDate = mdo
  let year = fmap getYearFromDay dynDate
  let month = fmap getMonthFromDay dynDate -- Dyn Int
  let day = fmap getDayFromDay dynDate
  (selectedYear, selectedMonth, selectedDay) <- elDynAttr "div" dynAttrs $ do
    (selectedYear', selectedMonth', firstDayOfSelMonthBool') <- divClass "monthAndYear" $ do
      selectedMonth'' <- selectMonth month
      selectedYear'' <- selectYear year
      let dayToGregorian = fromGregorian <$> selectedYear'' <*> selectedMonth'' <*> (constDyn 1)
      let initialDayOfSelMonth =  dayOfWeek <$> dayToGregorian
      let firstDayOfSelMonthBool = dayOfWeekBool <$> initialDayOfSelMonth
      return (selectedYear'', selectedMonth'', firstDayOfSelMonthBool)

    selectedDay''' <- el "div"  $ do
      selectedDay'' <- elClass "div" "dayNumbers" $ do
        el "div" $ do text "Su"
        el "div" $ text "Mo"
        el "div" $ text "Tu"
        el "div" $ text "We"
        el "div" $ text "Th"
        el "div" $ text "Fr"
        el "div" $ text "Sa"

        sun <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 1) firstDayOfSelMonthBool'
        mon <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 2) firstDayOfSelMonthBool'
        tue <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 3) firstDayOfSelMonthBool'
        wed <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 4) firstDayOfSelMonthBool'
        thu <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 5) firstDayOfSelMonthBool'
        fri <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 6) firstDayOfSelMonthBool'
        sat <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 7) firstDayOfSelMonthBool'

        a <- el "div" $ selDay selectedYear' selectedMonth' 1
        b <- el "div" $ selDay selectedYear' selectedMonth' 2
        c <- el "div" $ selDay selectedYear' selectedMonth' 3
        d <- el "div" $ selDay selectedYear' selectedMonth' 4
        e <- el "div" $ selDay selectedYear' selectedMonth' 5
        f <- el "div" $ selDay selectedYear' selectedMonth' 6
        g <- el "div" $ selDay selectedYear' selectedMonth' 7
        h <- el "div" $ selDay selectedYear' selectedMonth' 8
        i <- el "div" $ selDay selectedYear' selectedMonth' 9
        j <- el "div" $ selDay selectedYear' selectedMonth' 10
        k <- el "div" $ selDay selectedYear' selectedMonth' 11
        l <- el "div" $ selDay selectedYear' selectedMonth' 12
        m <- el "div" $ selDay selectedYear' selectedMonth' 13
        n <- el "div" $ selDay selectedYear' selectedMonth' 14
        o <- el "div" $ selDay selectedYear' selectedMonth' 15
        p <- el "div" $ selDay selectedYear' selectedMonth' 16
        q <- el "div" $ selDay selectedYear' selectedMonth' 17
        r <- el "div" $ selDay selectedYear' selectedMonth' 18
        s <- el "div" $ selDay selectedYear' selectedMonth' 19
        t <- el "div" $ selDay selectedYear' selectedMonth' 20
        u <- el "div" $ selDay selectedYear' selectedMonth' 21
        v <- el "div" $ selDay selectedYear' selectedMonth' 22
        w <- el "div" $ selDay selectedYear' selectedMonth' 23
        x <- el "div" $ selDay selectedYear' selectedMonth' 24
        y <- el "div" $ selDay selectedYear' selectedMonth' 25
        z <- el "div" $ selDay selectedYear' selectedMonth' 26
        a' <- el "div" $ selDay selectedYear' selectedMonth' 27
        b' <- el "div" $ selDay selectedYear' selectedMonth' 28
        c' <- el "div" $ selDay selectedYear' selectedMonth' 29
        d' <- el "div" $ selDay selectedYear' selectedMonth' 30
        e' <- el "div" $ selDay selectedYear' selectedMonth' 31
        return $ leftmost [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a', b', c', d', e'] --  (Event t Int)
      return selectedDay''
    return (selectedYear', selectedMonth', selectedDay''')
  today <- sample $ current day
  selectedDay' <- holdDyn today selectedDay -- Dynamic t a
  let date = fromGregorian <$> selectedYear <*> selectedMonth <*> selectedDay' -- Day

  (confirmDate, closeCalendar)  <- divClass "confirm-close-buttons" $ do
    (confirmDate', _) <- elDynAttr' "div" dynAttrs $ text "confirm"
    (closeCalendar', _) <- elDynAttr' "div" dynAttrs $ text "cancel"
    return (confirmDate', closeCalendar')

  confirmDateEv <- wrapDomEvent (_element_raw confirmDate) (elementOnEventName Click) (mouseXY)
  let confirmDate' = tagPromptlyDyn date confirmDateEv -- Dynamic a -> Event b -> Event a
  currentDate' <- sample $ current dynDate
  currentDate <- holdDyn currentDate' $ leftmost [confirmDate', updated dynDate]
  closeCalendarEv <- wrapDomEvent (_element_raw closeCalendar) (elementOnEventName Click) (mouseXY)
  let closeCalendar' = tagPromptlyDyn currentDate closeCalendarEv -- Dynamic a -> Event b -> Event a
  return $ leftmost [confirmDate', closeCalendar']

selDay :: MonadWidget t m => Dynamic t Integer -> Dynamic t Int -> Int -> m (Event t Int)
selDay year month selectableDay = do
  let dynAttrs = (howManyDaysInTheMonth selectableDay) <$> month <*> year
  let day = T.pack $ show selectableDay
  aEv <- clickableDivDynAttrs day selectableDay dynAttrs
  let selDay = selectableDay <$ aEv
  return selDay

firstDayOfMonth :: MonadWidget t m => Dynamic t Bool -> m ()
firstDayOfMonth bool = do
  let dynAttrs = monthStartsWhen <$> bool
  dayOfWeek' <- elDynAttr "div" dynAttrs $ text ""
  return ()

  -- CSS classes and styles
dayOfWeekBool :: DayOfWeek -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool)
dayOfWeekBool d
  | d == Sunday = (False, False, False, False, False, False, False) -- true is visible, false is hidden
  | d == Monday = (True, False, False, False, False, False, False)
  | d == Tuesday = (True, True, False, False, False, False, False)
  | d == Wednesday = (True, True, True, False, False, False, False)
  | d == Thursday = (True, True, True, True, False, False, False)
  | d == Friday = (True, True, True, True, True, False, False)
  | d == Saturday = (True, True, True, True, True, True, False)

getBoolFromBoolTuple :: Int -> (Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Bool
getBoolFromBoolTuple 1 (b1, b2, b3, b4, b5, b6, b7) = b1
getBoolFromBoolTuple 2 (b1, b2, b3, b4, b5, b6, b7) = b2
getBoolFromBoolTuple 3 (b1, b2, b3, b4, b5, b6, b7) = b3
getBoolFromBoolTuple 4 (b1, b2, b3, b4, b5, b6, b7) = b4
getBoolFromBoolTuple 5 (b1, b2, b3, b4, b5, b6, b7) = b5
getBoolFromBoolTuple 6 (b1, b2, b3, b4, b5, b6, b7) = b6
getBoolFromBoolTuple _ (b1, b2, b3, b4, b5, b6, b7) = b7

monthStartsWhen :: Bool ->  Map.Map T.Text T.Text
monthStartsWhen b = display b
  where
    display True = "class" =: "dateLabel"
    display _    = "class" =: "dateLabel" <> "style" =: "display: none"

weekDaysToNumbers :: DayOfWeek -> Int
weekDaysToNumbers Sunday = 0
weekDaysToNumbers Monday = 1
weekDaysToNumbers Tuesday = 2
weekDaysToNumbers Wednesday = 3
weekDaysToNumbers Thursday = 4
weekDaysToNumbers Friday = 5
weekDaysToNumbers _ = 6

calendarConfirmCloseButtonAttrs :: Bool -> Map.Map T.Text T.Text
calendarConfirmCloseButtonAttrs b = display b
  where
    display True = "class" =: "calendar-confirm-close-buttons"
    display _    = "class" =: "calendar-confirm-close-buttons" <> "style" =: "display: none"


calendaContainerAttrs :: Bool -> Map.Map T.Text T.Text
calendaContainerAttrs b = display b
  where
    display True = "class" =: "calendarContainer code-font background"
    display _    = "class" =: "calendarContainer  code-font background" <> "style" =: "display: none"

dateLabelAttrs :: Bool -> Map.Map T.Text T.Text
dateLabelAttrs b = "class" =: "dateLabel" <> "style" =: ("visibility: " <> visibility b)
  where
    visibility True = "hidden"
    visibility _    = "visible"

howManyDaysInTheMonth :: Int -> Int -> Integer -> Map.Map T.Text T.Text
howManyDaysInTheMonth day month year
  | (day > gregorianMonthLength year month) = "class" =: "dateLabel" <> "style" =: "visibility: hidden"
  | otherwise = "class" =: "dateLabel" <> "style" =: "visibility: visible"


timeWidget ::  MonadWidget t m => Dynamic t ZonedTime -> m (Event t TimeOfDay)
timeWidget zt = do
  let localTime = fmap (localTimeOfDay . zonedTimeToLocalTime) zt
  let hours = fmap todHour localTime -- Dynamic t Int
  let mins = fmap todMin localTime -- Dynamic t Int
  sampleHours <- sample $ current hours
  sampleMins <- sample $ current mins
  h <- divClass "timeWidgetInput" $ intTextInputW hours -- Event t Int --  $ def & attributes .~ constDyn ("class" =: "code-font background timeWidgetInput") & textInputConfig_inputType .~ "number" & textInputConfig_setValue .~ (updated hours) -- Dyn Text
  text ":"
  m <- divClass "timeWidgetInput" $ intTextInputW mins -- Event t Int -- $ def & attributes .~ constDyn ("class" =: "code-font background timeWidgetInput") & textInputConfig_inputType .~ "number" & textInputConfig_setValue .~ (updated mins) -- Dyn Text
  hDyn <- holdDyn sampleHours h
  mDyn <- holdDyn sampleMins m
  let newTimeOfDay = updatedTimeOfDay <$> hDyn <*> mDyn   -- Event t TimeOfDay
  return $ updated (newTimeOfDay)

updatedTimeOfDay :: Int -> Int -> TimeOfDay
updatedTimeOfDay h m  = do
  let h' | h < 0 = (00 :: Int)
         | h > 23 = (23 :: Int)
         | otherwise  = h
  let m' | m < 0 = (00 :: Int)
         | m > 59 = (59 :: Int)
         | otherwise  = m
  TimeOfDay h' m' (00 :: F.Pico)-- hoursToTimeZone (read (T.unpack tz) :: Int))

-- ZonedTime (LocalTime day timeOfDay) timeZone)
 -- type calendarEvent = (Text, ZonedTime)
zonedTimeToTimeOfDayAndTimeZone :: ZonedTime -> (Day, TimeOfDay, TimeZone) --  (TimeOfDay, TimeZone)
zonedTimeToTimeOfDayAndTimeZone zt = do
-- (LocalTime day timeOfDay) timeZone)
  let localToUTCtime = zonedTimeToUTC zt -- UTCTime
  let utcTime = timeToTimeOfDay $ utctDayTime localToUTCtime -- $ localTimeToUTC (zonedTimeZone zt) (zonedTimeToLocalTime zt) --  UTC {utctDay :: Day  utctDayTime :: DiffTime}
  let utcDay = utctDay localToUTCtime
  let utcTimeZone =  TimeZone { timeZoneName = "UTC"}
  (utcDay, utcTime, utcTimeZone)

getStartingDateFromCalendarEv :: CalendarEvent -> ZonedTime
getStartingDateFromCalendarEv (CalendarEvent details (CalendarTime startingDate recurrence)) = startingDate

getEndDateFromCalendarEv :: CalendarEvent -> ZonedTime
getEndDateFromCalendarEv (CalendarEvent details (CalendarTime startingDate Nothing)) = startingDate -- deberia ser nothing
getEndDateFromCalendarEv (CalendarEvent details (CalendarTime startingDate (Just (Recurrence periodicity endDate)))) = endDate



utcTimeToThisClientZoneTimeWidget :: MonadWidget t m => Event t CalendarEvent -> Dynamic t ZonedTime ->  m ()
utcTimeToThisClientZoneTimeWidget calendarEv serverZT = do
  let calendarEv' = fmap (T.pack . show . getStartingDateFromCalendarEv) calendarEv -- Ev t Text
  sampledServerZT <- sample $ current serverZT -- ZonedTime
  thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
  let serverZT' = fmap zonedTimeToUTC serverZT -- Event UTCTime
  let utcToZonedTime' = fmap (utcToZonedTime thisComputerZone) serverZT' -- Event t ZonedTime
  let thisComputerLocalTime = fmap (T.pack . show) utcToZonedTime' -- Event t text
  thisComputerLocalTime'' <- holdDyn (T.pack $ show sampledServerZT) (leftmost [updated thisComputerLocalTime, calendarEv'])
  dynText $ thisComputerLocalTime''

zonedTimeToUTCTime :: MonadWidget t m => Event t CalendarEvent -> Dynamic t ZonedTime ->  m ()
zonedTimeToUTCTime calendarEv serverZT = do
  sampledServerZT <- sample $ current serverZT -- ZonedTime
  let serverZT' = fmap (T.pack . show . zonedTimeToUTC) serverZT -- dyn Text
  let calendarEv' = fmap (T.pack . show . zonedTimeToUTC . getStartingDateFromCalendarEv) calendarEv
  zonedTimeToUTCTime <- holdDyn (T.pack $ show sampledServerZT) (leftmost [updated serverZT', calendarEv'])
  dynText $ zonedTimeToUTCTime

-- localTimeToUTC :: TimeZone -> LocalTime -> UTCTime
    -- data TimeOfDay = TimeOfDay
    --   { todHour :: Int
    --   , todMin  :: Int
    --   , todSec  :: Pico
    --   }

-- keep track of provided day and zone and when the user changes them, it will produce events.
-- text boxes for now.
-- the time zone; hard code our time zone -> The initial time zone is what it is, this widget will just display the UTC (and come back to the timezone picker? later). This widget displays the hours and minutes and if they are valid (i.e. only numbers).

-- data Variable t a = Variable {
--   currentValue :: Dynamic t a,
--   localEdits :: Event t a
--   }

toDate :: (Day, TimeOfDay, TimeZone) -> Day
toDate (day, time, zone) = day

toTime :: (Day, TimeOfDay, TimeZone) -> TimeOfDay
toTime (day, time, zone) = time

toZone :: (Day, TimeOfDay, TimeZone) -> TimeZone
toZone (day, time, zone) = zone

getYeatMonthOrDayFromTuple :: Int -> (Integer, Int, Int) -> Int
getYeatMonthOrDayFromTuple 1 (h, m, d) = fromIntegral h
getYeatMonthOrDayFromTuple 2 (h, m, d) = m
getYeatMonthOrDayFromTuple 3 (h, m, d) = d

utcTimeZoneWidget :: MonadWidget t m => Dynamic t (Day, TimeOfDay, TimeZone) -> m ()
utcTimeZoneWidget timeAndZone = do
  let timeOfDay' = fmap toTime timeAndZone
  let hour = fmap (T.pack . show . todHour) timeOfDay'
  let mins = fmap (T.pack . show . todMin ) timeOfDay'
  let date = fmap (toGregorian . toDate) timeAndZone
  let year = fmap (T.pack . show . getYeatMonthOrDayFromTuple 1) date
  let month = fmap (T.pack . show . getYeatMonthOrDayFromTuple 2) date
  let day = fmap (T.pack . show . getYeatMonthOrDayFromTuple 3) date
  let zone = fmap (T.pack . timeZoneName . toZone) timeAndZone
  updatedHour <- holdDyn "" (updated hour) -- Dynamic t Text
  updatedMins <- holdDyn "" (updated mins) -- Dynamic t Text
  updatedYear <- holdDyn "" (updated year) -- Dynamic t Text
  updatedMonth <- holdDyn "" (updated month) -- Dynamic t Text
  updatedDay <- holdDyn "" (updated day) -- Dynamic t Text
  divClass "utcTimeZoneWidget code-font background" $ dynText (updatedYear <> "-" <> updatedMonth <> "-" <> updatedDay <> " ")
  divClass "utcTimeZoneWidget code-font background" $ dynText (updatedHour <> ":" <> updatedMins <> "00 ")
  divClass "utcTimeZoneWidget code-font background" $ dynText zone

getDetailsFromCalendarEv :: CalendarEvent -> Text
getDetailsFromCalendarEv (CalendarEvent details calendarTime) = details

calendarEventWidget :: MonadWidget t m => Dynamic t CalendarEvent -> W t m (Variable t CalendarEvent)
calendarEventWidget delta = divClass "calendarEventWidget" $ mdo
  -- let changes = currentValue v
  descEv <- divClass "descriptionWidget code-font background" $ descriptionWidget $ fmap getDetailsFromCalendarEv delta -- Event t Text, representing a local edit
  autoUpdateStartingDateEv <- autoUpdateStartingDate $ fmap getStartingDateFromCalendarEv delta
  thisComputerLocalTime <- divClass "thisComputerLocalTimeWidget code-font background" $ utcTimeToThisClientZoneTimeWidget localUpdatesWithoutDescriptionF $ fmap getStartingDateFromCalendarEv delta
  (dateEv, timeOfDayEv,zoneEv,enableRecurrenceEv, changePeriodicityEv, changeEndDateEv) <- divClass "selectDateAndRecurrencyContainer" $ do
    dateEv' <- divClass "dateWidget code-font background" $ dateWidget $ fmap (localDay . zonedTimeToLocalTime . getStartingDateFromCalendarEv) delta -- Event t Day
    timeOfDayEv' <- divClass "timeWidget code-font background" $ timeWidget $ fmap getStartingDateFromCalendarEv delta -- Event t TimeOfDay
    zoneEv' <-divClass "zoneWidget code-font background" $ timeZoneWidget timeOfDayEv $ fmap (zonedTimeZone . getStartingDateFromCalendarEv) delta -- Event t TimeZone
    enableRecurrenceEv' <- divClass "enableRecurrence" $ enableRecurrenceWidget $ fmap getRecurrenceFromCalendarEvent delta-- Event t CalendarTime
    changePeriodicityEv' <- divClass "selectPeriodicity" $ selectPeriodicityWidget $ fmap (getPeriodicityFromRecurrence' . getRecurrenceFromCalendarEvent) delta
    changeEndDateEv' <- divClass "dateWidget code-font background" $ changeEndDateWidget $ fmap (localDay . zonedTimeToLocalTime . getEndDateFromCalendarEv) delta -- Event t Day
    return (dateEv', timeOfDayEv', zoneEv', enableRecurrenceEv', changePeriodicityEv', changeEndDateEv')
  zonedTimeToUTCTimeEv <- divClass "zonedTimeToUTCTime code-font background" $ zonedTimeToUTCTime localUpdatesWithoutDescriptionF $ fmap getStartingDateFromCalendarEv delta -- m ()

    -- printRecurrence' <- printRecurrence localUpdatesWithoutDescriptionF $ fmap getRecurrenceFromCalendarEvent delta

  let descF = fmap changeDescription descEv -- Event t (CalendarEvent -> CalendarEvent)
  let autoUpdateStartingDateF = fmap startingDayAutoUpdate autoUpdateStartingDateEv
  let dateF = fmap changeDate dateEv-- Event t (CalendarEvent -> CalendarEvent)
  let timeF = fmap changeTimeOfDay timeOfDayEv
  let zoneF = fmap changeTimeZone zoneEv
  let enableRecurrenceF = fmap enableRecurrence enableRecurrenceEv -- Event t (CalendarEvent -> CalendarEvent)
  -- let endDateF = fmap changeEndDate endDateEv -- Event t (CalendarEvent -> CalendarEvent)
  -- let recurrenceF = fmap changeRecurrence recurrenceEv
  let changePeriodicityF = fmap changePeriodicity changePeriodicityEv
  let changeEndDateF = fmap changeEndDate changeEndDateEv

  let localF = mergeWith (.) [descF, dateF, timeF, zoneF, enableRecurrenceF, changePeriodicityF, changeEndDateF, autoUpdateStartingDateF] -- Event t (CalendarEvent -> CalendarEvent)--
  let localUpdates = attachWith (flip ($)) (current $ currentValue v) localF -- Event t CalendarEvent
  let localFWithoutDescriptionF = mergeWith (.) [dateF, timeF, zoneF, enableRecurrenceF, changePeriodicityF, changeEndDateF]
  let localUpdatesWithoutDescriptionF = attachWith (flip ($)) (current $ currentValue v) localFWithoutDescriptionF -- Event t CalendarEvent
  v <- variable delta localUpdates
  return v


-- day of week
-- | \"Circular\", so for example @[Tuesday ..]@ gives an endless sequence.
-- Also: 'fromEnum' gives [1 .. 7] for [Monday .. Sunday], and 'toEnum' performs mod 7 to give a cycle of days.
data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Eq, Show, Read)

instance Enum DayOfWeek where
    toEnum i =
        case mod i 7 of
            0 -> Sunday
            1 -> Monday
            2 -> Tuesday
            3 -> Wednesday
            4 -> Thursday
            5 -> Friday
            _ -> Saturday
    fromEnum Monday = 1
    fromEnum Tuesday = 2
    fromEnum Wednesday = 3
    fromEnum Thursday = 4
    fromEnum Friday = 5
    fromEnum Saturday = 6
    fromEnum Sunday = 7
    enumFromTo wd1 wd2
        | wd1 == wd2 = [wd1]
    enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
    enumFromThenTo wd1 wd2 wd3
        | wd2 == wd3 = [wd1, wd2]
    enumFromThenTo wd1 wd2 wd3 = wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - (fromEnum wd1)) wd3

dayOfWeek :: Day -> DayOfWeek
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

showDayOfWeek :: DayOfWeek -> Text
showDayOfWeek d = T.pack $ show d
