{-# LANGUAGE OverloadedStrings, FlexibleContexts #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.CalendarEvent where

import Reflex
import Reflex.Dom
import Reflex.Dynamic

import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import Data.Time.LocalTime
import GHCJS.DOM.EventM
import qualified Data.Fixed as F
import Data.Map as Map
import Data.IntMap.Strict
import Data.Map.Strict as M
import qualified Data.IntMap as IntMap
import Data.Maybe as Maybe
import Control.Monad.IO.Class
import Control.Monad(liftM)
import qualified Data.Char as C
import Safe.Foldable (maximumMay)
import qualified Data.List as L

import Estuary.Types.Language
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Widgets.Reflex
import Estuary.Types.Language
import qualified Estuary.Types.Term as Term


-- working widget
calendarEventWidget :: MonadWidget t m => Dynamic t CalendarEvents -> W t m (Variable t CalendarEvents)
calendarEventWidget deltasDown = mdo
  iDeltasDown <- sample $ current deltasDown
  deltasDownBroken <- holdDyn iDeltasDown never
  today <- liftIO getZonedTime
  let i = IntMap.singleton 0 (CalendarEvent "" (CalendarTime today (Recurrence Once today)))
  -- delta <- holdDyn i never
  addButton <- divClass "addButtonCalendarEvent" $ buttonWithClass "+" -- Event t ()
  let newCalendarEvent = CalendarEvent "" (CalendarTime today (Recurrence Once today))
  mapEv <- widgetMapEventWithAddDelete deltasDown (newCalendarEvent <$ addButton) calendarEventBuilderMaybe
  variable deltasDown mapEv

calendarEventBuilderMaybe :: MonadWidget t m => Dynamic t CalendarEvent ->  W t m (Event t (Maybe CalendarEvent))
calendarEventBuilderMaybe delta = divClass "calendarEventRow" $ do
  today <- liftIO getZonedTime
  deleteButton <- divClass "deleteButtonCalendarEvent" $ buttonWithClass "-" -- Event t ()
  row <- calendarEventWidgetEv delta
  let rowMaybe = fmap Just row -- Event t (Maybe CalendarEvent)
  return $ leftmost [rowMaybe, Nothing <$ deleteButton]

calendarEventBuilder :: MonadWidget t m => Dynamic t CalendarEvent ->  W t m (Event t CalendarEvent)
calendarEventBuilder delta = do
  today <- liftIO getZonedTime
  row <- calendarEventWidgetEv delta -- Event t CalendarEvent
  -- let rowMaybe = fmap row -- Event t (Maybe CalendarEvent)
  return $ leftmost [row]

calendarEventWidgetEv :: MonadWidget t m => Dynamic t CalendarEvent -> W t m (Event t CalendarEvent)
calendarEventWidgetEv delta = divClass "calendarEventWidgetMainContainer" $ mdo
  autoUpdateStartingDateEv <- autoUpdateStartingDate $ fmap getStartingDateFromCalendarEv delta

  (descEv, dateEv, changePeriodicityEv, changeEndDateEv) <- divClass "calendarEventWidgetSubContainer" $ do
    descEv' <- divClass "detailsContainer" $ divClass "descriptionWidget code-font background" $ descriptionWidget $ fmap getDetailsFromCalendarEv delta -- Event t Text

    dateEv' <- divClass "selectStartingDateContainer" $ do
        dateEv'' <- divClass "dateWidget code-font background" $ utcTimeOrLocalTimeWidget (fmap getStartingDateFromCalendarEv delta) -- Event t TimeZone
        return dateEv''

    (changePeriodicityEv', changeEndDateEv') <- divClass "periodicity" $ do
      changePeriodicityEv'' <- divClass "selectPeriodicity  code-font background" $ changePeriodicityWidget $ fmap (getPeriodicityFromRecurrence . getRecurrenceFromCalendarEvent) delta
      changeEndDateEv'' <- divClass "endDateWidget code-font background" $ changeEndDateWidget changePeriodicityEv'' (fmap (getPeriodicityFromRecurrence . getRecurrenceFromCalendarEvent) delta) $ fmap (localDay . zonedTimeToLocalTime . getEndDateFromCalendarEv) delta -- Event t Day
      return (changePeriodicityEv'', changeEndDateEv'')
    return (descEv', dateEv', changePeriodicityEv', changeEndDateEv' {--, timeOfDayEv', zoneEv', utcTimeEv'--})

  let descF = fmap changeDescription descEv -- Event t (CalendarEvent -> CalendarEvent)
  let autoUpdateStartingDateF = fmap startingDayAutoUpdate autoUpdateStartingDateEv
  let dateAndTimeF = fmap changeDateAndTime dateEv
  let changePeriodicityF = fmap changePeriodicity changePeriodicityEv
  let changeEndDateF = fmap changeEndDate changeEndDateEv

  let localF = mergeWith (.) [descF, autoUpdateStartingDateF, dateAndTimeF, changePeriodicityF, changeEndDateF] -- Event t (CalendarEvent -> CalendarEvent)--

  let localUpdates = attachWith (flip ($)) (current $ currentValue v) localF -- Event t CalendarEvent

  v <- variable delta localUpdates --  m (Variable t a)
  return localUpdates -- -- Event t (CalendarEvent )

-- helper functions

changePeriodicity :: Periodicity -> CalendarEvent -> CalendarEvent
changePeriodicity Once (CalendarEvent details (CalendarTime startingDate (Recurrence periodicity endDate))) = CalendarEvent details (CalendarTime startingDate (Recurrence Once startingDate))
changePeriodicity newPeriodicity (CalendarEvent details (CalendarTime startingDate (Recurrence periodicity endDate))) = CalendarEvent details (CalendarTime startingDate (Recurrence newPeriodicity endDate))

--
changeEndDate :: Day -> CalendarEvent -> CalendarEvent
changeEndDate newDay (CalendarEvent details (CalendarTime startingDate  (Recurrence periodicity (ZonedTime (LocalTime day timeOfDay) timeZone)))) = CalendarEvent details (CalendarTime startingDate  (Recurrence periodicity (ZonedTime (LocalTime newDay timeOfDay) timeZone)))

--
startingDayAutoUpdate :: ZonedTime -> CalendarEvent -> CalendarEvent
startingDayAutoUpdate now (CalendarEvent details (CalendarTime startingDate   (Recurrence Once endDate))) = CalendarEvent details (CalendarTime startingDate   (Recurrence Once endDate))

-- Daily
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence Daily (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let newStartingDay = addDays 1 startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) = nextStartingDate
                        | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate'  (Recurrence Daily (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))

-- "daily until" recurrence
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence DailyUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let newStartingDay = addDays 1 startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) = nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Recurrence DailyUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))

-- "weekly"
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence Weekly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let newStartingDay = addDays 7 startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) =  nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate'  (Recurrence Weekly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))

-- "weekly until"
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence WeeklyUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let newStartingDay = addDays 7 startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) =  nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Recurrence WeeklyUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))


--"monthly date"
  --repeat on same day number as opposed to the same week day :: eg. will always update to the 3rd of the next month
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Recurrence MonthlyDate (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let numberOfDaysOfMonth  =  gregorianMonthLength (getYearFromDay startingDay) (getMonthFromDay startingDay)
  let newStartingDay = addDays (fromIntegral numberOfDaysOfMonth) startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) = nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate'  (Recurrence MonthlyDate (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))

--"monthly date until"
  --repeat on same day number as opposed to the same week day :: eg. will always update to the 3rd of the next month
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence MonthlyDateUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let numberOfDaysOfMonth  =  gregorianMonthLength (getYearFromDay startingDay) (getMonthFromDay startingDay)
  let newStartingDay = addDays (fromIntegral numberOfDaysOfMonth) startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) = nextStartingDate
                        | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate'  (Recurrence MonthlyDateUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))

-- monthly nth day of week until"
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Recurrence MonthlyNthDayOfWeekUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let (nth, dayOfWeek)  =  dayOfWeekAndNthofDayOfWeekOfMonth startingDay --  (Int, DayOfWeek) of select date, e.g 1st Monday of Month
  let newStartingDay = dateOfNthDayOfWeekOfNextMonth (nth, dayOfWeek) startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) = nextStartingDate
                        | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Recurrence MonthlyNthDayOfWeekUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))


-- "monthly on the first wed"
  -- repeat on same day week day as opposed to on a same day number: eg. will always update to the 3rd wednesday of the next month
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay) (Recurrence MonthlyNthDayOfWeek (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let (nth, weekOfDay)  =  dayOfWeekAndNthofDayOfWeekOfMonth startingDay --  (Int, DayOfWeek) of select date, e.g 1st Monday of Month
  let newStartingDay = dateOfNthDayOfWeekOfNextMonth (nth, weekOfDay) startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) = nextStartingDate
                        | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Recurrence MonthlyNthDayOfWeek (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))



-- "yearly "
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence Yearly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let yearLength year | (isLeapYear year) == True = 366
                      | otherwise = 365
  let newStartingDay = addDays (fromIntegral $ yearLength (getYearFromDay startingDay)) startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) =  nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate'  (Recurrence Yearly (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))


-- "yearly until"
startingDayAutoUpdate now (CalendarEvent details (CalendarTime (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)   (Recurrence YearlyUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))) = do
  let startingDate = (ZonedTime (LocalTime startingDay timeOfStartingDay) timeZoneOfStartingDay)
  let endDate = (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)
  let yearLength year | (isLeapYear year) == True = 366
                      | otherwise = 365
  let newStartingDay = addDays (fromIntegral $ yearLength (getYearFromDay startingDay)) startingDay
  let nextStartingDate = ZonedTime (LocalTime newStartingDay timeOfStartingDay) timeZoneOfStartingDay
  let diffNextStartDateAndEndDate = diffZonedTime nextStartingDate endDate
  let diffNowAndStartDate = diffZonedTime now startingDate
  let diffNowAndEndDate = diffZonedTime now endDate
  let nextStartingDate' | (diffNowAndStartDate > 0) && (diffNowAndEndDate < 0) && (diffNextStartDateAndEndDate < 0) =  nextStartingDate
                      | otherwise = startingDate
  CalendarEvent details (CalendarTime nextStartingDate' (Recurrence YearlyUntil (ZonedTime (LocalTime endDay timeOfEndDay) timeZoneOfEndingDay)))

dateOfNthDayOfWeekOfNextMonth :: (Int, DayOfWeek) -> Day -> Day
dateOfNthDayOfWeekOfNextMonth (n, weekOfDay) startingDay = do
  let nextmonth = addGregorianMonthsClip 1 startingDay -- Day
  let firstDayOfNextMonth = fromGregorian (getYearFromDay nextmonth) (getMonthFromDay nextmonth) (01 :: Int)
  let firstSelectedDayOfWeekOfNextMonth = firstDayOfWeekOnAfter weekOfDay firstDayOfNextMonth -- DayOfWeek -> Day -> Day  e.g. Wednesday
  nthOfDayOfWeekOfNextMonth n firstSelectedDayOfWeekOfNextMonth -- Day

nthOfDayOfWeekOfNextMonth :: Int -> Day -> Day
nthOfDayOfWeekOfNextMonth weekNum firstSelectedDayOfWeekOfMonth
  | weekNum == 1 = addDays (fromIntegral 0) firstSelectedDayOfWeekOfMonth
  | weekNum == 2 = addDays (fromIntegral 7) firstSelectedDayOfWeekOfMonth
  | weekNum == 3 = addDays (fromIntegral 14) firstSelectedDayOfWeekOfMonth
  | weekNum == 4 = addDays (fromIntegral 21) firstSelectedDayOfWeekOfMonth
  | otherwise = clipDayToLastDayOfMonth firstSelectedDayOfWeekOfMonth (addDays (fromIntegral 28) firstSelectedDayOfWeekOfMonth) (addDays (fromIntegral 21) firstSelectedDayOfWeekOfMonth)

-- si la fecha calculada al sumar 28 es mayor que el ultimo dia del mes, entonces default a 21 dias
clipDayToLastDayOfMonth :: Day -> Day -> Day -> Day
clipDayToLastDayOfMonth firstDayOfMonth d ddef = do
  let monthLength = gregorianMonthLength (getYearFromDay firstDayOfMonth) (getMonthFromDay firstDayOfMonth) -- e.g 31
  let lastDayOfMonth = fromGregorian (getYearFromDay firstDayOfMonth) (getMonthFromDay firstDayOfMonth) (monthLength :: Int)-- last Day  of month
  let clipDayToLastDayOfMonth' |d > lastDayOfMonth = ddef
                               |otherwise = d
  clipDayToLastDayOfMonth'
dayOfWeekAndNthofDayOfWeekOfMonth :: Day -> (Int, DayOfWeek)

dayOfWeekAndNthofDayOfWeekOfMonth selectedDay = do
  let nameOfselectedDay = dayOfWeek selectedDay -- :: Day -> DayOfWeek
  let fstDayOfMonth =  fromGregorian (getYearFromDay selectedDay) (getMonthFromDay selectedDay) (01 :: Int) -- Year -> MonthOfYear -> DayOfMonth -> Day
  let firstSelectedDayOfWeek = firstDayOfWeekOnAfter nameOfselectedDay fstDayOfMonth -- DayOfWeek -> Day -> Day  e.g. Wednesday
  let whichNofXDay' = whichNofXDay firstSelectedDayOfWeek selectedDay
  (whichNofXDay', nameOfselectedDay)

whichNofXDay :: Day -> Day -> Int
whichNofXDay d1 d2
  | (day1 + 0) ==  day2 = 1
  | (day1 + 7) ==  day2 = 2
  | (day1 + 14) ==  day2 = 3
  | (day1 + 21) ==  day2 = 4
  | otherwise = 5
    where
      (year1, month1, day1) = toGregorian d1
      (year2, month2, day2) = toGregorian d2


diffZonedTime :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZonedTime a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

changeDescription :: Text -> CalendarEvent -> CalendarEvent -- same for the others
changeDescription newDetails (CalendarEvent details calendarTime) = CalendarEvent newDetails calendarTime

changeDateAndTime :: ZonedTime -> CalendarEvent -> CalendarEvent -- given a day the func will return the new calendarEvent --attachWith
changeDateAndTime zt (CalendarEvent details (CalendarTime zonedTime   recurrence))  = CalendarEvent details  (CalendarTime zt  recurrence)

changeDate :: Day -> CalendarEvent -> CalendarEvent -- given a day the func will return the new calendarEvent --attachWith
changeDate d (CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) timeZone)   recurrence))  = CalendarEvent details  (CalendarTime (ZonedTime (LocalTime d timeOfDay) timeZone)  recurrence)

changeTimeOfDay :: TimeOfDay -> CalendarEvent -> CalendarEvent
changeTimeOfDay td (CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) timeZone)  recurrence)) = CalendarEvent details (CalendarTime (ZonedTime (LocalTime day td) timeZone)  recurrence)

changeTimeZone :: TimeZone -> CalendarEvent -> CalendarEvent --
changeTimeZone tz (CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) timeZone)  recurrence)) = CalendarEvent details (CalendarTime (ZonedTime (LocalTime day timeOfDay) tz)  recurrence)


descriptionWidget ::  MonadWidget t m => Dynamic t Text -> W t m (Event t Text) -- similar to our text editors ---- displays a text
descriptionWidget t = divClass "descriptionWidgetInput" $ do
  addTitle <- translatableText (M.fromList [(English,"Add a title"), (Español, "Agrega un titulo")]) -- Dynamic t Text
  let inputHint' = fmap inputHint addTitle
  description <- tooltip (textInputW inputHint' t) (dynText =<< translatableText (M.fromList [(English,"Add a title"), (Español, "Agrega un titulo")]))
  return $ description

inputHint :: Text -> Map Text Text
inputHint x = "placeholder" =: x

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

thisClientTimeZoneEv ::  MonadWidget t m => Event t TimeOfDay -> W t m (Event t TimeZone)
thisClientTimeZoneEv tEv = mdo
  thisClientTimeZone <- liftIO getCurrentTimeZone -- Dyn TimeZone -- new zone
  let zDyn = tagPromptlyDyn (constDyn thisClientTimeZone) tEv--
  return zDyn

showZoneWidget ::  MonadWidget t m => Dynamic t TimeZone -> W t m ()
showZoneWidget updatedTz = do
  let showZDyn = fmap (T.pack . timeZoneName) updatedTz
  dynText showZDyn -- <> " " <> "(the local time of the last person who edited the time)"

timeZoneWidget ::  MonadWidget t m => Event t TimeOfDay -> Dynamic t ZonedTime -> Dynamic t TimeZone -> W t m (Event t TimeZone)
timeZoneWidget changedTimeEv zt currentDynTimeZone = mdo
  sampledCurrentZone <- sample $ current currentDynTimeZone
  let showSampledCurrentZone = timeZoneOffsetTotimeZoneName sampledCurrentZone -- text
  thisClientTimeZoneEv' <- thisClientTimeZoneEv changedTimeEv -- event t TimeZone

  let showTimeZoneWidgetEv = fmap timeZoneOffsetTotimeZoneName thisClientTimeZoneEv'
  dynTextThisClientTimeZone <- holdDyn showSampledCurrentZone showTimeZoneWidgetEv -- Dyn text

  dynThisClientTimeZone <- holdDyn sampledCurrentZone thisClientTimeZoneEv' -- dyn TimeZone
  sampledThisClientTimeZone <- sample $ current dynThisClientTimeZone -- timezone
  let uctTimeOfDay = fmap (timeToTimeOfDay . utctDayTime. zonedTimeToUTC) zt -- Dynamic TimeOfDay
  sampledUTCTimeOfDay <- sample $ current uctTimeOfDay
  let localTimeOfDay = fmap (utcToLocalTimeOfDay sampledThisClientTimeZone) uctTimeOfDay
  return thisClientTimeZoneEv'

timeZoneOffsetTotimeZoneName :: TimeZone -> Text
timeZoneOffsetTotimeZoneName zone = do
 let offsetStringToInt = read (timeZoneOffsetString zone) :: Int
 let offsetIntDisplaced = (realToFrac offsetStringToInt) * 0.01
 let decimalFromOffset x | (abs ((realToFrac $ round x) - x) == 0.0) = show $ round x
                         | otherwise = show x
 T.pack $ "UTC" ++ (decimalFromOffset offsetIntDisplaced)


--
printRecurrence :: MonadWidget t m => Dynamic t Recurrence -> m ()
printRecurrence serverR = do
  sampledRecurrence <- sample $ current serverR
  recurrence <- holdDyn sampledRecurrence  (updated serverR)
  dynText $ fmap (T.pack . show) recurrence

getPeriodicityFromRecurrence :: Recurrence -> Periodicity
getPeriodicityFromRecurrence r = periodicity r

getRecurrenceFromCalendarEvent :: CalendarEvent -> Recurrence
getRecurrenceFromCalendarEvent (CalendarEvent details (CalendarTime startingDate  recurrence)) = recurrence

changePeriodicityWidget :: MonadWidget t m => Dynamic t (Periodicity) -> W t m (Event t Periodicity)
changePeriodicityWidget p = mdo
  -- let p' = fmap periodicityToKey p -- Dynamic Int -- (updated p) -- Event t Int
  perdiodicityWTranslation' <- perdiodicityWTranslation
  dd <- dropdown 1 perdiodicityWTranslation' (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" )) -- p' -- Event t Int -- $ def & dropdownConfig_setValue .~ currentPeriodicity
  let selItem = lookupPeriodicity <$> value dd <*> perdiodicityWTranslation' -- event t Periodicity
  return $ (updated selItem)

  -- Map.fromList [(1, "Once"), (2, "Daily"), (3, "Daily until"), (4, "Weekly"), (5, "Weekly until"), (6, "Monthly date"), (7, "Monthly date until"), (8, "Monthly nth weekday"), (9, "Monthly nth weekday until"), (10, "Yearly"), (11, "Yearly until")]

perdiodicityWTranslation :: MonadWidget t m =>  W t m (Dynamic t (Map Int Text))
perdiodicityWTranslation = do
  onceMap <- translatableText (M.fromList [(English,"Once"), (Español, "No se repite")]) -- Dynamic t Text
  dailyMap <- translatableText (M.fromList [(English,"Daily"), (Español, "Diario")]) -- Dynamic t Text
  dailyUntilMap <- translatableText (M.fromList [(English,"Daily until"), (Español, "Diario hasta")]) -- Dynamic t Text
  weeklyMap <- translatableText (M.fromList [(English,"Weekly"), (Español, "Cada semana")]) -- Dynamic t Text
  weeklyUntilMap <- translatableText (M.fromList [(English,"Weekly until"), (Español, "Cada semana hasta")]) -- Dynamic t Text
  monthlyDateMap <- translatableText (M.fromList [(English,"Monthly date"), (Español, "Cada Mes")]) -- Dynamic t Text
  monthlyDateUntilMap <- translatableText (M.fromList [(English,"Monthly date until"), (Español, "Cada mes hasta")]) -- Dynamic t Text
  monthlyNthDayMap <- translatableText (M.fromList [(English,"Monthly nth weekday"), (Español, "Cada x día del mes")]) -- Dynamic t Text
  monthlyNthDayUntilMap <- translatableText (M.fromList [(English,"Monthly nth weekday until"), (Español, "Cada x día del mes hasta")]) -- Dynamic t Text
  yearlyMap <- translatableText (M.fromList [(English,"Yearly"), (Español, "Anualmente")]) -- Dynamic t Text
  yearlyUntilMap <- translatableText (M.fromList [(English,"Yearly until"), (Español, "Anualmente hasta")]) -- Dynamic t Text
  let periodicityMapWTranslation = periodicities <$> onceMap <*> dailyMap <*> dailyUntilMap <*> weeklyMap <*> weeklyUntilMap <*> monthlyDateMap <*> monthlyDateUntilMap <*> monthlyNthDayMap <*> monthlyNthDayUntilMap <*> yearlyMap <*> yearlyUntilMap -- Dynamic t (Map Int Text)
  return periodicityMapWTranslation

periodicities :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Map.Map Int Text
periodicities t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = M.fromList [(1, t1), (2, t2), (3, t3), (4, t4), (5, t5), (6, t6), (7, t7), (8, t8), (9, t9), (10, t10), (11, t11)]

lookupPeriodicity :: Int ->  Map.Map Int Text -> Periodicity
lookupPeriodicity key lMap = textToPeriodicity $ Maybe.fromJust (Map.lookup key lMap)

textToPeriodicity :: Text -> Periodicity
textToPeriodicity x
  | x == "Once" ||  x == "No se repite" =  Once
  | x == "Daily" ||  x == "Diario" =  Daily
  | x == "Daily until" ||  x == "Diario hasta" =  DailyUntil
  | x == "Weekly" ||  x == "Cada semana" = Weekly
  | x == "Weekly until" ||  x == "Cada semana hasta" = WeeklyUntil
  | x == "Monthly date" ||  x == "Cada Mes" = MonthlyDate
  | x == "Monthly date until" ||  x ==  "Cada mes hasta" = MonthlyDateUntil
  | x == "Monthly nth weekday" ||  x == "Cada x día del mes" = MonthlyNthDayOfWeek
  | x == "Monthly nth weekday until" ||  x == "Cada x día del mes hasta" = MonthlyNthDayOfWeekUntil
  | x == "Yearly" ||  x == "Anualmente" = Yearly
  | x == "Yearly until" ||  x == "Anualmente hasta" = YearlyUntil

endDateAttrs :: Periodicity -> Map Text Text
endDateAttrs p | p == Once || p == Daily || p == Weekly || p == MonthlyDate || p == MonthlyNthDayOfWeek || p == Yearly =  "style" =: ("display: none;")
               | otherwise = "style" =: ("display: block;")

changeEndDateWidget :: MonadWidget t m => Event t Periodicity -> Dynamic t Periodicity -> Dynamic t Day -> W t m (Event t Day)
changeEndDateWidget pEv dynP d = mdo
    sampledCurrentPeriodicity <- sample $ current dynP
    let updatedP = leftmost [pEv, updated dynP]
    dynPeriodicity <- holdDyn sampledCurrentPeriodicity updatedP
    let dynAttrs = endDateAttrs <$> dynPeriodicity
    elDynAttr "div" dynAttrs $ changeEndDateWidget' d


changeEndDateWidget' :: MonadWidget t m => Dynamic t Day -> W t m (Event t Day)
changeEndDateWidget' d = mdo
  let openCloseEvs = leftmost [mode1Ev, () <$ mode2Ev]
  dynBool <- toggle False openCloseEvs
  let mode1Attrs = calendaContainerAttrs <$> dynBool
  let mode2Attrs = dateLabelAttrs <$> dynBool
  mode2Ev <- dateWidgetMode2Ev mode1Attrs d
  mode1Ev <- elDynAttr "div" mode2Attrs $ dateWidgetMode1ForEndDate selDate -- dateWidgetMode1ForEndDate selDate -- m (Event t ())
  today <- (sample $ current d)
  let updatedDay = leftmost [mode2Ev, updated d]
  selDate <- holdDyn today updatedDay -- mode2Ev -- Dynamic t Day
  return mode2Ev

autoUpdateStartingDate :: MonadWidget t m => Dynamic t ZonedTime -> W t m (Event t ZonedTime)
autoUpdateStartingDate defTime = do
  defTime' <- sample $ current defTime
  let defZonedTimeToUTC = zonedTimeToUTC defTime'
  nowUTC <- liftIO getCurrentTime
  evTick <- tickLossy 1 nowUTC
  let evTime = _tickInfo_lastUTC <$> evTick
  nowUTC' <- holdDyn defZonedTimeToUTC evTime
  let nowUTCtoZonedTime = fmap (utcToZonedTime (zonedTimeZone defTime')) nowUTC' -- ZonedTime
  return $ updated nowUTCtoZonedTime

dateWidgetMode1ForEndDate ::  MonadWidget t m =>  Dynamic t Day -> W t m (Event t ())
dateWidgetMode1ForEndDate d = mdo
  let d' = fmap (T.pack . show) d
  (openPicker, _) <- elClass' "div" "code-font background selectedDate" $ dynText d'
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t calendarEvent)
  return openPicker'

-- new challenge
--- it displays the date and time in local terms, and when you click on the date you are picking a date in local terms. clicking on the local area brings up a picker where you can set date and time in local, clicking on the UTC area brings up a similar picker where you can set date and time in UTC prior to the click, you are not editing
localTimeWidgetRefactored :: MonadWidget t m => Dynamic t ZonedTime ->  Event t () -> W t m (Event t ZonedTime, Event t ()) --  m (Event t ZonedTime)
localTimeWidgetRefactored zt mode1And2FromFromUTCTimeWidget = mdo
  let openCloseEvsMode1 = leftmost [mode1Ev, () <$ mode2Ev]
  let openCloseEvsMode2 = leftmost [mode1Ev, () <$ mode2Ev, mode1And2FromFromUTCTimeWidget]
  dynBoolMode1 <- toggle False openCloseEvsMode1
  dynBoolMode2 <- toggle False openCloseEvsMode2
  let mode1Attrs = calendaContainerAttrs <$> dynBoolMode1
  let mode2Attrs = dateLabelAttrs <$> dynBoolMode2
  mode2Ev <- dateAndTimeMode2InLocalTime mode1Attrs zt -- m Event t ZonedTime in local time -- this needs a holdDyn?
  mode1Ev <- elDynAttr "div" mode2Attrs $ displayLocalTimeWidget mode2Ev zt -- m (Event t ())
  return (mode2Ev, mode1Ev)-- mode2Ev -- evenrt t Zoned Time

-- 2. We need another (sub)widget utcTimeWidget :: Dynamic t ZonedTime -> m (Event t ZonedTime) - it displays the time in UTC terms, displays the +1 day when necessary, and has a way of being clicked on to pick a date (in which case, the date is being picked in universal terms).

utcTimeWidgetRefactored :: MonadWidget t m =>  Dynamic t ZonedTime -> Event t () -> W t m (Event t ZonedTime, Event t ()) --  m (Event t ZonedTime)
utcTimeWidgetRefactored  zt mode1And2FromFromLocalTimeWidget = mdo
  let openCloseEvsMode1 = leftmost [mode1Ev, () <$ mode2Ev]
  let openCloseEvsMode2 = leftmost [mode1Ev, () <$ mode2Ev, mode1And2FromFromLocalTimeWidget]
  dynBoolMode1 <- toggle False openCloseEvsMode1
  dynBoolMode2 <- toggle False openCloseEvsMode2
  let mode1Attrs = calendaContainerAttrs <$> dynBoolMode1
  let mode2Attrs = dateLabelAttrs <$> dynBoolMode2
  mode2Ev <- dateAndTimeMode2UTCTime mode1Attrs zt -- m Event t ZonedTime in utc time -- this needs a holdDyn?
  mode1Ev <- elDynAttr "div" mode2Attrs $ displayUTCTimeWidget mode2Ev zt -- m (Event t ())
  return (mode2Ev, mode1Ev) -- event t Zoned Time

-- 3. Then bringing them together is straightforward and should just use the generic mergeDeltas defined above.
mergeDeltas :: MonadWidget t m => Dynamic t ZonedTime -> Event t ZonedTime -> W t m (Dynamic t ZonedTime)
mergeDeltas delta edits  = do
  iVal <- sample $ current delta
  holdDyn iVal $ leftmost [edits,updated delta]

utcTimeOrLocalTimeWidget :: MonadWidget t m => Dynamic t ZonedTime -> W t m (Event t ZonedTime)
utcTimeOrLocalTimeWidget delta = mdo
  deltaFor1 <- mergeDeltas delta localEdits2
  deltaFor2 <- mergeDeltas delta localEdits1
  deltaFor3 <- mergeDeltas delta $ leftmost [localEdits1, localEdits2]
  elClass' "div" "showDate" $ tooltip (showSelectedDate deltaFor3) (text "Click on the dropdown menus to modify the date and time")
  (localEdits1, localEdits2) <- divClass "timeWidgetsContainer" $ mdo
    (localEdits1', clickEvFromLocalTimeWidget) <- localTimeWidgetRefactored deltaFor1 $ leftmost [clickEvFromUTCTimeWidget, () <$ localEdits2']
    (localEdits2', clickEvFromUTCTimeWidget) <-  utcTimeWidgetRefactored deltaFor2 $ leftmost [clickEvFromLocalTimeWidget, () <$ localEdits1']
    return $ (localEdits1', localEdits2')
  return $ leftmost [localEdits1,localEdits2]

showSelectedDate :: MonadWidget t m => Dynamic t ZonedTime -> W t m ()
showSelectedDate delta = do
  let selDate = fmap (T.pack . show . localDay . zonedTimeToLocalTime) delta
  dynText selDate


dateWidget ::  MonadWidget t m => Dynamic t ZonedTime -> W t m (Event t Day)
dateWidget zt = mdo
  thisComputerDay <- zonedDayToThisComputerDay zt
  let openCloseEvs = leftmost [mode1Ev, () <$ mode2Ev]
  dynBool <- toggle False openCloseEvs
  let mode1Attrs = calendaContainerAttrs <$> dynBool
  let mode2Attrs = dateLabelAttrs <$> dynBool
  mode2Ev <- dateWidgetMode2Ev mode1Attrs thisComputerDay
  mode1Ev <- elDynAttr "div" mode2Attrs $ dateWidgetMode1 selDate -- m (Event t ())
  today <- (sample $ current thisComputerDay)
  let updatedDay = leftmost [mode2Ev, updated thisComputerDay]
  selDate <- holdDyn today updatedDay -- mode2Ev -- Dynamic t Day
  return mode2Ev


dayAndTimeWidgetMode1 ::  MonadWidget t m =>  Dynamic t ZonedTime -> W t m (Event t ())
dayAndTimeWidgetMode1 zt = mdo
  let zt' = fmap (T.pack . show) zt
  (openPicker, _) <- elClass' "div" "code-font background selectedDate" $ dynText zt'
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t calendarEvent)
  return openPicker'

--1. show separateley the date from the time

displayLocalTimeWidget ::  MonadWidget t m =>  Event t ZonedTime -> Dynamic t ZonedTime -> W t m (Event t ())
displayLocalTimeWidget ztEv zt = mdo
  thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
  let localTime = fmap (utcToZonedTime thisComputerZone . zonedTimeToUTC) zt
  sampledLocalTime <- sample $ current localTime-- Dynamic t Zoned time in local time
  selLocalTime <- holdDyn sampledLocalTime $ leftmost [ztEv, updated localTime] -- mode2Ev -- Dynamic t zonedTime
  let selTimeOfDay =  fmap (localTimeOfDay . zonedTimeToLocalTime) selLocalTime -- Dynamic TimeOfDay
  let selTimeZone = fmap (zonedTimeZone) selLocalTime -- Dynamic TimeZone
  let showTimeOfDayWithoutSecAndMillisec' = showTimeOfDayWithoutSecAndMillisec <$> selTimeOfDay <*> selTimeZone -- Dynamic text
  dayAdj <- dayAdjustmentWidgetForLocalWidget ztEv zt
  (openPicker, _) <- elClass' "div" "code-font background selectedDate" $ tooltip (divClass "zoneWidget" $ dynText $ showTimeOfDayWithoutSecAndMillisec' <> " " <> dayAdj) (text "local time")  -- dynText $ selLocalTime' <> " " <> dayAdj
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t calendarEvent)
  return openPicker'

dayAdjustmentWidgetForLocalWidget :: MonadWidget t m =>  Event t ZonedTime -> Dynamic t ZonedTime -> W t m (Dynamic t Text)
dayAdjustmentWidgetForLocalWidget ztEv zt = mdo
  thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
  let localTime = fmap (utcToZonedTime thisComputerZone . zonedTimeToUTC) zt -- dyn zonedTime in localTime
  sampledLocalTime <- sample $ current localTime -- localTime
  let ztEvInLocalTime = fmap (utcToZonedTime thisComputerZone . zonedTimeToUTC) ztEv
  zt' <- holdDyn sampledLocalTime $ leftmost [ztEvInLocalTime, updated localTime]

  -- let utcTime = fmap zonedTimeToUTC zt
  -- sampledUTCTime <- sample $ current utcTime
  sampledZonedTime <- sample $ current zt
  utct <- holdDyn sampledZonedTime $ leftmost [ztEv, updated zt]

  return $ dayAdjustmentWidgetForLocalWidget' <$> zt'  <*> utct

dayAdjustmentWidgetForLocalWidget' :: ZonedTime -> ZonedTime -> Text
dayAdjustmentWidgetForLocalWidget' localT zonedT
  | zonedD == utcD = dayAdjustment''' localT zonedT
  | otherwise = ""
    where
      utcD = utctDay $ zonedTimeToUTC zonedT
      zonedD = localDay $ zonedTimeToLocalTime zonedT

dayAdjustment''' :: ZonedTime -> ZonedTime -> Text
dayAdjustment''' localT zonedT
  | localD > zonedD = "+" <> (T.pack $ show 1 <> "day")
  | localD < zonedD = (T.pack $ show (-1) <> "day")
  |otherwise = ""
    where
      zonedD = localDay $ zonedTimeToLocalTime zonedT
      localD = localDay $ zonedTimeToLocalTime localT

displayUTCTimeWidget ::  MonadWidget t m => Event t ZonedTime -> Dynamic t ZonedTime -> W t m (Event t ())
displayUTCTimeWidget ztEv zt = mdo
  sampledZTCurrent <- sample $ current zt
  let utcTimeCurrent = fmap zonedTimeToUTC zt --
  let utcTimeEv = fmap zonedTimeToUTC ztEv
  sampledUTCTime <- sample $ current utcTimeCurrent-- Dynamic t utcTime
  selUTCTime <- holdDyn sampledUTCTime $ leftmost [utcTimeEv, updated utcTimeCurrent] -- mode2Ev -- Dynamic t utcTime
  let selUTCTimeOfDay = fmap ( timeToTimeOfDay . utctDayTime) selUTCTime -- Dynamic TimeOfDay
  let showTimeOfDayWithoutSecAndMillisec' = fmap (\d -> showTimeOfDayWithoutSecAndMillisec d utc) selUTCTimeOfDay
  dayAdj <- dayAdjustmentWidget ztEv zt
  (openPicker, _) <- elClass' "div" "code-font background selectedDate" $ tooltip (divClass "changeUTCTimeWidgetText" $ dynText $ showTimeOfDayWithoutSecAndMillisec' <> " " <> dayAdj)(text "universal time")
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t calendarEvent)
  return openPicker'

  -- position: absolute;
  --   z-index: 1;

showTimeOfDayWithoutSecAndMillisec :: TimeOfDay -> TimeZone -> Text
showTimeOfDayWithoutSecAndMillisec d tz = do
  let timeWithFixedSecs = TimeOfDay (todHour d) (todMin d) (00)
  T.pack $ show timeWithFixedSecs <> " " <> (show tz)

dayAdjustmentWidget :: MonadWidget t m => Event t ZonedTime -> Dynamic t ZonedTime -> W t m (Dynamic t Text)
dayAdjustmentWidget ztEv zt = mdo
   thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
   let ztToUtcTime = fmap zonedTimeToUTC zt -- Dyn UTCTIme
   sampledZtToUtcTime <- sample $ current ztToUtcTime
   let ztEvToUTCTime = fmap zonedTimeToUTC  ztEv-- Event UTCTIme
   utct <- holdDyn sampledZtToUtcTime $ leftmost [ztEvToUTCTime, (updated ztToUtcTime)] -- Dyn UTCtime
   sampledZt <- sample $ current zt -- ZonedTime
   zt' <- holdDyn sampledZt $ leftmost [ztEv, (updated zt)] -- Dyn ZonedTime -- con este funciona bien todo!
   return $ dayAdjustment'' <$> zt'  <*> utct


dayAdjustment'' :: ZonedTime -> UTCTime -> Text
dayAdjustment'' zt utct
  | utcDay > ztDay = "+" <> (T.pack $ show 1 <> "day")
  | utcDay < ztDay = (T.pack $ show (-1) <> "day")
  |otherwise = ""
    where
      utcDay = utctDay utct
      ztDay = localDay $ zonedTimeToLocalTime zt



-- only displays; cant change the date
dateWidgetMode1 ::  MonadWidget t m =>  Dynamic t Day -> W t m (Event t ())
dateWidgetMode1 d = mdo
  let d' = fmap (T.pack . show) d
  (openPicker, _) <- elClass' "div" "code-font background selectedDate" $ dynText d'
  openPickerEv <- wrapDomEvent (_element_raw openPicker) (elementOnEventName Click) (mouseXY)
  let openPicker' = () <$ openPickerEv -- (Event t (Dynamic t calendarEvent)
  return openPicker'


zonedDayToThisComputerDay :: MonadWidget t m => Dynamic t ZonedTime -> W t m (Dynamic t Day)
zonedDayToThisComputerDay zt = do
  thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
  let zt' = fmap zonedTimeToUTC zt -- Dynamic UTCTime
  let utcToZonedTime' = fmap (utcToZonedTime thisComputerZone) zt' -- Dynamic t ZonedTime
  let thisComputerDay = fmap (localDay . zonedTimeToLocalTime) utcToZonedTime' -- Dynamic t Day
  return thisComputerDay

--
showZonedTimeWithoutSeconds :: ZonedTime -> Text
showZonedTimeWithoutSeconds zt = do
  let day = T.pack $ show $ localDay $ zonedTimeToLocalTime zt -- Text
  let hourOfDay = T.pack $ show $ todHour $ localTimeOfDay $ zonedTimeToLocalTime zt -- Text
  let minsOfDay = T.pack $ show $ todMin $ localTimeOfDay $ zonedTimeToLocalTime zt -- Text
  let zone = timeZoneOffsetTotimeZoneName $ zonedTimeZone zt-- Text
  day <> " " <> hourOfDay <> ":" <> minsOfDay <> " " <> zone

showUTCTimeWithoutSeconds :: UTCTime -> Text
showUTCTimeWithoutSeconds zt = do
  let day = T.pack $ show $ utctDay zt -- Text
  let hourOfDay = T.pack $ show $ todHour $ timeToTimeOfDay $ utctDayTime zt -- Text
  let minsOfDay = T.pack $ show $ todMin $ timeToTimeOfDay $ utctDayTime zt -- Text
  day <> " " <> "at" <> " " <> hourOfDay <> " : " <> minsOfDay <> " " <> "UTC" <> " " <> "(universal time)"


monthIndex :: Int -> Int
monthIndex i
  | i == (-1) = 11
  |otherwise = i

monthNames :: MonadWidget t m => Dynamic t Int -> W t m ()
monthNames i = do
  let i' = fmap monthIndex $ fmap ((+) (-1)) i  -- Dynamic t Int
  let monthName = ((!!) [M.fromList [(English,"January"), (Español, "Enero")], M.fromList [(English,"February"), (Español, "Febrero")], M.fromList [(English,"March"), (Español, "Marzo")], M.fromList [(English,"April"), (Español, "Abril")], M.fromList [(English,"May"), (Español, "Mayo")], M.fromList [(English,"June"), (Español, "Junio")], M.fromList [(English,"July"), (Español, "Julio")], M.fromList [(English,"August"), (Español, "Agosto")], M.fromList [(English,"September"), (Español, "Septiembre")], M.fromList [(English,"October"), (Español, "Octubre")], M.fromList [(English,"November"), (Español, "Noviembre")], M.fromList [(English,"December"), (Español, "Diciembre")]]) <$> i' -- Dynamic t Text
  el "tr" $ do
    el "th" $ dynText =<< (dynTranslatableText $ monthName)
  return ()

navMonthCount :: Int -> Int
navMonthCount c
  |mod c 12 == 0 = 12
  |otherwise = mod c 12

selectMonth :: MonadWidget t m => Int -> W t m (Dynamic t Int)
selectMonth month = divClass "selectMonth" $ mdo
  leftArrow <- clickableDivClass' "<" "CalendarLeftArrow" (-1)
  dynMonthName <- monthNames dynMonth -- $ fmap ((+) (-1)) dynMonth
  rightArrow <- clickableDivClass' ">" "CalendarRightArrow" 1
  let a = leftmost [leftArrow, rightArrow] -- Event t Int
  count <- foldDyn (+) (month :: Int)  a -- Dynamic Int
  let dynMonth = fmap ((flip mod) 12) count -- monthCount <$> month <*> count   -- Dynamic Int
  let dynMonth' = fmap navMonthCount dynMonth -- monthCount <$> month <*> count   -- Dynamic Int
  -- divClass "" $ dynText (fmap (T.pack . show) dynMonth)
  return dynMonth'

selectYear :: MonadWidget t m => Integer -> W t m (Dynamic t Integer)
selectYear year = divClass "selectYear" $ mdo
  leftArrow <- clickableDivClass' "<" "CalendarLeftArrow" (-1)
  showDynYear <- elClass "th" "" $ dynText (fmap (T.pack . show) year')
  rightArrow <- clickableDivClass' ">" "CalendarRightArrow" 1
  let a = leftmost [leftArrow, rightArrow] -- Event t Int
  year' <- foldDyn (+) (year :: Integer)  a -- Dynamic Int
  -- divClass "" $ dynText (fmap (T.pack . show) dynMonth)
  return year'

dateAndTimeMode2InLocalTime :: MonadWidget t m => Dynamic t (Map Text Text) -> Dynamic t ZonedTime -> W t m (Event t ZonedTime)
dateAndTimeMode2InLocalTime dynAttrs zt = mdo
  thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
  let localTime = fmap (utcToZonedTime thisComputerZone . zonedTimeToUTC) zt -- Dynamic t Zoned time in local time
  let localDay' = fmap (localDay . zonedTimeToLocalTime) localTime -- Dynamic t localday
  let localTimeOfDay' = fmap (localTimeOfDay . zonedTimeToLocalTime) localTime -- Dynamic t local time of day
  let localTimeZone = fmap zonedTimeZone localTime -- Dynamic t local time zone
  sampledLocalTimeZone <- sample $ current localTimeZone
  elDynAttr "div" dynAttrs $ dynText =<< (translatableText $ M.fromList [(English,"Local Time"), (Español, "Hora Local")])
  date <- dateWidgetMode2Dyn dynAttrs localDay' -- Dyn t day
  time <- elDynAttr "div" dynAttrs $ divClass "timeWidget" $ timeWidgetDyn (T.pack $ show sampledLocalTimeZone) localTimeOfDay'
  let makeLocalTime = makeZonedTime <$> date <*> time <*> localTimeZone-- dynamic zonedTime

  (confirmDateButton, closeCalendarButton)  <- divClass "confirm-close-buttons" $ do
    confirmDate' <- elDynAttr "div" dynAttrs $ clickableDiv "confirmButton" $ dynText =<< (translatableText $ M.fromList [(English,"confirm"), (Español, "confirmar")])
    closeCalendar' <- elDynAttr "div" dynAttrs $ clickableDiv "cancelButton" $ dynText =<< (translatableText $ M.fromList [(English,"cancel"), (Español, "cancelar")])
    return (confirmDate', closeCalendar') -- (Event t ())

  let confirmDate = tag (current makeLocalTime) confirmDateButton -- Dynamic a -> Event b -> Event a

  sampledCurrentLocalTime <- sample $ current localTime
  updatedLocalTime <- holdDyn sampledCurrentLocalTime $ leftmost [confirmDate, updated localTime] -- dyn Day

  let closeCalendar = tag (current updatedLocalTime) closeCalendarButton -- Dynamic a -> Event b -> Event a
  return $ leftmost [confirmDate, closeCalendar]


dateAndTimeMode2UTCTime :: MonadWidget t m => Dynamic t (Map Text Text) -> Dynamic t ZonedTime -> W t m (Event t ZonedTime)
dateAndTimeMode2UTCTime dynAttrs zt = mdo
  thisComputerZone <- liftIO getCurrentTimeZone -- TimeZone
  let utcTime = fmap zonedTimeToUTC zt -- Dynamic t time in utc time
  let utcDay' = fmap utctDay utcTime -- Dynamic t utcday
  let utcTimeOfDay' = fmap (timeToTimeOfDay . utctDayTime) utcTime -- Dynamic t utc time of day
  let makeUTCTime = makeZonedTime  <$> utcDay' <*> utcTimeOfDay' <*> (constDyn utc)
  elDynAttr "div" dynAttrs $ dynText =<< (translatableText $ M.fromList [(English,"Universal Time"), (Español, "Hora Universal")])
  date <- dateWidgetMode2Dyn dynAttrs utcDay' -- Dyn t day
  time <- elDynAttr "div" dynAttrs $ divClass "timeWidget" $ timeWidgetDyn "UTC" utcTimeOfDay'
  let makeUpdatedUTCTime = makeZonedTime <$> date <*> time <*> (constDyn utc) -- dynamic zonedTime

  (confirmDateButton, closeCalendarButton)  <- divClass "confirm-close-buttons" $ do
    confirmDate' <- elDynAttr "div" dynAttrs $ clickableDiv "confirmButton" $ dynText =<< (translatableText $ M.fromList [(English,"confirm"), (Español, "confirmar")])
    closeCalendar' <- elDynAttr "div" dynAttrs $ clickableDiv "cancelButton" $ dynText =<< (translatableText $ M.fromList [(English,"cancel"), (Español, "cancelar")])
    return (confirmDate', closeCalendar') -- (Event t ())

  let confirmDate = tag (current makeUpdatedUTCTime) confirmDateButton -- Dynamic a -> Event b -> Event a

  sampledCurrentUTCTime <- sample $ current makeUTCTime
  updatedUTCTime <- holdDyn sampledCurrentUTCTime $ leftmost [confirmDate, updated makeUTCTime] -- dyn Day

  let closeCalendar = tag (current updatedUTCTime) closeCalendarButton -- Dynamic a -> Event b -> Event a
  return $ leftmost [confirmDate, closeCalendar]


makeZonedTime :: Day -> TimeOfDay -> TimeZone -> ZonedTime
makeZonedTime d t z = ZonedTime	(LocalTime	d t) z

dateWidgetMode2Dyn :: MonadWidget t m => Dynamic t (Map Text Text) -> Dynamic t Day -> W t m (Dynamic t Day)
dateWidgetMode2Dyn dynAttrs dynDate = mdo
  today <- liftIO getZonedTime
  serversDate <- sample $ current dynDate
  let todaysYear = getYearFromDay $ localDay $ zonedTimeToLocalTime today -- Integer
  let todaysMonth = getMonthFromDay $ localDay $ zonedTimeToLocalTime today -- Int
  (selectedYear, selectedMonth, selectedDay) <- elDynAttr "div" dynAttrs $ do
    (selectedYear', selectedMonth', firstDayOfSelMonthBool') <- divClass "monthAndYear" $ do
      selectedMonth'' <- selectMonth todaysMonth -- Dyn Int
      selectedYear'' <- selectYear todaysYear -- Dyn Integer
      let dayToGregorian = fromGregorian <$> selectedYear'' <*> selectedMonth'' <*> (constDyn 1)
      let initialDayOfSelMonth =  dayOfWeek <$> dayToGregorian
      let firstDayOfSelMonthBool = dayOfWeekBool <$> initialDayOfSelMonth
      return (selectedYear'', selectedMonth'', firstDayOfSelMonthBool)

    let listOfDays' = listOfDays <$> selectedYear' <*> selectedMonth' -- [Dynamic Day]
    holdDynListOfDays <- holdDyn (listOfDays todaysYear todaysMonth) (updated listOfDays')
    let matchSelDayWithCalendarDay = matchSelDay <$> holdDynListOfDays <*> date -- Dynamic t [Bool]
    backgroundDynBool <- holdDyn (replicate 31 False) $ updated matchSelDayWithCalendarDay -- Dynamic [Bool]
    let highlightSelectedDayAttrs' = fmap highlightSelectedDayAttrsList backgroundDynBool -- Dynamic t [Map Text Text]

    selectedDay''' <- el "div"  $ do
      selectedDay'' <- elClass "div" "dayNumbers" $ do

        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Su"), (Español, "Do")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Mo"), (Español, "Lu")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Tu"), (Español, "Ma")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "We"), (Español, "Mi")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Th"), (Español, "Ju")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Fr"), (Español, "Vi")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Sa"), (Español, "Sa")])

        sun <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 1) firstDayOfSelMonthBool'
        mon <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 2) firstDayOfSelMonthBool'
        tue <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 3) firstDayOfSelMonthBool'
        wed <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 4) firstDayOfSelMonthBool'
        thu <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 5) firstDayOfSelMonthBool'
        fri <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 6) firstDayOfSelMonthBool'
        sat <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 7) firstDayOfSelMonthBool'

        a <- elDynAttr "div" (fmap (\x -> x !! 0) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 1
        b <- elDynAttr "div" (fmap (\x -> x !! 1) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 2
        c <- elDynAttr "div" (fmap (\x -> x !! 2) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 3
        d <- elDynAttr "div" (fmap (\x -> x !! 3) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 4
        e <- elDynAttr "div" (fmap (\x -> x !! 4) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 5
        f <- elDynAttr "div" (fmap (\x -> x !! 5) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 6
        g <- elDynAttr "div" (fmap (\x -> x !! 6) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 7
        h <- elDynAttr "div" (fmap (\x -> x !! 7) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 8
        i <- elDynAttr "div" (fmap (\x -> x !! 8) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 9
        j <- elDynAttr "div" (fmap (\x -> x !! 9) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 10
        k <- elDynAttr "div" (fmap (\x -> x !! 10) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 11
        l <- elDynAttr "div" (fmap (\x -> x !! 11) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 12
        m <- elDynAttr "div" (fmap (\x -> x !! 12) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 13
        n <- elDynAttr "div" (fmap (\x -> x !! 13) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 14
        o <- elDynAttr "div" (fmap (\x -> x !! 14) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 15
        p <- elDynAttr "div" (fmap (\x -> x !! 15) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 16
        q <- elDynAttr "div" (fmap (\x -> x !! 16) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 17
        r <- elDynAttr "div" (fmap (\x -> x !! 17) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 18
        s <- elDynAttr "div" (fmap (\x -> x !! 18) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 19
        t <- elDynAttr "div" (fmap (\x -> x !! 19) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 20
        u <- elDynAttr "div" (fmap (\x -> x !! 20) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 21
        v <- elDynAttr "div" (fmap (\x -> x !! 21) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 22
        w <- elDynAttr "div" (fmap (\x -> x !! 22) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 23
        x <- elDynAttr "div" (fmap (\x -> x !! 23) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 24
        y <- elDynAttr "div" (fmap (\x -> x !! 24) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 25
        z <- elDynAttr "div" (fmap (\x -> x !! 25) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 26
        a' <- elDynAttr "div" (fmap (\x -> x !! 26) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 27
        b' <- elDynAttr "div" (fmap (\x -> x !! 27) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 28
        c' <- elDynAttr "div" (fmap (\x -> x !! 28) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 29
        d' <- elDynAttr "div" (fmap (\x -> x !! 29) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 30
        e' <- elDynAttr "div" (fmap (\x -> x !! 30) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 31
        return $ leftmost [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a', b', c', d', e'] --  (Event t Int)
      return selectedDay''
    return (selectedYear', selectedMonth', selectedDay''')
  let dayFromServer = getDayFromDay serversDate
  selectedDay' <- holdDyn dayFromServer selectedDay -- Dynamic t a
  let date = fromGregorian <$> selectedYear <*> selectedMonth <*>  selectedDay' -- Dynamic Day
  return date -- Dynamic t Day

dateWidgetMode2Ev :: MonadWidget t m => Dynamic t (Map Text Text) -> Dynamic t Day -> W t m (Event t Day)
dateWidgetMode2Ev dynAttrs dynDate = mdo
  today <- liftIO getZonedTime
  serversDate <- sample $ current dynDate
  let todaysYear = getYearFromDay $ localDay $ zonedTimeToLocalTime today -- fmap getYearFromDay dynDate
  let todaysMonth = getMonthFromDay $ localDay $ zonedTimeToLocalTime today -- fmap getMonthFromDay dynDate -- Dyn Int

  (selectedYear, selectedMonth, selectedDay) <- elDynAttr "div" dynAttrs $ mdo
    (selectedYear', selectedMonth', firstDayOfSelMonthBool') <- divClass "monthAndYear" $ mdo
      selectedMonth'' <- selectMonth todaysMonth -- Dyn Int
      selectedYear'' <- selectYear todaysYear -- Dyn Integer
      let dayToGregorian = fromGregorian <$> selectedYear'' <*> selectedMonth'' <*> (constDyn 1)
      let initialDayOfSelMonth =  dayOfWeek <$> dayToGregorian
      let firstDayOfSelMonthBool = dayOfWeekBool <$> initialDayOfSelMonth
      return (selectedYear'', selectedMonth'', firstDayOfSelMonthBool)

    let listOfDays' = listOfDays <$> selectedYear' <*> selectedMonth' -- [Dynamic Day]
    holdDynListOfDays <- holdDyn (listOfDays todaysYear todaysMonth ) (updated listOfDays')

    let matchSelDayWithCalendarDay = matchSelDay <$> holdDynListOfDays <*> date -- Dynamic t [Bool]
    backgroundDynBool <- holdDyn (replicate 31 False) $ updated matchSelDayWithCalendarDay -- Dynamic [Bool]
    let highlightSelectedDayAttrs' = fmap highlightSelectedDayAttrsList backgroundDynBool -- Dynamic t [Map Text Text]

    selectedDay''' <- el "div"  $ do
      selectedDay'' <- elClass "div" "dayNumbers" $ do

        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Su"), (Español, "Do")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Mo"), (Español, "Lu")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Tu"), (Español, "Ma")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "We"), (Español, "Mi")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Th"), (Español, "Ju")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Fr"), (Español, "Vi")])
        el "div" $ dynText =<< (translatableText $ M.fromList [(English, "Sa"), (Español, "Sa")])


        sun <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 1) firstDayOfSelMonthBool'
        mon <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 2) firstDayOfSelMonthBool'
        tue <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 3) firstDayOfSelMonthBool'
        wed <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 4) firstDayOfSelMonthBool'
        thu <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 5) firstDayOfSelMonthBool'
        fri <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 6) firstDayOfSelMonthBool'
        sat <- firstDayOfMonth $ fmap (getBoolFromBoolTuple 7) firstDayOfSelMonthBool'

        a <- elDynAttr "div" (fmap (\x -> x !! 0) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 1
        b <- elDynAttr "div" (fmap (\x -> x !! 1) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 2
        c <- elDynAttr "div" (fmap (\x -> x !! 2) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 3
        d <- elDynAttr "div" (fmap (\x -> x !! 3) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 4
        e <- elDynAttr "div" (fmap (\x -> x !! 4) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 5
        f <- elDynAttr "div" (fmap (\x -> x !! 5) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 6
        g <- elDynAttr "div" (fmap (\x -> x !! 6) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 7
        h <- elDynAttr "div" (fmap (\x -> x !! 7) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 8
        i <- elDynAttr "div" (fmap (\x -> x !! 8) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 9
        j <- elDynAttr "div" (fmap (\x -> x !! 9) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 10
        k <- elDynAttr "div" (fmap (\x -> x !! 10) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 11
        l <- elDynAttr "div" (fmap (\x -> x !! 11) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 12
        m <- elDynAttr "div" (fmap (\x -> x !! 12) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 13
        n <- elDynAttr "div" (fmap (\x -> x !! 13) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 14
        o <- elDynAttr "div" (fmap (\x -> x !! 14) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 15
        p <- elDynAttr "div" (fmap (\x -> x !! 15) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 16
        q <- elDynAttr "div" (fmap (\x -> x !! 16) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 17
        r <- elDynAttr "div" (fmap (\x -> x !! 17) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 18
        s <- elDynAttr "div" (fmap (\x -> x !! 18) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 19
        t <- elDynAttr "div" (fmap (\x -> x !! 19) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 20
        u <- elDynAttr "div" (fmap (\x -> x !! 20) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 21
        v <- elDynAttr "div" (fmap (\x -> x !! 21) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 22
        w <- elDynAttr "div" (fmap (\x -> x !! 22) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 23
        x <- elDynAttr "div" (fmap (\x -> x !! 23) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 24
        y <- elDynAttr "div" (fmap (\x -> x !! 24) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 25
        z <- elDynAttr "div" (fmap (\x -> x !! 25) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 26
        a' <- elDynAttr "div" (fmap (\x -> x !! 26) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 27
        b' <- elDynAttr "div" (fmap (\x -> x !! 27) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 28
        c' <- elDynAttr "div" (fmap (\x -> x !! 28) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 29
        d' <- elDynAttr "div" (fmap (\x -> x !! 29) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 30
        e' <- elDynAttr "div" (fmap (\x -> x !! 30) highlightSelectedDayAttrs') $ selDay selectedYear' selectedMonth' 31
        return $ leftmost [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z, a', b', c', d', e'] --  (Event t Int)
      return selectedDay''
    return (selectedYear', selectedMonth', selectedDay''')
  let dayFromServer = getDayFromDay serversDate
  selectedDay' <- holdDyn dayFromServer selectedDay -- Dynamic t a
  let date = fromGregorian <$> selectedYear <*> selectedMonth <*>  selectedDay' -- Dynamic Day

  (confirmDate, closeCalendar)  <- divClass "confirm-close-buttons" $ do
    (confirmDate', _) <- elDynAttr' "div" dynAttrs $ dynText =<< (translatableText $ M.fromList [(English,"confirm"), (Español, "confirmar")])
    (closeCalendar', _) <- elDynAttr' "div" dynAttrs $ dynText =<< (translatableText $ M.fromList [(English,"cancel"), (Español, "cancelar")])
    return (confirmDate', closeCalendar')

  confirmDateEv <- wrapDomEvent (_element_raw confirmDate) (elementOnEventName Click) (mouseXY)
  let confirmDate' = tagPromptlyDyn date confirmDateEv -- Dynamic a -> Event b -> Event a
  currentDate <- holdDyn serversDate $ leftmost [confirmDate', updated dynDate]

  closeCalendarEv <- wrapDomEvent (_element_raw closeCalendar) (elementOnEventName Click) (mouseXY)
  let closeCalendar' = tagPromptlyDyn currentDate closeCalendarEv -- Dynamic a -> Event b -> Event a
  return $ leftmost [confirmDate', closeCalendar']

listOfDays :: Integer -> Int -> [Day]
listOfDays year month = fmap (\d -> fromGregorian year month d) [1 .. 31]

matchSelDay :: [Day] -> Day -> [Bool]
matchSelDay xs d = fmap (matchSelDay' d) xs -- [Bool]

matchSelDay' :: Day -> Day -> Bool
matchSelDay' date1 date2
  | (date1 == date2) = True
  | otherwise = False

selDay :: MonadWidget t m => Dynamic t Integer -> Dynamic t Int -> Int -> W t m (Event t Int)
selDay year month selectableDay = mdo
  let dynAttrs = (howManyDaysInTheMonth selectableDay) <$> month <*> year
  let day = T.pack $ show selectableDay
  aEv <- clickableDivDynAttrs day selectableDay dynAttrs
  let selDay = selectableDay <$ aEv --  Event t Int
  return selDay

highlightSelectedDayAttrsList :: [Bool] -> [Map.Map T.Text T.Text]
highlightSelectedDayAttrsList xs = fmap highlightSelectedDayAttrs xs -- [Map.Map T.Text T.Text]

highlightSelectedDayAttrs :: Bool -> Map.Map T.Text T.Text
highlightSelectedDayAttrs True = "style" =: "background-color: var(--secondary-color);"
highlightSelectedDayAttrs _ = "style" =: ""

firstDayOfMonth :: MonadWidget t m => Dynamic t Bool -> W t m ()
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
    display True = "class" =: "dayLabel"
    display _    = "class" =: "dayLabel" <> "style" =: "display: none"

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
  | (day > gregorianMonthLength year month) = "class" =: "dayLabel" <> "style" =: "visibility: hidden;"
  | otherwise = "class" =: "dayLabel" <> "style" =: "visibility: visible;"

makeTuple :: TimeOfDay -> Bool -> (TimeOfDay, Bool)
makeTuple t b = (t, b)


showDayAdjustmentUTC :: MonadWidget t m => Event t TimeOfDay -> Dynamic t ZonedTime  -> W t m ()
showDayAdjustmentUTC tdEv ztCurrent = mdo
  thisComputerZone <- liftIO getCurrentTimeZone
  sampledZTCurrent <- sample $ current ztCurrent
  let sampledTimeZoneCurrent = zonedTimeZone sampledZTCurrent
  let sampledTimeOfDayCurrent = localTimeOfDay $ zonedTimeToLocalTime sampledZTCurrent
  let sampledDayAdjCurrent = T.pack $ show $ fst $ localToUTCTimeOfDay sampledTimeZoneCurrent sampledTimeOfDayCurrent
  let getDayAdjustmentEv = fmap (T.pack . show . fst . localToUTCTimeOfDay thisComputerZone) tdEv -- Event t text
  let tzCurrent = fmap zonedTimeZone ztCurrent
  let tdCurrent = fmap (localTimeOfDay . zonedTimeToLocalTime) ztCurrent
  let localToUTCTimeOfDay' = localToUTCTimeOfDay <$> tzCurrent <*> tdCurrent
  let getDayAdjustmentDyn = fmap (T.pack . show . fst) localToUTCTimeOfDay'
  dayAdjustmentDyn <- holdDyn sampledDayAdjCurrent $ leftmost [getDayAdjustmentEv, (updated getDayAdjustmentDyn)]
  divClass "dayAdjustmentStartingTime" $ dynText dayAdjustmentDyn


timeWidget ::  MonadWidget t m => Dynamic t TimeOfDay -> W t m (Event t TimeOfDay)
timeWidget td = do
  let hours = fmap todHour td -- Dynamic t Int
  let mins = fmap todMin td -- Dynamic t Int
  sampleHours <- sample $ current hours
  sampleMins <- sample $ current mins
  dynText =<< (translatableText $ M.fromList [(English,"hh"), (Español, "mm")])
  h <- divClass "timeWidgetInput" $ intTextInputW hours -- Event t Int --
  divClass "timeWidgetColon" $ text ":"
  m <- divClass "timeWidgetInput" $ intTextInputW mins -- Event t Int --
  hDyn <- holdDyn sampleHours h
  mDyn <- holdDyn sampleMins m
  let newTimeOfDay = updatedTimeOfDay <$> hDyn <*> mDyn  -- Event t TimeOfDay
  return $ updated newTimeOfDay


timeWidgetDyn ::  MonadWidget t m => Text -> Dynamic t TimeOfDay -> W t m (Dynamic t TimeOfDay)
timeWidgetDyn tz td  = do
  let hours = fmap todHour td -- Dynamic t Int
  let mins = fmap todMin td -- Dynamic t Int
  sampleHours <- sample $ current hours
  sampleMins <- sample $ current mins
  divClass "hoursAndminsLabelContainer" $ do
    divClass "hoursAndminsLabel" $ el "th" $ dynText =<< (translatableText $ M.fromList [(English,"hh"), (Español, "mm")])
    divClass "hoursAndminsLabel" $ el "th" $ dynText =<< (translatableText $ M.fromList [(English,"hh"), (Español, "mm")])
    divClass "hoursAndminsLabel" $ el "th" $ dynText =<< (translatableText $ M.fromList [(English,"time zone"), (Español, "Zona horaria")])
  (h, m) <- divClass "timeWidgetInputContainer" $ do
    h' <- divClass "timeWidgetInput" $ intTextInputW (constDyn sampleHours) -- (constDyn 00) -- Event t Int --
    divClass "timeWidgetColon" $ text ":"
    m' <- divClass "timeWidgetInput" $ intTextInputW (constDyn sampleMins) -- (constDyn 00) -- Event t Int --
    divClass "timeWidgetInput" $ text tz
    return $ (h', m')
  hDyn <- holdDyn sampleHours h
  mDyn <- holdDyn sampleMins m
  let newTimeOfDay = updatedTimeOfDay <$> hDyn <*> mDyn  -- Event t TimeOfDay
  return newTimeOfDay

updatedTimeOfDay :: Int -> Int -> TimeOfDay
updatedTimeOfDay h m = do
  let h' | h < 0 = (00 :: Int)
         | h > 23 = (23 :: Int)
         | otherwise  = h
  let m' | m < 0 = (00 :: Int)
         | m > 59 = (59 :: Int)
         | otherwise  = m
  TimeOfDay h' m' (00 :: F.Pico)


getStartingDateFromCalendarEv :: CalendarEvent -> ZonedTime
getStartingDateFromCalendarEv (CalendarEvent details (CalendarTime startingDate recurrence)) = startingDate

getEndDateFromCalendarEv :: CalendarEvent -> ZonedTime
getEndDateFromCalendarEv (CalendarEvent details (CalendarTime startingDate (Recurrence periodicity endDate))) = endDate

getDetailsFromCalendarEv :: CalendarEvent -> Text
getDetailsFromCalendarEv (CalendarEvent details calendarTime) = details

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

-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = F.mod' (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d
