module Estuary.Types.ZonedTime where

import Data.Time

instance Eq ZonedTime where
   (ZonedTime zonedTimeToLocalTime1 zonedTimeZone1) == (ZonedTime zonedTimeToLocalTime2 zonedTimeZone2) = (zonedTimeToLocalTime1 == zonedTimeToLocalTime2) && (zonedTimeZone1 == zonedTimeZone2)


-- data ZonedTime = ZonedTime {
--   zonedTimeToLocalTime :: LocalTime
--   zonedTimeZone :: TimeZone
-- }

--  times that are actually the same but have different expressions because of different time zones, will be, in our instance, not equal
