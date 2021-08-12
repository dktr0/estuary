{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.StopWatch where

import Reflex
import Reflex.Dom
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Control.Monad
import Control.Monad.IO.Class

import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Types.Definition


-- from Estuary.Types.Definition.hs:
--   type StopWatch = Either (Maybe NominalDiffTime) UTCTime

stopWatchWidget :: MonadWidget t m => Dynamic t StopWatch -> W t m (Variable t StopWatch)
stopWatchWidget deltasDown = mdo

  -- 1. Translate button presses into localChanges (Event t StopWatch)
  x <- button "button"
  let y = tag (current $ currentValue v) x
  localChanges <- performEvent $ fmap (liftIO . stopWatchToNextState) y

  -- 2. Calculate and display text
  widgetBuildTime <- liftIO $ getCurrentTime
  initialStopWatch <- sample $ current deltasDown
  let initialText = stopWatchToText initialStopWatch widgetBuildTime
  tick <- tickLossy 0.050 widgetBuildTime -- :: Event t UTCTime
  let textUpdates = attachWith stopWatchToText (current $ currentValue v) $ fmap _tickInfo_lastUTC tick
  holdDyn initialText textUpdates >>= dynText

  v <- returnVariable deltasDown localChanges
  return v

-- attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c 

stopWatchToNextState :: StopWatch -> IO StopWatch
-- this function is used to transition between the three states of the stopwatch
-- as this sometimes requires checking the current time, the computation is in IO
-- A. If stop watch is stopped at 0:00 then it starts:
stopWatchToNextState (Left Nothing) = do
  now <- getCurrentTime
  return $ Right now
-- B. If stop watch is counting then it stops:
stopWatchToNextState (Right startTime) = do
  now <- getCurrentTime
  return $ Left $ Just $ diffUTCTime now startTime
-- C. If stop watch is stopped at x:yy then it goes back to 0:
stopWatchToNextState (Left (Just _)) = return (Left Nothing)


stopWatchToText :: StopWatch -> UTCTime -> Text
stopWatchToText (Left Nothing) _ = diffTimeToText 0
stopWatchToText (Right startTime) now = diffTimeToText $ diffUTCTime now startTime
stopWatchToText (Left (Just ndt)) _ = diffTimeToText ndt

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)
