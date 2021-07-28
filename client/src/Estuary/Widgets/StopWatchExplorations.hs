{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.StopWatchExplorations where

import Reflex
import Reflex.Dom
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Control.Monad
import Control.Monad.IO.Class

import Estuary.Widgets.Editor
import Estuary.Types.Variable
import Estuary.Types.Definition
import Estuary.Reflex.Utility
import Estuary.Widgets.Text

-- fix the text input first!!!
-- mind definitions!! Think about types: TimerUp and Down
-- still the button does not work!
-- check threads in discord

-- from Estuary.Types.Definition.hs:
--   type StopWatch = Either (Maybe NominalDiffTime) UTCTime

stopWatchWidget' :: MonadWidget t m => Dynamic t Counter -> Editor t m (Variable t Counter)
stopWatchWidget' deltasDown = mdo
  -- 1. Translate button presses into localChanges (Event t StopWatch)
  x <- dynButton $ dynSnd -- Event t Text
  -- x <- button "hola"  -- :: m (Event t ()) 
  let y = tag (current $ currentValue v) x -- current:: Dyn -> Behaviour -- behaviour and event, event fires, gets the val of the beha -- tag samples the behaviour at a particular time -- curr val at button pressed
  localChanges <- performEvent $ fmap (liftIO . countToNextState) y -- Event t StopW map through IO (that is why performEvent) so :: Event t 

  -- 2. Calculate and display text
  widgetBuildTime <- liftIO $ getCurrentTime  -- :: UTC (happens when widget is built)
  initialStopWatch <- sample $ current deltasDown     -- Behaviour t ?? (only happens when widget is built)
  let initialText = fst $ stopWatchToText initialStopWatch widgetBuildTime -- calculated once :: Text
  tick <- tickLossy 0.050 widgetBuildTime -- :: tickInfo (next line is UTC)
  let textUpdates = attachWith stopWatchToText (current $ currentValue v) $ fmap _tickInfo_lastUTC tick -- :: Event t c
  holdDyn initialText (fmap fst textUpdates) >>= dynText

  dynSnd <- holdDyn "moo" $ fmap snd textUpdates -- transform Event t to Dynamic t (notice the <-, still in the IO monad (is that correct?)) and from (Tx,Tx) -> Tx
  v <- returnVariable deltasDown localChanges  -- delta remote edits, -- must be Editor t m (variable t StopWatch)
  return v


countToNextState :: Counter -> IO Counter
-- as this sometimes requires checking the current time, the computation is in IO
-- A. If stop watch is stopped at 0:00 then it starts:
countToNextState (Left Nothing) = do
  now <- getCurrentTime
  return $ Right now
-- B. If stop watch is counting then it stops:
countToNextState (Right startTime) = do
  now <- getCurrentTime
  return $ Left $ Just $ diffUTCTime now startTime
-- C. If stop watch is stopped at x:yy then it goes back to 0:
countToNextState (Left (Just _)) = return (Left Nothing)

            --  :: StopWatch -> UTCTime -> Text
stopWatchToText :: Counter -> UTCTime -> (Text, Text)
stopWatchToText (Left Nothing) _ = (diffTimeToText 0, "start")
stopWatchToText (Right startTime) now = (diffTimeToText $ diffUTCTime now startTime, "stop")
stopWatchToText (Left (Just ndt)) _ = (diffTimeToText ndt, "clear)")

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)


countDownToText:: NominalDiffTime -> Counter -> UTCTime -> (Text, Text)
countDownToText tMinus (Right startT) now = 
  let target = addUTCTime tMinus startT -- startTime + countdown
      count = diffUTCTime target now
  in if count > 0 then (diffTimeToText count,"stop") else (diffTimeToText 0, "stop")
countDownToText tMinus (Left Nothing) _ = (diffTimeToText tMinus, "start")
countDownToText tMinus (Left (Just ndt)) _ = (diffTimeToText (tMinus - ndt), "clear")


-- from Estuary.Types.Definition.hs:
--   type CountDown = Either (Maybe NominalDiffTime) UTCTime

countDownWidget :: MonadWidget t m => Dynamic t Counter -> Editor t m (Variable t Counter)
countDownWidget deltasDown = mdo
  let initialText = showt "t minus what?"
  let updatedText = showt "pim pum papas"
  (tValue,_,tEval) <- textWidget 1 (constDyn False) initialText updatedText
  -- 1. Translate button presses into localChanges (Event t StopWatch)
  -- x <- dynButton $ dynSnd -- Event t Text
  x <- button "t-30"  -- :: m (Event t ()) 
  let y = tag (current $ currentValue v) x -- current:: Dyn -> Behaviour -- behaviour and event, event fires, gets the val of the beha -- tag samples the behaviour at a particular time -- curr val at button pressed
  localChanges <- performEvent $ fmap (liftIO . countToNextState) y -- Event t StopW map through IO (that is why performEvent) so :: Event t 

  -- 2. Calculate and display text
  widgetBuildTime <- liftIO $ getCurrentTime  -- :: UTC (happens when widget is built)
  initialStopWatch <- sample $ current deltasDown     -- Behaviour t ?? (only happens when widget is built)
  let initialTime = fst $ countDownToText 30 initialStopWatch widgetBuildTime -- calculated once :: Text
  tick <- tickLossy 0.050 widgetBuildTime -- :: tickInfo (next line is UTC)
  let textUpdates = attachWith (countDownToText 30) (current $ currentValue v) $ fmap _tickInfo_lastUTC tick -- :: Event t c
  holdDyn initialTime (fmap fst textUpdates) >>= dynText

  dynSnd <- holdDyn "moo" $ fmap snd textUpdates -- transform Event t to Dynamic t (notice the <-, still in the IO monad (is that correct?)) and from (Tx,Tx) -> Tx
  v <- returnVariable deltasDown localChanges  -- delta remote edits, -- must be Editor t m (variable t StopWatch)
  return v


-- this is for a text field that becomes the countdown value

-- let initialText = showt (freq a)
--     let updatedText = fmap (showt . freq) eventA
--     (tValue,_,tEval) <- textWidget 1 (constDyn False) initialText updatedText