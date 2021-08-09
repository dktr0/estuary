{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.StopWatchExplorations where

import Reflex
import Reflex.Dom
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read
import TextShow
import Control.Monad
import Control.Monad.IO.Class

import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Widgets.Text

-- import Estuary.Reflex.Utility

-- fix the text input first!!!
-- mind definitions!! Think about types: TimerUp and Down
-- still the button does not work!
-- check threads in discord

-- from Estuary.Types.Definition.hs:
--   type StopWatch = Either (Maybe NominalDiffTime) UTCTime

stopWatchWidget' :: MonadWidget t m => Dynamic t Clock -> W t m (Variable t Clock)
stopWatchWidget' deltasDown = mdo
  -- 1. Translate button presses into localChanges (Event t StopWatch)
  x <- dynButton $ dynSnd -- Event t Text
  -- x <- button "hola"  -- :: m (Event t ()) 
  let y = tag (current $ currentValue v) $ traceEvent "x" x -- current:: Dyn -> Behaviour -- behaviour and event, event fires, gets the val of the beha -- tag samples the behaviour at a particular time -- curr val at button pressed
  -- localChanges <- performEvent $ fmap (liftIO . countToNextState) $ traceEvent "y" y -- Event t StopW map through IO (that is why performEvent) so :: Event t 
  localChanges <- fmap (traceEvent "localChanges") $ performEvent $ fmap (liftIO . stopWatchToNextState) y
  -- 2. Calculate and display text
  widgetBuildTime <- liftIO $ getCurrentTime  -- :: UTC (happens when widget is built)
  initialStopWatch <- sample $ current deltasDown     -- Behaviour t ?? (only happens when widget is built)
  let initialText = fst $ stopWatchToText initialStopWatch widgetBuildTime -- calculated once :: Text
  v <- returnVariable deltasDown localChanges
  tick <- tickLossy 1 widgetBuildTime -- :: tickInfo (next line is UTC)
  -- let textUpdates = attachWith stopWatchToText vTemp $ fmap _tickInfo_lastUTC tick -- :: Event t c
  let textUpdates = traceEvent "textUpdates" $ attachWith stopWatchToText (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
  holdDyn initialText (fmap fst textUpdates) >>= dynText

  dynSnd <- holdDyn "stopwatch!" $ fmap snd textUpdates -- transform Event t to Dynamic t (notice the <-, still in the IO monad (is that correct?)) and from (Tx,Tx) -> Tx
  -- v <- returnVariable deltasDown localChanges  -- delta remote edits, -- must be Editor t m (variable t StopWatch)
  fakeDeltasDown <- holdDyn (initialStopWatch) never
  
  -- vTemp <- current <$> holdDyn initialStopWatch localChanges
  returnVariable deltasDown never

------ State calculations ----

stopWatchToNextState :: Clock -> IO Clock
stopWatchToNextState (TimerUp (Left Nothing)) = do
  now <- getCurrentTime
  return (TimerUp (Right now))
-- B. If stop watch is counting then it stops:
stopWatchToNextState (TimerUp (Right startTime)) = do
  now <- getCurrentTime
  return (TimerUp (Left $ Just $ diffUTCTime now startTime))
-- C. If stop watch is stopped at x:yy then it goes back to 0:
stopWatchToNextState (TimerUp (Left (Just _))) = return (TimerUp (Left Nothing))

countDownToNextState :: TimerDownState -> IO TimerDownState
countDownToNextState (Stopped tar) = do
  now <- getCurrentTime
  return (Running tar now)
countDownToNextState (Running tar y) = do
  return (Stopped tar)

assambleCountDown :: Int -> TimerDownState-> TimerDownState
assambleCountDown target (Stopped x) = (Stopped target) -- if count is stopped then it can assamble a new target
assambleCountDown target (Running x y) = (Running target y) -- if count is running no new target can be made

--- transform clocks to display text -------

            --  :: StopWatch -> UTCTime -> Text
stopWatchToText :: Clock -> UTCTime -> (Text, Text)
stopWatchToText (TimerUp (Left Nothing)) _ = (diffTimeToText 0, "start")
stopWatchToText (TimerUp (Right startTime)) now = (diffTimeToText $ diffUTCTime now startTime, "stop")
stopWatchToText (TimerUp (Left (Just ndt))) _ = (diffTimeToText ndt, "clear")

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)


countDownToText:: TimerDownState -> UTCTime -> (Text,Text)
countDownToText (Stopped x) now = (diffTimeToText (realToFrac x),"Run")
countDownToText (Running x y) now = (diffTimeToText xx, "Stop") -- aqui va el (target+starttoime) - now
                                 where xx = (diffUTCTime (addUTCTime (realToFrac x) y) now)


countDownWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
countDownWidget deltasDown =  divClass "ensembleTempo ui-font primary-color" $  mdo

  let initialText = "input time"
  let updatedText = fmap (showt) targetTimeEvent
  (value,edits,eval) <- textWidget 1 (constDyn False) initialText updatedText
  -- butt <- button "start counting"
  butt <- dynButton $ dynSnd 
  let evalEvent = tagPromptlyDyn value $ leftmost [butt,eval]
  let targetTimeEvent = fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) evalEvent 
  timeDyn <- holdDyn 60 targetTimeEvent

  let y = tag (current $ currentValue v) $ traceEvent "boton" butt 
  let yy = traceEvent "value" $ attachWith (assambleCountDown) (current timeDyn) (y)
  localChanges <- fmap (traceEvent "localChanges") $ performEvent $ fmap (liftIO . countDownToNextState) yy
  widgetBuildTime <- liftIO $ getCurrentTime  
  initialCount <- sample $ current deltasDown
  let initialTime = fst $ countDownToText initialCount widgetBuildTime
  tick <- tickLossy 1.0 widgetBuildTime 
  let textUpdates = traceEvent "textUpdates" $ attachWith countDownToText (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
  holdDyn initialTime (fmap fst textUpdates) >>= dynText

  dynSnd <- holdDyn "moo" $ fmap snd textUpdates -- transform Event t to Dynamic t (notice the <-, still in the IO monad (is that correct?)) and from (Tx,Tx) -> Tx
  v <- returnVariable deltasDown localChanges
  return v


