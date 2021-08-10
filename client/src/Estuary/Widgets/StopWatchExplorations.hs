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



stopWatchWidget' :: MonadWidget t m => Dynamic t TimerUpState -> W t m (Variable t TimerUpState)
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

stopWatchToNextState :: TimerUpState -> IO TimerUpState
stopWatchToNextState Cleared = do
  now <- getCurrentTime
  return (Running now)
-- B. If stop watch is counting then it stops:
stopWatchToNextState (Running startTime) = do
  now <- getCurrentTime
  return (Stopped (diffUTCTime now startTime))
-- C. If stop watch is stopped at x:yy then it goes back to 0:
stopWatchToNextState (Stopped _) = return (Cleared)


stopWatchToText :: TimerUpState -> UTCTime -> (Text, Text)
stopWatchToText Cleared _ = (diffTimeToText 0, "start")
stopWatchToText (Running startTime) now = (diffTimeToText $ diffUTCTime now startTime, "stop")
stopWatchToText (Stopped ndt) _ = (diffTimeToText ndt, "clear")



-------- Countdown widget and its helpers


--- if the state is holding then make the box editable
---- if the state is falling make it uneditable by displaying textUpdates

-- textOrCount:: TimerDownState -> Text -> Text


countDownWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
countDownWidget deltasDown =  divClass "countDown ui-font primary-color" $  mdo

  widgetBuildTime <- liftIO $ getCurrentTime  
  initialCount <- sample $ current deltasDown
  let initialTime = countDownToDisplay initialCount widgetBuildTime
  tick <- tickLossy 0.01 widgetBuildTime 
  let textUpdates = traceEvent "textUpdates" $ attachWith countDownToDisplay (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
  -- holdDyn initialTime textUpdates >>= dynText -- if state is faslling then do this
  let bText = countDownToButtonText <$> currentValue v
  
  let initialText = "initial count is 60, change it here"
  let updatedText = fmap (showt) textUpdates  -- if the state is holding then it might be target time event
  (valTxBx,edits,_) <- textWidget 1 (constDyn False) initialText updatedText -- instead of updated text a function that recieves state and spits this or that...
  let targetTimeEvent = fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) buttonPressedEvent 
  timeDyn <- holdDyn 60 targetTimeEvent

  -- butt <- button "el botoncillo"
  butt <- dynButton $ bText 
  let buttonPressedEvent = tagPromptlyDyn valTxBx $ butt

  let stateWhenButtonPressed = tag (current $ currentValue v) buttonPressedEvent
  localChanges <- performEvent $ attachWith countDownButtonStateChange (current timeDyn) stateWhenButtonPressed
 
  v <- returnVariable deltasDown localChanges
  return v

-----  u have to fix the dynamic text for the button!!!!!

countDownButtonStateChange :: MonadIO m => Int -> TimerDownState -> m TimerDownState
countDownButtonStateChange newTar (Holding tar) = do
  now <- liftIO getCurrentTime
  return (Falling newTar now)
countDownButtonStateChange newTar (Falling tar y) = do
  return (Holding newTar)

countDownToDisplay:: TimerDownState -> UTCTime -> Text
countDownToDisplay (Holding x) now = diffTimeToText (realToFrac x)
countDownToDisplay (Falling x y) now = if xx < 0 then diffTimeToText 0 else diffTimeToText xx 
                                 where xx = (diffUTCTime (addUTCTime (realToFrac x) y) now)

countDownToButtonText:: TimerDownState -> Text
countDownToButtonText (Holding _) = "Start"
countDownToButtonText (Falling _ _) = "Stop"


-- general helpers

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)
