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


stopWatchToText :: Clock -> UTCTime -> (Text, Text)
stopWatchToText (TimerUp (Left Nothing)) _ = (diffTimeToText 0, "start")
stopWatchToText (TimerUp (Right startTime)) now = (diffTimeToText $ diffUTCTime now startTime, "stop")
stopWatchToText (TimerUp (Left (Just ndt))) _ = (diffTimeToText ndt, "clear")



-------- Countdown widget and its helpers


countDownWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
countDownWidget deltasDown =  divClass "countDown ui-font primary-color" $  mdo

  let initialText = "inital count is 60, change it here"
  let updatedText = fmap (showt) targetTimeEvent
  (valTxBx,edits,_) <- textWidget 1 (constDyn False) initialText updatedText
  let targetTimeEvent = fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) buttonPressedEvent 
  timeDyn <- holdDyn 60 targetTimeEvent

  butt <- button "el botoncillo"
  -- butt <- dynButton $ dynSnd 
  let buttonPressedEvent = tagPromptlyDyn valTxBx $ butt

  let stateWhenButtonPressed = tag (current $ currentValue v) buttonPressedEvent
  localChanges <- performEvent $ attachWith countDownButtonStateChange (current timeDyn) stateWhenButtonPressed

  widgetBuildTime <- liftIO $ getCurrentTime  
  initialCount <- sample $ current deltasDown
  let initialTime = countDownToDisplay initialCount widgetBuildTime
  tick <- tickLossy 1.0 widgetBuildTime 
  let textUpdates = traceEvent "textUpdates" $ attachPromptlyDynWith countDownToDisplay (currentValue v) $ fmap _tickInfo_lastUTC tick 
  holdDyn initialTime textUpdates >>= dynText

--  dynSnd <- holdDyn "welcome to the extinction widget" $ countDownToButtonText (current $ currentValue v) -- transform Event t to Dynamic t (notice the <-, still in the IO monad (is that correct?)) and from (Tx,Tx) -> Tx
  v <- returnVariable deltasDown localChanges
  return v



countDownButtonStateChange :: MonadIO m => Int -> TimerDownState -> m TimerDownState
countDownButtonStateChange newTar (Stopped tar) = do
  now <- liftIO getCurrentTime
  return (Running newTar now)
countDownButtonStateChange newTar (Running tar y) = do
  return (Stopped newTar)

countDownToDisplay:: TimerDownState -> UTCTime -> Text
countDownToDisplay (Stopped x) now = diffTimeToText (realToFrac x)
countDownToDisplay (Running x y) now = if xx < 0 then diffTimeToText 0 else diffTimeToText xx 
                                 where xx = (diffUTCTime (addUTCTime (realToFrac x) y) now)

countDownToButtonText:: TimerDownState -> Text
countDownToButtonText (Stopped _) = "Run"
countDownToButtonText (Running _ _) = "Stop"


-- general helpers

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)
