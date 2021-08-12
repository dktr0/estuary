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
  -- 1. Translate button presses into localChanges
  let bText = stopWatchToButtonText <$> currentValue v
  x <- dynButton $ bText 
  let y = tag (current $ currentValue v) x 
  localChanges <- performEvent $ fmap (liftIO . stopWatchToNextState) y
  -- 2. Calculate and display text
  widgetBuildTime <- liftIO $ getCurrentTime  -- :: UTC (happens when widget is built)
  initialStopWatch <- sample $ current deltasDown     -- Behaviour t ?? (only happens when widget is built)
  let initialText = stopWatchToText initialStopWatch widgetBuildTime -- calculated once :: Text
  tick <- tickLossy 0.01 widgetBuildTime -- :: tickInfo (next line is UTC)
  let textUpdates = attachWith stopWatchToText (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
  holdDyn initialText textUpdates >>= dynText
  v <- returnVariable deltasDown localChanges
  return v

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


stopWatchToText :: TimerUpState -> UTCTime -> Text
stopWatchToText Cleared _ = diffTimeToText 0
stopWatchToText (Running startTime) now = diffTimeToText $ diffUTCTime now startTime
stopWatchToText (Stopped ndt) _ = diffTimeToText ndt

stopWatchToButtonText:: TimerUpState -> Text
stopWatchToButtonText Cleared = "Start"
stopWatchToButtonText (Running _) = "Stop"
stopWatchToButtonText (Stopped _) = "Clear"


-------- Countdown widget and its helpers


--- if the state is holding then make the box editable
---- if the state is falling make it uneditable by displaying textUpdates



-- textOrCount:: TimerDownState -> Text
-- textOrCount (Holding _) fallingState holdingState = holdingState
-- textOrCount (Falling _ _) fallingState holdingState = fallingState


countDownWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
countDownWidget deltasDown =  divClass "countDown ui-font" $  mdo

  let initialText = "initial count is 60, change it here"
  let updatedText = fmap (showt) targetTimeEvent  -- Event t Text

  let editable = editableText <$> currentValue v

  textos <- holdDyn initialText $ leftmost [updatedText, textUpdates]

  (valTxBx,_) <- textWithLockWidget 1 "color: white" editable textos


  let bText = countDownToButtonText <$> currentValue v
  butt <- dynButton $ bText 
  let buttonPressedEvent = tagPromptlyDyn valTxBx $ butt

  let stateWhenButtonPressed = tagPromptlyDyn (currentValue v) buttonPressedEvent
  localChanges <- performEvent $ attachWith countDownButtonStateChange (current timeDyn) stateWhenButtonPressed

  let targetTimeEvent = fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) buttonPressedEvent 
  timeDyn <- holdDyn 60 targetTimeEvent

  widgetBuildTime <- liftIO $ getCurrentTime  
  initialCount <- sample $ current deltasDown
  let initialTime = countDownToDisplay initialCount widgetBuildTime
  tick <- tickLossy 0.01 widgetBuildTime 
  let textUpdates = traceEvent "textUpdates" $ attachWithMaybe countDownToDisplay (current $ currentValue v) $ fmap _tickInfo_lastUTC tick -- Maybe Text
  let sandUpdates = attachWithMaybe sandClock (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
  -- holdDyn initialTime textUpdates >>= dynText -- if state is falling then do this
  -- holdDyn "" sandUpdates >>= dynText -- if state is falling then do this

  v <- returnVariable deltasDown localChanges
  return v


editableText:: TimerDownState -> Bool
editableText (Holding _) = False
editableText (Falling _ _) = True

countDownButtonStateChange :: MonadIO m => Int -> TimerDownState -> m TimerDownState
countDownButtonStateChange newTar (Holding tar) = do
  now <- liftIO getCurrentTime
  return (Falling newTar now)
countDownButtonStateChange newTar (Falling tar y) = do
  return (Holding newTar)

countDownToDisplay:: TimerDownState -> UTCTime -> Maybe Text
countDownToDisplay (Holding _) _ = Nothing
countDownToDisplay (Falling x y) now = if xx < 0 then Just $ diffTimeToText 0 else Just $ diffTimeToText xx 
                                 where xx = (diffUTCTime (addUTCTime (realToFrac x) y) now)

countDownToButtonText:: TimerDownState -> Text
countDownToButtonText (Holding _) = "Start"
countDownToButtonText (Falling _ _) = "Stop"


-- function to calculate in percentage the countdown

sandClock :: TimerDownState -> UTCTime -> Maybe Text 
sandClock (Holding _) _ = Nothing
sandClock (Falling target startTime) now = if xx < 0 then Just $ timeToSand 0 else Just $ timeToSand (countToPercent target xx) 
                                 where xx = (diffUTCTime (addUTCTime (realToFrac target) startTime) now)

countToPercent:: Int -> NominalDiffTime -> Int
countToPercent target grains = if target == 0 then 0 else round $ (grains / (realToFrac target)) * 200

timeToSand :: Int -> Text
timeToSand grains = showt $ concat $ replicate grains "."




-- general helpers

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)
