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
stopWatchWidget' deltasDown =  divClass "stopwatch" $  mdo
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
 -- holdDyn initialText textUpdates >>= dynText -- simple unstilled display of the timer
  texto <- holdDyn initialText textUpdates
  visualiseStopwatchWidget $ texto
  v <- returnVariable deltasDown localChanges
  return v

------ stopwatch visualisation widget (work in progress)

visualiseStopwatchWidget :: MonadWidget t m => Dynamic t Text -> W t m ()
visualiseStopwatchWidget delta = do
  let class' = constDyn $ "class" =: "stopwatch"
  let style = constDyn $ "style" =: ("height: auto; font-size:2em; color: white; margin: 1px;")
  let attrs = mconcat [class',style]
  elDynAttr "stopwatch" (attrs) $ dynText delta
  return ()

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


countDownWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
countDownWidget deltasDown =  divClass "countDown" $  mdo

  let initialText = "initial count is 60, change it here"
  let updatedText = fmap (showt) $ updated timeDyn  -- Event t Text
  let editable = editableText <$> currentValue v
  textos <- holdDyn initialText $ leftmost [updatedText, textUpdates]
  (valTxBx,_) <- textWithLockWidget 1 "color: white" editable textos
  let bText = countDownToButtonText <$> currentValue v
  butt <- dynButton $ bText 
  let buttonPressedEvent = tagPromptlyDyn valTxBx $ butt
  let stateWhenButtonPressed = tagPromptlyDyn (currentValue v) buttonPressedEvent
  localChanges <- performEvent $ attachPromptlyDynWith countDownButtonStateChange timeDyn stateWhenButtonPressed
  -- this needs to change to attachWith countDownButtonStateChange (current timeDyn) stateWhenButtonPressed, however I have to discover how to updateText in line 81 and keep an eye on the targetTime update issue, for the moment it is clear that buttonPressedEvent caqnnot be in line 81 without consequences in the proper functioning of the widget...

  timeDyn <- holdDyn 60 $ fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) buttonPressedEvent

  widgetBuildTime <- liftIO $ getCurrentTime  
  initialCount <- sample $ current deltasDown
  let initialTime = countDownToDisplay initialCount widgetBuildTime
  tick <- tickLossy 0.01 widgetBuildTime 
  let textUpdates = attachWithMaybe countDownToDisplay (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 

---- here I have to open a pathway for different kind of visualisations, so far: text, sandclock, bar progress----

--- sandclock experiments
  let sandUpdates = attachWithMaybe sandClock (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
  -- holdDyn initialTime textUpdates >>= dynText -- if state is falling then do this
  coso <- holdDyn "" sandUpdates -- >>= dynText -- if state is falling then do this

--  sandClockWidget coso

  visualiseSVGWidget coso

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
countToPercent target grains = if target == 0 then 0 else round $ (grains / (realToFrac target)) * 500

timeToSand :: Int -> Text
timeToSand grains = showt $ concat $ replicate grains "."

------ ambitious sandclock widget ----

-- a este hay q meterle un dynam ikc q sale con holdDyn "" sandUpdates

sandClockWidget :: MonadWidget t m => Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
sandClockWidget delta = do
  i <- sample $ current delta
  let class' = constDyn $ "class" =: "invertedTriangle code-font"
  let rows' = constDyn $ textWidgetRows 1
  let readon = constDyn $ "readonly" =: ""
  let style = constDyn $ "style" =: ("height: auto; color: yellow; background-color: #003BDE; clip-path: polygon(50% 0, 100% 100%, 0 100%);")
  let attrs = mconcat [class',rows',readon,style]
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits)

-- el :: forall t m a. MonadWidget t m => String -> m a -> m a

-- elClass :: forall t m a. MonadWidget t m => String -> String -> m a -> m a 


visualiseSVGWidget :: MonadWidget t m => Dynamic t Text -> W t m ()
visualiseSVGWidget delta = do
  let class' = constDyn $ "class" =: "stopwatch"
  let style = constDyn $ "style" =: ("height: auto; font-size:2em; color: white; margin: 1px;")
  let attrs = mconcat [class',style]
  elDynAttr "svg" attrs $ el "circle" $ blank   -- $ dynText delta
  return ()

-- <svg width="100" height="100">
--   <circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />
-- </svg>

-- general helpers

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)
