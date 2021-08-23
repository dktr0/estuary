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
import Data.Map

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
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let style = constDyn $ "style" =: "height: auto;"
  let attrs = mconcat [class',style]
  elDynAttr "stopwatch" attrs $ dynText delta
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
  (valTxBx,_) <- textWithLockWidget 1 editable textos
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
  -- coso <- holdDyn "" sandUpdates -- >>= dynText -- if state is falling then do this
--  sandClockWidget coso

  let sandUpdates' = attachWithMaybe clockForSVGs (current $ currentValue v) $ fmap _tickInfo_lastUTC tick
  coso' <- holdDyn 0 sandUpdates'

  visualiseSVGWidget coso'

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




clockForSVGs:: TimerDownState -> UTCTime -> Maybe Int 
clockForSVGs (Holding _) _ = Nothing
clockForSVGs (Falling target startTime) now = if xx < 0 then Just $ 0 else Just $ countToPercent 100 target xx
                                 where xx = (diffUTCTime (addUTCTime (realToFrac target) startTime) now)


-- function to calculate in percentage the countdown

sandClock :: TimerDownState -> UTCTime -> Maybe Text 
sandClock (Holding _) _ = Nothing
sandClock (Falling target startTime) now = if xx < 0 then Just $ timeToSand 0 else Just $ timeToSand (countToPercent 100 target xx) 
                                 where xx = (diffUTCTime (addUTCTime (realToFrac target) startTime) now)


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

points :: [(Int,Int)] -> Map Text Text
points [] = Data.Map.empty
points x = "points" =: (coordToText x)

coordToText:: [(Int,Int)] -> Text
coordToText p = Prelude.foldl (\ x y -> x <> " " <> (ptsToCoord y)) "" p

ptsToCoord:: (Int,Int) -> Text
ptsToCoord (x,y) = T.pack (show x) <> (T.pack ",") <> T.pack (show y)

dynY:: (Int,Int) -> Map Text Text
dynY (_,y) = "y" =: (showt y) 

dynHei:: (Int,Int) -> Map Text Text
dynHei (h,_) = "height" =: (showt h)

visualiseSVGWidget :: MonadWidget t m => Dynamic t Int -> W t m ()
visualiseSVGWidget delta = do
  hy <- countToUp 50 0 <$> delta
  let class' = constDyn $ "class" =: "mySVG"
  let width = constDyn $ "width" =: "100"
  let height = constDyn $ "height" =: "100"
  let style = constDyn $ "style" =: ("height: auto; color: white;")
  let attrs = mconcat [class',width,height, style]

  -- sand falling --<rect mask="url(#myMask)" x="0" y="0" width="100" height="50" fill="blue" />
  let x = constDyn $ "x" =: "0"
--  let y = constDyn $ "y" =: "0"
  let y = dynY hy
  let width' = constDyn $ "width" =: "100"
--  let height' = constDyn $ "height" =: "50"
  let height' = dynHei hy
  let strokeUp = constDyn $ "fill" =: "yellow"
  let mask' = constDyn $ "mask" =: "url(#myMask)"
  let attrsUp = mconcat [mask',class',strokeUp,x,y,width',height']
  -- mask="url(#myMask)"

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- creatMask first
    sandClockMask
    -- shape of clock
--    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "polygon" attrsPol $ return () 
    -- sand Up
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsUp $ return () 
    -- sand down
  
  return ()

countToUp:: Rational -> Rational -> Int -> (Int,Int)
countToUp defH defY percent = 
  let actualH = round $ defH * (realToFrac percent) -- int
      actualY = round $ (defY + (defH * (realToFrac percent))) 
  in (actualH, actualY)


sandClockMask:: MonadWidget t m => W t m ()
sandClockMask = do
  let class' = constDyn $ "class" =: "human-to-human-comm textInputToEndOfLine code-font"
  -- rect mask
  let x = constDyn $ "x" =: "0"
  let y = constDyn $ "y" =: "0"
  let width' = constDyn $ "width" =: "100"
  let height' = constDyn $ "height" =: "100"
  let fill' = constDyn $ "fill" =: "black"
  let attrsRect = mconcat [class', x,y,width',height',fill']
  -- clock shape attributes
  let points' = constDyn $ points [(5,95),(95,95),(45,45),(5,5),(95,5)]
  let stroke' = constDyn $ "stroke" =: "white"
  let fill'' = constDyn $ "fill" =: "white"
  let attrsClock = mconcat [class',stroke',points',fill'']
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "mask" (constDyn $ "id" =: "myMask") $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return () 
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "polygon" attrsClock $ return () 
    return ()
  return ()
    

-- <mask id="myMask">  
-- <rect x="0" y="0" width="100" height="100" fill="black" />
-- <polygon points="5,95 95,95 45,45 5,5 95,5" style="fill:white;stroke:white;stroke-width:1" />
-- </mask>


-- general helpers

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> showt (floor x `mod` 60 :: Int)

countToPercent:: Int -> Int -> NominalDiffTime -> Int
countToPercent newSize target grains = if target == 0 then 0 else round $ (grains / (realToFrac target)) * (realToFrac newSize)