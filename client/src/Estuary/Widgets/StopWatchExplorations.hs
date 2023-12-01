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


-- selectVisualiser :: MonadWidget t m => TimeVision -> W t m (Event t TimeVision)-- :: this variable represents the timeVision to be built, EG. Cyclic 2
-- selectVisualiser (Tv 0 seg k) = divClass "tempo-visualiser" $ do
--   cycleTracer seg
--   x <- do 
--     y <- divClass "flex-container-for-timeVision" $ do
--       leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
--           divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
--       let leftEvent = tvNextStateLeft <$ leftPanel-- Event t (TimeVision -> TimeVision)
--       centreEvent <- do
--           z <- elClass "div" "flex-container-for-timeVision-vertical" $ do
--             upPanel <- clickableDiv "flex-item-for-timeVision-vertical-up" $ do
--               divClass "up-panel-hoover" $ text $ T.pack "more subdivisions"
--             infoDisplay upPanel seg 4
--             let upEvent = segmentUp <$ upPanel  
--             downPanel <- clickableDiv "flex-item-for-timeVision-vertical-down" $ do
--               divClass "down-panel-hoover" $ text $ T.pack "less subdivisions"
--             infoDisplay downPanel seg 4
--             let downEvent = segmentDown <$ downPanel
--             let cPanelEvent = leftmost [upEvent,downEvent]
--             return cPanelEvent
--           return z
--       rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
--           divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
--       let rightEvent = tvNextStateRight <$ rightPanel
--       let panelEvent = fmap (\x -> x $ Tv 0 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
--       return panelEvent
--     return y
--   return x

stopWatchWidget :: MonadWidget t m => Dynamic t StopwatchState -> W t m (Variable t StopwatchState)
stopWatchWidget deltasDown =  divClass "stopwatch" $  mdo
  -- 1. Translate button presses into localChanges
  let hoverText = stopWatchToButtonText <$> currentValue v
  -- x <- dynButton $ bText 

  panel <- clickableDiv "stopwatch-panel" $ do -- :: Event t ()
          divClass "stopwatch-panel-hover" $ dynText hoverText
  
  let y = tag (current $ currentValue v) panel -- Event
  localChanges <- performEvent $ fmap (liftIO . stopWatchToNextState) y
  -- 2. Calculate and display text
  widgetBuildTime <- liftIO $ getCurrentTime  -- :: UTC (happens when widget is built)
  initialStopWatch <- sample $ current deltasDown     -- Behaviour t ?? (only happens when widget is built)
  let initialText = stopWatchToText initialStopWatch widgetBuildTime -- calculated once :: Text
  tick <- tickLossy 0.06666666666666667 widgetBuildTime -- :: tickInfo (next line is UTC)
  let textUpdates = attachWith stopWatchToText (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
 -- holdDyn initialText textUpdates >>= dynText -- simple unstyled display of the timer
  texto <- holdDyn initialText textUpdates
  drawStopwatch texto
  v <- returnVariable deltasDown localChanges
  return v

------ stopwatch visualisation widget (work in progress)

-- visualiseStopwatchWidget :: MonadWidget t m => Dynamic t Text -> W t m ()
-- visualiseStopwatchWidget delta = do
--   let class' = constDyn $ "class" =: "human-to-human-comm code-font"
--   let style = constDyn $ "style" =: "height: auto;"
--   let attrs = mconcat [class',style]
--   elDynAttr "stopwatch" attrs $ dynText delta
--   return ()

drawStopwatch :: MonadWidget t m => Dynamic t Text -> W t m ()
drawStopwatch countup = do
  -- svg attrs
  let class' = constDyn $ "class" =: "visualiser code-font"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%"
  let h' = constDyn $ "height" =: "100%"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]
  -- text attrs
  -- let txAttrs = constDyn $ "" =: ""
  -- tspans attrs
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let fill = constDyn $ "fill" =: "var(--primary-color)"
  let x' = constDyn $ "x" =: "50"
  -- tspan1 attrs
  let y' = constDyn $ "y" =: "95"
  -- tspan2 attrs
  let font2 = constDyn $ "font-size" =: "5em"
  let y'' = constDyn $ "y" =: "75"
  let tspan2Attrs = mconcat [txAnchor,fill,x',y'',font2]

  let txAttrs = mconcat [txAnchor,fill,x',y']

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspan2Attrs $ do
        dynText countup
        return ()
      return ()
    return ()    
  return ()


------ State calculations ----

stopWatchToNextState :: StopwatchState -> IO StopwatchState
stopWatchToNextState Cleared = do
  now <- getCurrentTime
  return (Running now)
-- B. If stop watch is counting then it stops:
stopWatchToNextState (Running startTime) = do
  now <- getCurrentTime
  return (Stopped (diffUTCTime now startTime))
-- C. If stop watch is stopped at x:yy then it goes back to 0:
stopWatchToNextState (Stopped _) = return (Cleared)


stopWatchToText :: StopwatchState -> UTCTime -> Text
stopWatchToText Cleared _ = diffTimeToText 0
stopWatchToText (Running startTime) now = diffTimeToText $ diffUTCTime now startTime
stopWatchToText (Stopped ndt) _ = diffTimeToText ndt

stopWatchToButtonText:: StopwatchState -> Text
stopWatchToButtonText Cleared = "Start"
stopWatchToButtonText (Running _) = "Stop"
stopWatchToButtonText (Stopped _) = "Clear"

-- -- general helpers

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> (add0Mod x)

add0Mod:: NominalDiffTime -> Text
add0Mod x = if modulo < 10 then ("0" <> (showt modulo)) else showt modulo 
  where modulo = (floor x) `mod` (60 :: Int)

countToPercent:: Int -> Int -> NominalDiffTime -> Int
countToPercent newSize target grains = if target == 0 then 0 else round $ (grains / (realToFrac target)) * (realToFrac newSize)
