{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Tempo where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read
import Data.Text
import Data.Time
import Data.Map

import Sound.MusicW.AudioContext
import Data.Text (Text)
import qualified Data.Text as T
import TextShow

import Estuary.Types.Tempo
import Estuary.Types.Context
import Estuary.Types.EnsembleResponse
import Estuary.Widgets.Text
import Estuary.Widgets.Reflex
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Widgets.W
import Estuary.Types.Definition

tempoWidget :: MonadWidget t m => Dynamic t Tempo -> W t m (Event t Tempo)
tempoWidget tempoDyn = do
  v <- variableWidget tempoDyn $ \a eventA -> divClass "ensembleTempo ui-font primary-color" $ mdo
    let initialText = showt (freq a)
    let updatedText = fmap (showt . freq) eventA
    (tValue,_,tEval) <- textWidget 1 (constDyn False) initialText updatedText
    b <- dynButton =<< term Term.NewTempo
    let evalEvent = tagPromptlyDyn tValue $ leftmost [b,tEval]
    let cpsEvent = fmapMaybe ((readMaybe :: String -> Maybe Rational) . T.unpack) evalEvent
    edits <- performEvent $ fmap liftIO $ attachPromptlyDynWith (flip changeTempoNow) tempoDyn cpsEvent -- *** attachPromptlyDynWith here might not be right!!!
    return edits
  return $ localEdits v
------

cycleTracer:: MonadWidget t m => Rational ->  W t m ()
cycleTracer segments = do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.01 widgetBuildTime
  beatPosition <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick 
  beat <- holdDyn 0 beatPosition
  visualiseCycles beat segments
  return ()

metreTracer:: MonadWidget t m => Rational -> W t m ()
metreTracer subDivisions = do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.01 widgetBuildTime
  beatPosition <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick 
  beat <- holdDyn 0 beatPosition
  visualiseMetre beat subDivisions -- segments-- W t m TimeVision
  return ()

ringTracer:: MonadWidget t m => Rational -> W t m ()
ringTracer segments = do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.01 widgetBuildTime
  beatPosition <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick 
  beat <- holdDyn 0 beatPosition
  visualiseRing beat segments
  return ()

-- select visualiser at the bottom

getElapsedBeats :: MonadIO m => Tempo -> UTCTime -> m Rational
getElapsedBeats t now = do
  let x = timeToCount t now 
  return x  

visualiseTempoWidget:: MonadWidget t m => Dynamic t TimeVision -> W t m (Variable t TimeVision)
visualiseTempoWidget delta = divClass "tempoVisualiser" $  mdo
  v <- variable delta $ localEdits'
  initialValue <- sample $ current delta
  let initialWidget = selectVisualiser initialValue
  let remoteOrLocalEdits = leftmost [updated delta, localEdits']
  let updatedWidgets = fmap selectVisualiser remoteOrLocalEdits -- type? dynamic or event??
  localEdits <- widgetHold initialWidget updatedWidgets -- m (Dynamic t (Event t T)) -- this does not need localEdits <-
  let localEdits' = switchDyn localEdits -- this line seems unecessary -- switchdyn digs up the event inside the dynamic
  return v

---- separate the view Box from the circle, so this function can be a generic container for the metric and cyclic vis
visualiseCycles :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseCycles delta segments = do
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let style = constDyn $ "style" =: "height: auto;"
  let vB = constDyn $ "viewBox" =: "-1.5 -1.5 3 3"
  let w' = constDyn $ "width" =: "100"
  let h' = constDyn $ "height" =: "100"
  let attrs = mconcat [class',w',h',style,vB]

  let (cx,cy) = (constDyn $ "cx" =: "0", constDyn $ "cy" =: "0")
  let r = constDyn $ "r" =: "1"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05" 
  let attrsCircle = mconcat [cx,cy,r,stroke,strokeWidth]

  let (x1,x2) = (constDyn $ "x1" =: "0",constDyn $ "x2" =: "0")
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "-1")
  let transform = beatToRotation <$> delta 

  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth,transform]

 -- elDynAttr "stopwatch" attrs $ dynText $ fmap (showt) $ fmap (showt) delta
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- create circular dial
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrsCircle $ return () 
    -- manecilla
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
    -- mark
    generatePieSegments segments
  return ()



beatToRotation:: Rational -> Map Text Text
beatToRotation r = "transform" =: ("rotate(" <> (showt radio) <> ")")
  where radio = fromIntegral (round $ radio' * 360) :: Double
        radio' = r - (realToFrac $ floor r)
 
beatToPercentage:: Text -> Rational -> Map Text Text
beatToPercentage atr beat = atr =: (showt percen)
  where percen = fromIntegral (round $ percen' * 100) :: Double
        percen' = beat - (realToFrac $ floor beat)



visualiseMetre :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseMetre delta subDivisions = do
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let style = constDyn $ "style" =: "height: auto;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100"
  let h' = constDyn $ "height" =: "100"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05;" 

  let attrs = mconcat [class',w',h',style,vB]

  let x1 = beatToPercentage "x1" <$> delta
  let x2 = beatToPercentage "x2" <$> delta
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "100")
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- manecilla
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
    -- mark
    generateSegments 100 subDivisions
  return ()


-- simpleList :: MonadWidget t m => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a]) 

generateSegments:: MonadWidget t m => Rational -> Rational ->  m ()
generateSegments width nLines = do
  let segmentsSize = width / nLines 
      lineList = constDyn $ Prelude.take (floor nLines) $ iterate (+ segmentsSize) 0
  x <- simpleList lineList (generateSegment)
  return ()
  
generateSegment:: MonadWidget t m => Dynamic t Rational ->  m ()
generateSegment x = do
  let x1 = generateAttr "x1" <$> x
  let x2 = generateAttr "x2" <$> x
  let y1 = constDyn $ "y1" =: "0"
  let y2 = constDyn $ "y2" =: "100"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05;"
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
  return ()
  
generateAttr :: Text -> Rational -> Map Text Text
generateAttr atr x = atr =: (showt (realToFrac x :: Double))


generatePieSegments:: MonadWidget t m => Rational ->  m ()
generatePieSegments nLines = do
  let segmentsSize = 360 / nLines 
      lineList = constDyn $ Prelude.take (floor nLines) $ iterate (+ segmentsSize) 0
  x <- simpleList lineList (generatePieSegment)
  return ()
  
generatePieSegment:: MonadWidget t m => Dynamic t Rational ->  m ()
generatePieSegment x = do
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05"
  let (x1,x2) = (constDyn $ "x1" =: "0",constDyn $ "x2" =: "0")
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "-1")
  let transform = (\x -> "transform" =: ("rotate(" <> (showt (realToFrac x :: Double)) <> ")")) <$> x
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth,transform]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
  return ()


visualiseRing :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseRing delta segments = do
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let style = constDyn $ "style" =: "height: auto;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100"
  let h' = constDyn $ "height" =: "100"
  let attrs = mconcat [class',w',h',style,vB]

  let radius = 30 :: Float
  let stroke = constDyn $ "stroke" =: "var(--secondary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "8"
  let fill = constDyn $ "fill" =: "transparent"
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "50"
  let transformar = constDyn $ "transform" =: "rotate(180 50 50)"
  let r = constDyn $ "r" =: (showt radius)
  let dashArray = constDyn $ "stroke-dasharray" =: ((showt ((radius * pi * 2)/(realToFrac segments :: Float)*(realToFrac (segments - 1) ::Float))) <> " " <> (showt ((radius * pi * 2)/(realToFrac segments :: Float))))
  let offset = beatToRingSegment radius segments <$> delta

  let currentBeatAttrs = mconcat [class',cx,cy,r,fill,stroke,strokeWidth,dashArray,offset,transformar]

 -- elDynAttr "stopwatch" attrs $ dynText $ fmap (showt) $ fmap (showt) delta
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    ring
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" currentBeatAttrs $ return ()
  --  generateRingSegments segments
  return ()

ring:: MonadWidget t m => m ()
ring = do
  let class' = constDyn $ "class" =: "human-to-human-comm code-font"
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "50"
  let r = constDyn $ "r" =: "30"
  let fill = constDyn $ "fill" =: "transparent"
  let (stroke,strokew) = (constDyn $ "stroke" =: "var(--primary-color)",constDyn $ "stroke-width" =: "14")
  let ringAttrs = mconcat [class',cx,cy,r,fill,stroke,strokew]
  let holeAttrs = mconcat [class',cx,cy,r,fill]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" ringAttrs $ return ()
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" holeAttrs $ return ()
  return ()

generateRingSegments:: MonadWidget t m => Rational ->  m ()
generateRingSegments nSegs = do
  let segmentsSize = 100 / nSegs 
      segList = constDyn $ Prelude.take (floor nSegs) $ iterate (+ segmentsSize) 0
  x <- simpleList segList (generateRingSegment)
  return ()
  
generateRingSegment:: MonadWidget t m => Dynamic t Rational ->  m ()
generateRingSegment x = do
  let stroke = constDyn $ "stroke" =: "var(--background-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "4"
  let fill = constDyn $ "fill" =: "transparent"
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "50"
  let r = constDyn $ "r" =: "30"
  let dashArray = constDyn $ "stroke-dasharray" =: "25 75" 
  let offset = (\x -> "stroke-dashoffset" =: (showt (realToFrac x :: Double))) <$> x
  
  let attrs = mconcat [cx,cy,r,stroke,strokeWidth,fill,dashArray,offset]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrs $ return ()
  return ()

beatToRingSegment:: Float -> Rational -> Rational -> Map Text Text
beatToRingSegment r nSegments beat = whichSegment r nSegments percen
  where percen = fromIntegral (round $ percen' * 100) :: Rational
        percen' = beat - (realToFrac $ floor beat)

whichSegment:: Float -> Rational -> Rational -> Map Text Text
whichSegment r nSegments beatInPercent = 
  let segmentSize = 100 / nSegments
      segList = Prelude.take (floor nSegments) $ iterate (+ segmentSize) 0
      segment' = Prelude.last $ Prelude.filter (<= beatInPercent) segList
      segment = ((realToFrac segment' :: Float) * (r * pi * 2) * 0.01)
  in "stroke-dashoffset" =: (showt $ (realToFrac segment :: Double))

--   <circle class="donut-segment" cx="21" cy="21" r="15.91549430918954" fill="transparent" stroke="black" stroke-width="3" stroke-dasharray="25 75" stroke-dashoffset="0"></circle>
--   <circle class="donut-segment" cx="21" cy="21" r="15.91549430918954" fill="transparent" stroke="black" stroke-width="3" stroke-dasharray="25 75" stroke-dashoffset="25"></circle>
--   <circle class="donut-segment" cx="21" cy="21" r="15.91549430918954" fill="transparent" stroke="black" stroke-width="3" stroke-dasharray="25 75" stroke-dashoffset="50"></circle>
--   <circle class="donut-segment" cx="21" cy="21" r="15.91549430918954" fill="transparent" stroke="black" stroke-width="3" stroke-dasharray="25 75" stroke-dashoffset="75"></circle>


-- <svg width="100%" height="100%" viewBox="0 0 42 42" class="donut">
--   <circle class="donut-hole" cx="21" cy="21" r="15.91549430918954" fill="#fff"></circle>
--   <circle class="donut-ring" cx="21" cy="21" r="15.91549430918954" fill="transparent" stroke="green" stroke-width="4"></circle>
-- </svg>

--- select visualiser

selectVisualiser :: MonadWidget t m => TimeVision -> W t m (Event t TimeVision)
selectVisualiser (Cyclic 0) = divClass "human-to-human-comm code-font" $ do
  cycleTracer 0 
  buttonEvent <- button $ "change" 
  return $ fmap (const (Cyclic 1)) buttonEvent
selectVisualiser (Cyclic 1) = do
  cycleTracer 1 
  buttonEvent <- button $ "change" 
  return $ fmap (const (Cyclic 2)) buttonEvent
selectVisualiser (Cyclic 2) = do
  cycleTracer 2 
  buttonEvent <- button $ "change" 
  return $ fmap (const (Cyclic 3)) buttonEvent
selectVisualiser (Cyclic 3) = do
  cycleTracer 3 
  buttonEvent <- button $ "change" 
  return $ fmap (const (Cyclic 4)) buttonEvent
selectVisualiser (Cyclic 4) = do
  cycleTracer 4 
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 2)) buttonEvent
selectVisualiser (Metric 2) = do
  metreTracer 2
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 3)) buttonEvent
selectVisualiser (Metric 3) = do-- here I will add the second part of the type TimeVision: Cyclic 0
  metreTracer 3
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 4)) buttonEvent
selectVisualiser (Metric 4) = do
  metreTracer 4
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 5)) buttonEvent
selectVisualiser (Metric 5) = do
  metreTracer 5
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 6)) buttonEvent
selectVisualiser (Metric 6) = do
  metreTracer 6
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 7)) buttonEvent
selectVisualiser (Metric 7) = do
  metreTracer 7
  buttonEvent <- button $ "change" 
  return $ fmap (const (Metric 8)) buttonEvent
selectVisualiser (Metric 8) = do
  metreTracer 8
  buttonEvent <- button $ "change" 
  return $ fmap (const (Ring 3)) buttonEvent
selectVisualiser (Ring 3) = do  ------- OJO ----- if ring is 0 produces a crash in the system!!!!!
  ringTracer 3
  buttonEvent <- button $ "change" 
  return $ fmap (const (Ring 4)) buttonEvent
selectVisualiser (Ring 4) = do
  ringTracer 4
  buttonEvent <- button $ "change" 
  return $ fmap (const (Ring 5)) buttonEvent
selectVisualiser (Ring 5) = do
  ringTracer 5
  buttonEvent <- button $ "change" 
  return $ fmap (const (Ring 6)) buttonEvent
selectVisualiser (Ring 6) = do
  ringTracer 6
  buttonEvent <- button $ "change" 
  return $ fmap (const (Ring 7)) buttonEvent
selectVisualiser (Ring 7) = do
  ringTracer 7
  buttonEvent <- button $ "change" 
  return $ fmap (const (Ring 8)) buttonEvent
selectVisualiser (Ring 8) = do
  ringTracer 8
  buttonEvent <- button $ "change" 
  return $ fmap (const (Cyclic 0)) buttonEvent
