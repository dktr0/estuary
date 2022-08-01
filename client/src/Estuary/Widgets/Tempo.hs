{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Tempo where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read
import Data.Text
import Data.Time
import Data.Map
import Data.Maybe

import Sound.MusicW.AudioContext
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import qualified Sound.Tidal.Bjorklund as TBj

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

-------------------

-- timerWidget:: MonadWidget t m => Dynamic t Timer -> W t m (Variable t Timer)
-- timerWidget delta = mdo
--   v <- variable delta $ localEdits'
--   initialValue' <- sample $ current delta
--   initialValue <- TimerDown [20,30,10] utc False Cycles  
--   let initialWidget = selectPanelOfTimer initialValue
--   let remoteOrLocalEdits = leftmost [updated delta, localEdits']
--   let updatedWidgets = fmap selectPanelOfTimer remoteOrLocalEdits -- type? dynamic or event??
--   localEdits <- widgetHold initialWidget updatedWidgets -- m (Dynamic t (Event t T)) -- this does not need localEdits <-
--   let localEdits' = switchDyn localEdits -- this line seems unecessary -- switchdyn digs up the event inside the dynamic
--   return v


-- create test function: utc

data Measure = Cycles | Seconds deriving (Show)

data Visualiser = Interface | SandClock | SimpleBar | Text deriving (Show)

--                    targets      start  Loop?                    
data Timer = TimerUp Visualiser (Maybe [Int]) UTCTime Bool Measure | TimerDown Visualiser [Int] UTCTime Bool Measure deriving (Show)

-- timer views  do not require a definition as they are local behaviours


timer:: MonadWidget t m => Dynamic t Rational -> Dynamic t Tempo -> W t m ()
timer beat tempo = mdo 
  let textos = constDyn "enter multiple countdowns like: 20 30 10"
  (valTxBx,_) <- textWithLockWidget 2 (constDyn False) textos -- Dyn t Text
  boton <- button "test" -- Event ()

  -- terrible loop mechanism, this will change once the definition is concieved
  tru <- button "true"
  fals <- button "false"
  let si = tag (constant True)  tru -- Event t Bool
  let no = tag (constant False) fals -- Event t Bool
  stateOfLoop <- holdDyn False $ leftmost [si,no]

  let beatAtBEvent = tag (current $ beat) boton -- Event t Rational
  lastBEventDyn <- holdDyn 0 beatAtBEvent -- Dynamic t Rational

  let txPressed = tag (current $ valTxBx) boton
  targetDyn <- holdDyn [] $ fmap parseForm txPressed -- Dyn t [Rational]

  let countFromBEvent = (\x y -> x - y) <$> beat <*> lastBEventDyn

  let countFromBEventLooped = loopBool <$> stateOfLoop <*> targetDyn <*> countFromBEvent

  let inSecsBeat = countToTime <$> tempo <*> beat
  let inSecsLastBEventDyn = countToTime <$> tempo <*> lastBEventDyn

  let countdown = multiTimer 0 <$> targetDyn <*> countFromBEventLooped

  let countFromBEventInSecs = diffUTCTime <$> inSecsBeat <*> inSecsLastBEventDyn 

--  dynText $ fmap (\x -> showt (realToFrac x ::Double)) beat -- this shows beats from booting estuary
  dynText $ fmap (\x -> showt $ (realToFrac x :: Double)) countdown
  text "| is the countdown(s) (in beats) |"
  dynText $ fmap (\x -> showt $ (realToFrac x ::Double)) countFromBEventInSecs 
  text "| is the count up from button pressed in seconds |"
  dynText $ fmap (\x -> showt $ (realToFrac x ::Double)) countFromBEvent 
  text "| is the count up from button pressed in beats |"
  return ()


-- countDown':: Rational -> Rational -> Rational
-- countDown' len beatPosition = len - beatPosition  -- 290 - 200

-- endTime:: Rational -> Rational -> Rational
-- endTime len start = start + len 

-- countDown:: MonadWidget t m => Event t () -> Dynamic t (Rational) -> W t m (Event t Rational)
-- countDown trig len = do
--   c <- context
--   let currentTempo = fmap (tempo . ensemble . ensembleC) c -- this is a dynamic!!!! OJO AQUI 
--   widgetBuildTime <- liftIO $ getCurrentTime
--   let st = performEvent $ fmap (liftIO $ getCurrentTime) trig
--   let start = attachWith timeToCount (current currentTempo) $ st -- event t Rational -- event gets the current time (start moment of the counter), then transform it to beats
--   let end = attachWith endTime (current $ len) start -- event t rational
--   tick <- tickLossy 0.01 widgetBuildTime -- event t tickinfo
--   beatPosition <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick
--   x <- countDown' <$> end <*> beatPosition
--   return x

loopBool':: Bool -> [Rational] -> Rational -> Rational
loopBool' True xs b = realToFrac (mod (floor b) $ floor $ sum xs) :: Rational
loopBool' False _ b = b

loopBool:: Bool -> [Rational] -> Rational -> Rational
loopBool True xs b = ((b / (sum xs)) - (realToFrac (floor (b / (sum xs))) :: Rational)) * (sum xs)
loopBool False _ b = b

parseForm:: T.Text -> [Rational] 
parseForm tx = 
    let listOfDurs = T.words $ T.strip tx
        listOfInts = Data.Maybe.mapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) listOfDurs
    in Prelude.map fromIntegral listOfInts

multiTimer:: Rational -> [Rational] -> Rational -> Rational
multiTimer startPoint x  b
  | (x==[]) = 0
  | otherwise = if (Prelude.head ts) > b then (Prelude.head ts) - b else multiTimer (Prelude.head ts) (Prelude.tail x) b
        where ts = Prelude.tail $ Prelude.scanl (+) startPoint x


---
getElapsedBeats :: MonadIO m => Tempo -> UTCTime -> m Rational
getElapsedBeats t now = do
  let x = timeToCount t now 
  return x  

currentBeat:: MonadWidget t m => W t m (Event t Rational)
currentBeat = do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.06666666666666667 widgetBuildTime
  beatPosition <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick
  return beatPosition


----
-- interfaceForTimerTracer:: MonadQidget t m => W t m ()
-- interfaceForTimerTracer = do
--   c <- context 
--   let currentTempo = fmap (tempo . ensemble . ensembleC) c
--   beat' <- currentBeat
--   beat <- holdDyn 0 beat'
--   timer beat currentTempo
--   return ()

----

cycleTracer:: MonadWidget t m => Rational ->  W t m ()
cycleTracer segments = do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
--  timer beat currentTempo
  visualiseCycles beat segments
  return ()

metreTracer:: MonadWidget t m => Rational -> W t m ()
metreTracer subDivisions = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseMetre beat subDivisions -- segments-- W t m TimeVision
  return ()

ringTracer:: MonadWidget t m => Rational -> W t m ()
ringTracer segments = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseRing beat segments
  return ()

beadsTracer:: MonadWidget t m => Rational -> Rational -> W t m ()
beadsTracer k segments = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseBeads beat k segments
  return ()

-- select visualiser at the bottom

visualiseTempoWidget:: MonadWidget t m => Dynamic t TimeVision -> W t m (Variable t TimeVision)
visualiseTempoWidget delta = mdo
  v <- variable delta $ localEdits'
  initialValue <- sample $ current delta
  let initialWidget = selectVisualiser initialValue
  let remoteOrLocalEdits = leftmost [updated delta, localEdits']
  let updatedWidgets = fmap selectVisualiser remoteOrLocalEdits -- type? dynamic or event??
  localEdits <- widgetHold initialWidget updatedWidgets -- m (Dynamic t (Event t T)) -- this does not need localEdits <-
  let localEdits' = switchDyn localEdits -- this line seems unecessary -- switchdyn digs up the event inside the dynamic
  return v

-- beat helpers

beatToRotation:: Rational -> Map Text Text
beatToRotation r = "transform" =: ("rotate(" <> (showt radio) <> ")")
  where radio = fromIntegral (round $ radio' * 360) :: Double
        radio' = r - (realToFrac $ floor r)
 
beatToPercentage:: Text -> Rational -> Map Text Text
beatToPercentage atr beat = atr =: (showt percen)
  where percen = fromIntegral (round $ percen' * 100) :: Double
        percen' = beat - (realToFrac $ floor beat)

beatToPercentage':: Rational -> Rational -- outputs a Rational representing percentage normalize from 0 to 1
beatToPercentage' beat = percen 
  where percen = beat - (realToFrac $ floor beat)




-- cycle visualiser widget

---- separate the view Box from the circle, so this function can be a generic container for the metric and cyclic vis
visualiseCycles :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseCycles delta segments = do
  let class' = constDyn $ "class" =: "cycleVisualiser"
--  let style = constDyn $ "style" =: "position: absolute; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "-1.5 -1.5 3 3"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let w' = constDyn $ "width" =: "100%;"
  let h' = constDyn $ "height" =: "100%;"
  let attrs = mconcat [class',w',h',vB, par]

  let (cx,cy) = (constDyn $ "cx" =: "0", constDyn $ "cy" =: "0")
  let r = constDyn $ "r" =: "1.4"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05" 
  let attrsCircle = mconcat [cx,cy,r,stroke,strokeWidth]

  let (x1,x2) = (constDyn $ "x1" =: "0",constDyn $ "x2" =: "0")
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "-1.4")
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
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "-1.4")
  let transform = (\x -> "transform" =: ("rotate(" <> (showt (realToFrac x :: Double)) <> ")")) <$> x
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth,transform]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
  return ()     

-- metre visuliser widget

visualiseMetre :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseMetre delta subDivisions = do
  let class' = constDyn $ "class" =: "metreVisualiser code-font"
--  let style = constDyn $ "style" =: "position: relative; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%"
  let h' = constDyn $ "height" =: "100%"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.5"
  let par = constDyn $ "preserveAspectRatio" =: "none"

  let attrs = mconcat [class',w',h',vB, par]

  let x1 = beatToPercentage "x1" <$> delta
  let x2 = beatToPercentage "x2" <$> delta
--  let lineStyle = constDyn $ "style" =: "z-index:-9;"
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "100")
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- manecilla
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
    -- mark
    generateSegments 100 subDivisions
  return ()

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
  let strokeWidth = constDyn $ "stroke-width" =: "0.5"
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
  return ()
  
generateAttr :: Text -> Rational -> Map Text Text
generateAttr atr x = atr =: (showt (realToFrac x :: Double))

-- ring visualiser widget

visualiseRing :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseRing delta segs = do
  let segments = if segs < 1 then 1 else segs
  let class' = constDyn $ "class" =: "ringVisualiser code-font"
--  let style = constDyn $ "style" =: "position: relative; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%;"
  let h' = constDyn $ "height" =: "100%;"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]

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

  let currentBeatAttrs = mconcat [cx,cy,r,fill,stroke,strokeWidth,dashArray,offset,transformar]

 -- elDynAttr "stopwatch" attrs $ dynText $ fmap (showt) $ fmap (showt) delta
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    ring
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" currentBeatAttrs $ return ()
  --  generateRingSegments segments
  return ()

ring:: MonadWidget t m => m ()
ring = do
 -- let class' = constDyn $ "class" =: "ringVisualiser code-font"
  let z = constDyn $ "z" =: "-9"
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "50"
  let r = constDyn $ "r" =: "30"
  let fill = constDyn $ "fill" =: "transparent"
  let (stroke,strokew) = (constDyn $ "stroke" =: "var(--primary-color)",constDyn $ "stroke-width" =: "14")
  let ringAttrs = mconcat [cx,cy,r,fill,stroke,strokew,z]
  let holeAttrs = mconcat [cx,cy,r,fill,z]
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
  let z = constDyn $ "z" =: "-9"
  let stroke = constDyn $ "stroke" =: "var(--background-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "4"
  let fill = constDyn $ "fill" =: "transparent"
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "50"
  let r = constDyn $ "r" =: "30"
  let dashArray = constDyn $ "stroke-dasharray" =: "25 75" 
  let offset = (\x -> "stroke-dashoffset" =: (showt (realToFrac x :: Double))) <$> x
  
  let attrs = mconcat [cx,cy,r,stroke,strokeWidth,fill,dashArray,offset,z]
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

  ------ bead visualiser

beadPosition:: Rational -> Rational -> Map Text Text
beadPosition nBeads' position =
  let nBeads = if nBeads' /= 0 then nBeads' else 1
      beadDur = 360 / nBeads
      beadList = Prelude.take (floor nBeads) $ iterate (+ beadDur) 0
      bead = Prelude.last $ Prelude.filter (<= (position*360)) beadList
  in "transform" =: ("rotate(" <> (showt (realToFrac bead :: Double)) <> ",50,50)")

beadSize:: Rational -> Rational -> Map Text Text
beadSize nBeads' position = 
  let nBeads = if nBeads' /= 0 then nBeads' else 1
      beadDur = if nBeads /= 0 then (1 / nBeads) else 1
      beadList = Prelude.take (floor nBeads) $ iterate (+ beadDur) 0
      bead' = beadDur + (negate $ position - (Prelude.last $ Prelude.filter (<= (position)) beadList)) -- cuenta regresiva de fraccion q representa segmento del beat a 0
      bead = bead' / beadDur
      scaleF = (beadScaling nBeads) + 0.1
  in "r" =: showt (realToFrac (bead*scaleF) :: Double)

visualiseBeads :: MonadWidget t m => Dynamic t Rational -> Rational -> Rational -> m ()
visualiseBeads delta k beads = do
  let class' = constDyn $ "class" =: "beadVisualiser code-font"
--  let style = constDyn $ "style" =: "position: relative; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100"
  let h' = constDyn $ "height" =: "100"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]

  -- define circle attrs
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "7.2"
  let fill = constDyn $ "fill" =:"var(--primary-color)"

  -- size of bead changes depending of position in the beat
  let currentBeatBead = beadPosition beads <$> beatToPercentage' <$> delta -- Map Text Text --- transform: rotate(x,50,50)
  let beadDynSize = beadSize beads <$> beatToPercentage' <$> delta

  let beadAttrs = mconcat [cx,cy,beadDynSize,currentBeatBead,fill]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- create bids with transparent fill and minimal stroke
    generateBeads beads
    -- bjorklund beads
    generateBjorklundBeads (k,beads)
    -- change the filled bid of position and size
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" beadAttrs $ return ()
    
  return ()

generateBeads:: MonadWidget t m => Rational ->  m ()
generateBeads nBeads' = do
  let nBeads = if nBeads' /= 0 then nBeads' else 1
      beadSize = beadScaling nBeads
      beadDistribution = if nBeads /= 0 then (360 / nBeads) else 1
      segList = constDyn $ Prelude.take (floor nBeads) $ iterate (+ beadDistribution) 0
  x <- simpleList segList (generateBead beadSize False)
  return ()

beadScaling:: Rational -> Rational
beadScaling x
  | x <= 5 = 7
  | x <= 8 = 6
  | x <= 12 = 5
  | x <= 16 = 4 
  | otherwise = 3

generateBead:: MonadWidget t m => Rational -> Bool -> Dynamic t Rational -> m ()
generateBead beadSize colour beads = do 
  let stroke = constDyn $ beadStroke colour
  let strokeWidth = constDyn $ "stroke-width" =: "0.5"
  let fill = constDyn $ beadFill colour
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "7.2"
  let r = constDyn $ "r" =: showt (realToFrac beadSize :: Double)
  let transform = (\x -> "transform" =: ("rotate(" <> (showt (realToFrac x :: Double)) <> ",50,50)")) <$> beads
  let attrs = mconcat [cx,cy,r,stroke,strokeWidth,fill,transform]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrs $ return ()
  return ()

beadStroke:: Bool -> Map Text Text
beadStroke True = "stroke" =: "var(--secondary-color)"
beadStroke False = "stroke" =: "var(--primary-color)"

beadFill:: Bool -> Map Text Text
beadFill True = "fill" =: "var(--secondary-color)"
beadFill False = "fill" =: "transparent"

generateBjorklundBeads:: MonadWidget t m => (Rational,Rational) ->  m ()
generateBjorklundBeads (k,n) = do
  let nBeads = if n /= 0 then n else 1
      beadSize = beadScaling nBeads
      beadInterval = (360 / nBeads) 
      beadDistribution = Prelude.map (*beadInterval) $ bjorklundR (k,n)
      segList = constDyn $ Prelude.take (floor k) $ Prelude.scanl (+) 0 $ beadDistribution
  x <- simpleList segList (generateBead beadSize True)
  return ()

bjorklundR:: (Rational,Rational) -> [Rational]
bjorklundR (k,n) = 
  let x = (floor k :: Int, floor n :: Int)
      bj = TBj.bjorklund x -- [t,f,f,t,f,f,t,f] --- 0,3,6,8 = 3 - 0 // 6 - 3 // 8 - 6 
      durs' = Prelude.reverse $ Prelude.map (snd) $ Prelude.filter (\x -> (fst x) == True) $ Prelude.zip bj [0..] 
      durs = getScaling $ Prelude.reverse $ ((snd x) : durs') -- [0,3,6,8] -> [3,3,2]
  in Prelude.map (\x -> realToFrac x :: Rational) durs

getScaling:: [Int] -> [Int]
getScaling (d:urs) 
    | (Prelude.length (d:urs)) == 0 = []
    | (Prelude.length (d:urs)) == 1 = []
    | otherwise = (Prelude.head urs) - d : (getScaling urs)

--- select visualiser
selectVisualiser :: MonadWidget t m => TimeVision -> W t m (Event t TimeVision)-- :: this variable represents the timeVision to be built, EG. Cyclic 2
selectVisualiser (Cyclic seg) = divClass "tempo-visualiser" $ do
  cycleTracer seg
  x <- do 
    x <- divClass "flex-container-for-timeVision" $ do
      leftPanel <- clickableDiv "flex-item-for-timeVision" blank -- :: Event t ()
      let leftEvent = tvNextStateLeft <$ leftPanel-- Event t (TimeVision -> TimeVision)
      centreEvent <- do
          x <- elClass "div" "flex-container-for-timeVision-vertical" $ do
            upPanel <- clickableDiv "flex-item-for-timeVision-vertical" blank
            let upEvent = segmentUp <$ upPanel  
            downPanel <- clickableDiv "flex-item-for-timeVision-vertical" blank
            let downEvent = segmentDown <$ downPanel
            let cPanelEvent = leftmost [upEvent,downEvent]
            return cPanelEvent
          return x
      rightPanel <- clickableDiv "flex-item-for-timeVision" blank
      let rightEvent = tvNextStateRight <$ rightPanel
      let panelEvent = fmap (\x -> x $ Cyclic seg) $ leftmost [centreEvent,leftEvent,rightEvent]
      return panelEvent
    return x   
  return x

selectVisualiser (Metric seg) = divClass "tempo-visualiser" $ do
  metreTracer seg
  x <- do
    x <- divClass "flex-container-for-timeVision" $ do
      leftPanel <- clickableDiv "flex-item-for-timeVision" blank  -- :: Event t ()
      let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
      centreEvent <- do 
        x <- elClass "div" "flex-container-for-timeVision-vertical" $ do
          upPanel <- clickableDiv "flex-item-for-timeVision-vertical" blank
          let upEvent = segmentUp <$ upPanel  
          downPanel <- clickableDiv "flex-item-for-timeVision-vertical" blank
          let downEvent = segmentDown <$ downPanel
          let cPanelEvent = leftmost [upEvent,downEvent]
          return cPanelEvent
        return x
      rightPanel <- clickableDiv "flex-item-for-timeVision" blank
      let rightEvent = tvNextStateRight <$ rightPanel
      let panelEvent = fmap (\x -> x $ Metric seg) $ leftmost [centreEvent,leftEvent,rightEvent]
      return panelEvent
    return x
  return x

selectVisualiser (Ring seg) = divClass "tempo-visualiser" $ do
  ringTracer seg
  x <- divClass "flex-container-for-timeVision" $ do
    leftPanel <- clickableDiv "flex-item-for-timeVision" blank  -- :: Event t ()
    let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
    centreEvent <- do 
      x <- elClass "div" "flex-container-for-timeVision-vertical" $ do
        upPanel <- clickableDiv "flex-item-for-timeVision-vertical" blank
        let upEvent = segmentUp <$ upPanel  
        downPanel <- clickableDiv "flex-item-for-timeVision-vertical" blank
        let downEvent = segmentDown <$ downPanel
        let cPanelEvent = leftmost [upEvent,downEvent]
        return cPanelEvent
      return x
    rightPanel <- clickableDiv "flex-item-for-timeVision" $ blank
    let rightEvent = tvNextStateRight <$ rightPanel
    let panelEvent = fmap (\x -> x $ Ring seg) $ leftmost [centreEvent,leftEvent,rightEvent]
    return panelEvent
  return x

selectVisualiser (Beads (k,seg)) = divClass "tempo-visualiser" $ do
  beadsTracer k seg
  x <- divClass "flex-container-for-timeVision" $ do
    leftPanel <- clickableDiv "flex-item-for-timeVision" blank  -- :: Event t ()
    let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
    centreEvent <- do  
      x <- elClass "div" "flex-container-for-timeVision-vertical-2" $ do
        upPanel <- clickableDiv "flex-item-for-timeVision-vertical-2" blank
        let upEvent = segmentUp <$ upPanel  
        middlePanel <- clickableDiv "flex-item-for-timeVision-vertical-2" blank
        let middleEvent = bjorklundUp <$ middlePanel
        downPanel <- clickableDiv "flex-item-for-timeVision-vertical-2" blank
        let downEvent = segmentDown <$ downPanel
        let cPanelEvent = leftmost [middleEvent,upEvent,downEvent]
        return cPanelEvent
      return x
    rightPanel <- clickableDiv "flex-item-for-timeVision" $ blank
    let rightEvent = tvNextStateRight <$ rightPanel
    let panelEvent = fmap (\x -> x $ Beads (k,seg)) $ leftmost [centreEvent,leftEvent,rightEvent]
    return panelEvent
  return x

beatLim = 33

segmentUp:: TimeVision -> TimeVision
segmentUp (Cyclic x) = (Cyclic (realToFrac ((floor (x+1))`mod`beatLim) :: Rational))
segmentUp (Metric x) = (Metric (realToFrac ((floor (x+1))`mod`beatLim) :: Rational))
segmentUp   (Ring x) =   (Ring (realToFrac ((floor (x+1))`mod`beatLim) :: Rational))
segmentUp  (Beads x) =  (Beads ((realToFrac ((floor (fst x))`mod`((floor $ snd x)+1)) :: Rational), (realToFrac ((floor ((snd x)+1))`mod`beatLim) :: Rational)))

segmentDown:: TimeVision -> TimeVision
segmentDown (Cyclic x) = (Cyclic (realToFrac ((floor (x-1))`mod`beatLim) :: Rational))
segmentDown (Metric x) = (Metric (realToFrac ((floor (x-1))`mod`beatLim) :: Rational))
segmentDown   (Ring x) =   (Ring (realToFrac ((floor (x-1))`mod`beatLim) :: Rational))
segmentDown  (Beads x) = (Beads ((realToFrac ((floor (fst x))`mod`((floor $ snd x)+1)) :: Rational), (realToFrac ((floor ((snd x)-1))`mod`beatLim) :: Rational)))

bjorklundUp:: TimeVision -> TimeVision
bjorklundUp  (Beads x) =  (Beads ((realToFrac ((floor ((fst x)+1))`mod`((floor $ snd x)+1)) :: Rational),snd x))
bjorklundUp _ = (Beads (10,15))


tvNextStateRight:: TimeVision -> TimeVision
tvNextStateRight (Cyclic x) = (Metric x)
tvNextStateRight (Metric x) = (Ring x)
tvNextStateRight   (Ring x) = (Beads (0,x))
tvNextStateRight  (Beads x) = (Cyclic (snd x))

tvNextStateLeft:: TimeVision -> TimeVision
tvNextStateLeft (Cyclic x) = (Beads (0,x))
tvNextStateLeft  (Beads x) = (Ring (snd x))
tvNextStateLeft   (Ring x) = (Metric x)
tvNextStateLeft (Metric x) = (Cyclic x)


------------------------------------------
-- data Timer = TimerUp Visualiser (Maybe [Int]) UTCTime Bool Measure | TimerDown Visualiser [Int] UTCTime Bool Measure deriving (Show)

-- how will this work? too much information being shared by all users... not clear if the visualiser is part or not of the definition... in principle no...

--- select either interface or time visualiser
-- selectPanelOfTimer :: MonadWidget t m => Timer -> W t m (Event t Timer)

-- this should be discussed with David...


-- interfaceOfTimer :: MonadWidget t m => Dynamic t Rational -> m ()
-- interfaceOfTimer delta = divClass "interfaceForTimers" $ do
--   leftPanel <- clickableDiv "sideButton" blank
--   -- event with next state for changing to panels
--   divClass "interactiveArea" do
--     textWidget xxx xxx xxx >>= divClass "txAreaDiv" 
--     divClass "mid-centre-InteractiveArea" $ do
--       loopButton
--       secondsOrCycles
--       return ()
--     divClass "timeDisplay" $ do
--       timeDisplay
--       beatDisplay
--     return () -- here an event with the new timer configuration
--   rightPanel <- clickableDiv "side-Button" blank
--   -- event with next state for changing to panels
--   return ()