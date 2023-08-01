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
import Estuary.Widgets.Text
import Estuary.Widgets.Reflex
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
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

currentBeat:: MonadWidget t m => W t m (Event t Rational)
currentBeat = do
  currentTempo <- tempo
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.06666666666666667 widgetBuildTime
  pure $ attachWith timeToCount (current currentTempo) $ fmap _tickInfo_lastUTC tick


cycleTracer:: MonadWidget t m => Rational ->  W t m ()
cycleTracer segments = do
  currentTempo <- tempo
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  beat' <- holdUniqDyn beat
--  beat' <- traceDynamicWith (\x -> "beat of cyclicTracer: " ++ show (realToFrac x :: Double)) beat
  -- timer beat' currentTempo
  visualiseCycles beat' segments
  return ()

ringTracer:: MonadWidget t m => Rational -> W t m ()
ringTracer segments = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseRing beat segments
  return ()

metreTracerFancy:: MonadWidget t m => Rational -> W t m ()
metreTracerFancy subDivisions = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseMetreFancy beat subDivisions -- segments-- W t m TimeVision
  return ()

metreTracerCheap:: MonadWidget t m => Rational -> W t m ()
metreTracerCheap subDivisions = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseMetreCheap beat subDivisions -- segments-- W t m TimeVision
  return ()

beadsTracerFancy:: MonadWidget t m => Rational -> Rational -> W t m ()
beadsTracerFancy k bead = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseBeads beat k bead True
  return ()

beadsTracerCheap:: MonadWidget t m => Rational -> Rational -> W t m ()
beadsTracerCheap k bead = do
  beatPosition <- currentBeat
  beat <- holdDyn 0 beatPosition
  visualiseBeads beat k bead False
  return ()

-- select visualiser at the bottom

visualiseTempoWidget:: MonadWidget t m => Dynamic t TimeVision -> W t m (Variable t TimeVision)
visualiseTempoWidget delta = mdo
  v <- variable delta $ localEdits'
  initialValue <- sample $ current delta
  let initialWidget = selectVisualiser initialValue
  let remoteOrLocalEdits = leftmost [updated delta, localEdits']
  let updatedWidgets = fmap selectVisualiser remoteOrLocalEdits -- type? dynamic or event??
  localEdits <- widgetHold initialWidget updatedWidgets
 -- localEdits <- widgetHold initialWidget $ traceEventWith show updatedWidgets -- m (Dynamic t (Event t TimeVi)) 
  let localEdits' = switchDyn localEdits -- Event t TimeVision
  return v

-- beat helpers

truncToDec:: Rational -> Rational
truncToDec r = r - (realToFrac $ floor r)

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
  delta' <- holdUniqDyn delta
--  liftIO $ putStrLn "build the cycle visualiser"
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

--  delta' <- traceDynamicWith (\x -> show $ (realToFrac x :: Double)) delta

  let transform = beatToRotation <$> delta'

  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth,transform]

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
 -- liftIO $ putStrLn "generate segments for cycle visualiser"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.05"
  let (x1,x2) = (constDyn $ "x1" =: "0",constDyn $ "x2" =: "0")
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "-1.4")
  let transform = (\x -> "transform" =: ("rotate(" <> (showt (realToFrac x :: Double)) <> ")")) <$> x
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth,transform]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
  return ()

-- metre visuliser widget
visualiseMetreFancy :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseMetreFancy delta subDivisions = do
  delta' <- holdUniqDyn delta
 -- liftIO $ putStrLn "build the metre visualiser"
  let class' = constDyn $ "class" =: "metreVisualiser code-font"
--  let style = constDyn $ "style" =: "position: relative; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%"
  let h' = constDyn $ "height" =: "100%"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "0.5"
  let par = constDyn $ "preserveAspectRatio" =: "none"

  let attrs = mconcat [class',w',h',vB, par]

--  delta' <- traceDynamicWith (\x -> "beat of metre: " ++ show (realToFrac x :: Double)) delta

  let x1 = beatToPercentage "x1" <$> delta'
  let x2 = beatToPercentage "x2" <$> delta'
--  let lineStyle = constDyn $ "style" =: "z-index:-9;"
  let (y1,y2) = (constDyn $ "y1" =: "0",constDyn $ "y2" =: "100")
  let attrsLine = mconcat [x1,y1,x2,y2,stroke,strokeWidth]


  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    generateSegments 100 subDivisions
    -- manecilla
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" attrsLine $ return ()
  return ()


visualiseMetreCheap :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseMetreCheap delta subDivisions = do
  delta' <- holdUniqDyn delta
 -- liftIO $ putStrLn "build the metre visualiser"
  let class' = constDyn $ "class" =: "metreVisualiser code-font"
--  let style = constDyn $ "style" =: "position: relative; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%"
  let h' = constDyn $ "height" =: "100%"
  let par = constDyn $ "preserveAspectRatio" =: "none"

  let attrs = mconcat [class',w',h',vB, par]

--  delta' <- traceDynamicWith (\x -> "beat of metre: " ++ show (realToFrac x :: Double)) delta

--  this is for the segment
  let barPos = barPosition subDivisions <$> beatToPercentage' <$> delta'
  let y = constDyn $ "y" =: "0"
  let width = constDyn $ "width" =: (showt (realToFrac (safeDiv 100 subDivisions) :: Double))
  let height = constDyn $ "height" =: "100"
  let f = constDyn $ "fill" =: "var(--primary-color)"
  let attrsRect = mconcat [barPos,y,width,height,f]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- markTheZero delta
    -- mark
    generateSegments 100 subDivisions
    -- segment
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return ()
  return ()

markTheZero:: MonadWidget t m => Dynamic t Rational -> m ()
markTheZero beat = do
  beat' <- holdUniqDyn beat
  let x = constDyn $ "cx" =: "50"
  let y = constDyn $ "cy" =: "50"
  let r = constDyn $ "r" =: "5"
  let opacity = markOpacity <$> beat
  let dynFill = markMetre <$> beat'
  let attrs = mconcat [x,y,r,dynFill,opacity]
      -- rect for beat
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrs $ return ()
  return ()

markOpacity:: Rational -> Map Text Text
markOpacity x = "style" =: ("opacity:" <> showt fadeOut)
    where fadeOut = realToFrac (0.45 - (0.45 * truncToDec x)) :: Double

markMetre:: Rational -> Map Text Text
markMetre x 
    | ((truncToDec x) >= 0) && ((truncToDec x) < 0.45)  = "fill" =: "var(--primary-color)"
    | otherwise = "fill" =: "none"

safeDiv:: Rational -> Rational -> Rational
safeDiv e 0 = 0
safeDiv e n = e/n 

safeDiv':: Float -> Float -> Float
safeDiv' e 0 = 0
safeDiv' e n = e/n

generateSegments:: MonadWidget t m => Rational -> Rational ->  m ()
generateSegments width nLines = do
  let segmentsSize = width / nLines
      lineList = constDyn $ Prelude.take (floor nLines) $ iterate (+ segmentsSize) 0
  x <- simpleList lineList (generateSegment (width/nLines))
  return ()
  
generateSegment:: MonadWidget t m => Rational -> Dynamic t Rational ->  m ()
generateSegment width x = do
  x' <- holdUniqDyn x
--  liftIO $ putStrLn "build a segment of metre"
  let x1 = generateAttr "x" <$> x'
--  let x2 = generateAttr "x2" <$> x
  let w = constDyn $ "width" =: (showt (realToFrac width :: Double))
  let h = constDyn $ "height" =: "100"
  let y1 = constDyn $ "y" =: "0"
--  let y2 = constDyn $ "y2" =: "100"
  let f = constDyn $ "fill" =: "var(--secondary-color)"
  let opacity = setOpacityPercen <$> x'
  let attrsRect = mconcat [x1,y1,w,h,f,opacity]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return ()
  return ()

generateAttr :: Text -> Rational -> Map Text Text
generateAttr atr x = atr =: (showt (realToFrac x :: Double))

setOpacityPercen:: Rational -> Map Text Text
setOpacityPercen segPos = "style" =: ("opacity: " <> showt (realToFrac x :: Double))
          where x = 0.15 + (x'*0.85)
                x' = 1 - (segPos / 100)

-- generalize this algorithm for cyclic and metric visualisers
barPosition:: Rational -> Rational -> Map Text Text
barPosition nSegs' position =
  let nSegs = if nSegs' /= 0 then nSegs' else 1
      segDur = 100 / nSegs
      segList = Prelude.take (floor nSegs) $ iterate (+ segDur) 0
      segment = Prelude.last $ Prelude.filter (<= (position*100)) segList
  in "x" =: (showt (realToFrac segment :: Double)) 

-- ring visualiser widget

visualiseRing :: MonadWidget t m => Dynamic t Rational -> Rational -> m ()
visualiseRing delta segs = do
  delta' <- holdUniqDyn delta
--  liftIO $ putStrLn "build the doughnut visualiser"
--  delta' <- traceDynamicWith (\x -> "beat of ring: " ++ show (realToFrac x :: Double)) delta
 -- delta' <- traceDynamicWith (\x -> "beat of metre: " ++ show (realToFrac x :: Double)) delta
  let class' = constDyn $ "class" =: "ringVisualiser code-font"
--  let style = constDyn $ "style" =: "position: relative; z-index: -10;"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%;"
  let h' = constDyn $ "height" =: "100%;"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]

  let radius = 40 :: Float
  let stroke = constDyn $ dynStroke segs
  let strokeWidth = constDyn $ "stroke-width" =: "10"
  let fill = constDyn $ "fill" =: "none"
  let cx = constDyn $  "cx" =: "50" 
  let cy = constDyn $  "cy" =: "50"
  let currentSegPos = segmentPosition segs <$> beatToPercentage' <$> delta' -- this!!!
  let r = constDyn $ "r" =: (showt radius)
  let dashArray = constDyn $ segmentSize radius (realToFrac segs :: Float) -- because of PI it has to be floats
  let style = constDyn $ "style" =: "opacity: 0.85"

  let currentBeatAttrs = mconcat [cx,cy,r,fill,stroke,strokeWidth,dashArray,currentSegPos,style]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    markTheZero delta'
    generateRingSegments segs radius
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" currentBeatAttrs $ return ()
    
  return ()

dynStroke:: Rational -> Map Text Text
dynStroke x 
    | x == 0  = "stroke" =: "none"
    | otherwise = "stroke" =: "var(--primary-color)"


segmentSize:: Float -> Float -> Map Text Text
segmentSize radio segs = "stroke-dasharray" =: ((showt arg1) <> " " <> (showt arg2))
            where arg1 = realToFrac (safeDiv' (radio * pi * 2) segs) :: Double
                  arg2 = realToFrac ((safeDiv' (radio * pi * 2) segs)*if segs == 0 then 0 else (segs - 1)) :: Double

segmentPosition:: Rational -> Rational -> Map Text Text
segmentPosition segs position 
  | (segs == 0) = "transform" =: "rotate(0)"
  | otherwise =
    let segDur = safeDiv 360 segs
        segList = Prelude.take (floor segs) $ iterate (+ segDur) 0
        segPos = Prelude.last $ Prelude.filter (<= (position*360)) segList
        segPosO = offset segPos 180 -- 180 means the thing starts at the left and goes to the right, 270 would start at top
    in "transform" =: ("rotate(" <> (showt (realToFrac segPosO :: Double)) <> ",50,50)")

offset:: Rational -> Rational -> Rational
offset setPos off = (x - x')*360
  where x = (setPos + off)/360
        x' = realToFrac (floor x) :: Rational

generateRingSegments:: MonadWidget t m => Rational -> Float ->  m ()
generateRingSegments nSegs' radius = do
  let nSegs = if nSegs' == 0 then 1 else nSegs'
      segmentsSize = safeDiv 360 nSegs -- if 0 subdivisions then mark the whole circle with secondary colour
      segList = constDyn $ Prelude.take (floor nSegs) $ iterate (+ segmentsSize) 0
  x <- simpleList segList (\segPos -> generateRingSegment nSegs segPos radius)
  return ()
  
generateRingSegment:: MonadWidget t m => Rational -> Dynamic t Rational -> Float ->  m ()
generateRingSegment segs segPos radius = do
  segPos' <- holdUniqDyn segPos
 -- liftIO $ putStrLn "build segment of doughnut"
  let stroke = constDyn $ "stroke" =: "var(--secondary-color)"
  let strokeWidth = constDyn $ "stroke-width" =: "15"
  let fill = constDyn $ "fill" =: "transparent"
  let cx = constDyn $  "cx" =: "50"
  let cy = constDyn $  "cy" =: "50"
  let r = constDyn $ "r" =: (showt radius)
  let dashArray = constDyn $ segmentSize radius (realToFrac segs :: Float)
  let position = segPosition <$> segPos'
  let opacity = segOpacityDegrees <$> segPos'
  
  let attrs = mconcat [cx,cy,r,stroke,strokeWidth,fill,dashArray,position,opacity]
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" attrs $ return ()
  return ()

segPosition:: Rational -> Map Text Text
segPosition segPos = "transform" =: ("rotate(" <> (showt (realToFrac segPosO :: Double)) <> ",50,50)")
          where segPosO = offset segPos 180

segOpacityDegrees:: Rational -> Map Text Text
segOpacityDegrees segPos = "style" =: ("opacity: " <> showt (realToFrac x :: Double))
          where x = 0.15 + (x'*0.85)
                x' = 1 - (segPos / 360)

  ------ bead visualiser

beadPosition:: Rational -> Rational -> Map Text Text
beadPosition 0 _ = "" =: ""
beadPosition nBeads position =
  let beadDur = safeDiv 360 nBeads
      beadList = Prelude.take (floor nBeads) $ iterate (+ beadDur) 0
      bead = Prelude.last $ Prelude.filter (<= (position*360)) beadList
  in "transform" =: ("rotate(" <> (showt (realToFrac bead :: Double)) <> ",50,50)")

beadSize:: Bool -> Rational -> Rational -> Map Text Text
beadSize _ 0 _ = "" =: ""
beadSize True nBeads position = 
  let beadDur = safeDiv 1 nBeads
      beadList = Prelude.take (floor nBeads) $ iterate (+ beadDur) 0
      bead' = beadDur + (negate $ position - (Prelude.last $ Prelude.filter (<= (position)) beadList)) -- cuenta regresiva de fraccion q representa segmento del beat a 0
      bead = safeDiv bead' beadDur
      scaleF = (beadScaling nBeads) + 0.1
  in "r" =: showt (realToFrac (bead*scaleF) :: Double)
beadSize False nBeads pos = "r" =: showt (realToFrac (beadScaling nBeads) :: Double)

visualiseBeads :: MonadWidget t m => Dynamic t Rational -> Rational -> Rational -> Bool -> m ()
visualiseBeads delta k beads dyn = do
  delta' <- holdUniqDyn delta
 -- liftIO $ putStrLn "build beads visualiser"
--  delta' <- traceDynamicWith (\x -> "beat of beads: " ++ show (realToFrac x :: Double)) delta
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
  let currentBeatBead = beadPosition beads <$> beatToPercentage' <$> delta' -- Map Text Text --- transform: rotate(x,50,50)
  let beadDynSize = beadSize dyn beads <$> beatToPercentage' <$> delta'

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
generateBeads nBeads = do
 -- liftIO $ putStrLn "build segment of beads"
  let beadSize = beadScaling nBeads
      beadDistribution = safeDiv 360 nBeads
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
  beads' <- holdUniqDyn beads
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
  let nBeads = n
      beadSize = beadScaling nBeads
      beadInterval = safeDiv 360 nBeads 
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

infoDisplay:: MonadWidget t m => Event t () -> Rational -> Integer -> m ()
infoDisplay x seg tics = do 
  let segDisplay = floor seg :: Int
--  liftIO $ putStrLn $ show segDisplay
  eventTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.2 eventTime -- Event t (TickInfo)
  let w = intsToBool tics <$> fmap _tickInfo_n tick  -- Event t (Bool)
  z' <- holdDyn False w
  z <- holdUniqDyn z' -- Dyn t Bool
  hideableWidget z "segmentMark" $ text (showt segDisplay)
  pure ()

-- this one displays two numbers: k and n
infoDisplay':: MonadWidget t m => Event t () -> Rational ->  Rational -> Integer -> m ()
infoDisplay' x k seg tics = do 
  let segDisplay = floor seg :: Int
  let kDisplay = floor k :: Int
--  liftIO $ putStrLn $ show segDisplay
  eventTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.2 eventTime -- Event t (TickInfo)
  let w = intsToBool tics <$> fmap _tickInfo_n tick  -- Event t (Bool)
  z' <- holdDyn False w
  z <- holdUniqDyn z' -- Dyn t Bool
  hideableWidget z "segmentMark" $ text ("("<>(showt kDisplay)<>","<>(showt segDisplay)<>")")
  pure ()

intsToBool:: Integer -> Integer -> Bool
intsToBool n' n
  | n < n' = True
  | otherwise = False

selectVisualiser :: MonadWidget t m => TimeVision -> W t m (Event t TimeVision)-- :: this variable represents the timeVision to be built, EG. Cyclic 2
selectVisualiser (Tv 0 seg k) = divClass "tempo-visualiser" $ do
  cycleTracer seg
  x <- do 
    y <- divClass "flex-container-for-timeVision" $ do
      leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
          divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
      let leftEvent = tvNextStateLeft <$ leftPanel-- Event t (TimeVision -> TimeVision)
      centreEvent <- do
          z <- elClass "div" "flex-container-for-timeVision-vertical" $ do
            upPanel <- clickableDiv "flex-item-for-timeVision-vertical-up" $ do
              divClass "up-panel-hoover" $ text $ T.pack "more subdivisions"
            infoDisplay upPanel seg 4
            let upEvent = segmentUp <$ upPanel  
            downPanel <- clickableDiv "flex-item-for-timeVision-vertical-down" $ do
              divClass "down-panel-hoover" $ text $ T.pack "less subdivisions"
            infoDisplay downPanel seg 4
            let downEvent = segmentDown <$ downPanel
            let cPanelEvent = leftmost [upEvent,downEvent]
            return cPanelEvent
          return z
      rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
          divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
      let rightEvent = tvNextStateRight <$ rightPanel
      let panelEvent = fmap (\x -> x $ Tv 0 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
      return panelEvent
    return y
  return x

selectVisualiser (Tv 1 seg k) = divClass "tempo-visualiser" $ do
  ringTracer seg
  x <- divClass "flex-container-for-timeVision" $ do
    leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
        divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
    let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
    centreEvent <- do 
      x <- elClass "div" "flex-container-for-timeVision-vertical" $ do
        upPanel <- clickableDiv "flex-item-for-timeVision-vertical-up" $ do
          divClass "up-panel-hoover" $ text $ T.pack "more subdivisions"
        infoDisplay upPanel seg 4
        let upEvent = segmentUp <$ upPanel  
        downPanel <- clickableDiv "flex-item-for-timeVision-vertical-down" $ do
          divClass "down-panel-hoover" $ text $ T.pack "less subdivisions"
        infoDisplay downPanel seg 4
        let downEvent = segmentDown <$ downPanel
        let cPanelEvent = leftmost [upEvent,downEvent]
        return cPanelEvent
      return x
    rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
        divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
    let rightEvent = tvNextStateRight <$ rightPanel
    let panelEvent = fmap (\x -> x $ Tv 1 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
    return panelEvent
  return x

selectVisualiser (Tv 2 seg k) = divClass "tempo-visualiser" $ do
  metreTracerFancy seg
  x <- do
    x <- divClass "flex-container-for-timeVision" $ do
      leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
          divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
      let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
      centreEvent <- do
        x <- elClass "div" "flex-container-for-timeVision-vertical" $ do
          upPanel <- clickableDiv "flex-item-for-timeVision-vertical-up" $ do
            divClass "up-panel-hoover" $ text $ T.pack "more subdivisions"
          infoDisplay upPanel seg 4
          let upEvent = segmentUp <$ upPanel  
          downPanel <- clickableDiv "flex-item-for-timeVision-vertical-down" $ do
            divClass "down-panel-hoover" $ text $ T.pack "less subdivisions"
          infoDisplay downPanel seg 4
          let downEvent = segmentDown <$ downPanel
          let cPanelEvent = leftmost [upEvent,downEvent]
          return cPanelEvent
        return x
      rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
          divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
      let rightEvent = tvNextStateRight <$ rightPanel
      let panelEvent = fmap (\x -> x $ Tv 2 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
      return panelEvent
    return x
  return x

selectVisualiser (Tv 3 seg k) = divClass "tempo-visualiser" $ do
  metreTracerCheap seg
  x <- do
    x <- divClass "flex-container-for-timeVision" $ do
      leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
          divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
      let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
      centreEvent <- do 
        x <- elClass "div" "flex-container-for-timeVision-vertical" $ do
          upPanel <- clickableDiv "flex-item-for-timeVision-vertical-up" $ do
            divClass "up-panel-hoover" $ text $ T.pack "more subdivisions"
          infoDisplay upPanel seg 4
          let upEvent = segmentUp <$ upPanel  
          downPanel <- clickableDiv "flex-item-for-timeVision-vertical-down" $ do
            divClass "down-panel-hoover" $ text $ T.pack "less subdivisions"
          infoDisplay downPanel seg 4
          let downEvent = segmentDown <$ downPanel
          let cPanelEvent = leftmost [upEvent,downEvent]
          return cPanelEvent
        return x
      rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
          divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
      let rightEvent = tvNextStateRight <$ rightPanel
      let panelEvent = fmap (\x -> x $ Tv 3 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
      return panelEvent
    return x
  return x

selectVisualiser (Tv 4 seg k) = divClass "tempo-visualiser" $ do
  beadsTracerFancy k seg
  x <- divClass "flex-container-for-timeVision" $ do
    leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
        divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
    let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
    centreEvent <- do  
      x <- elClass "div" "flex-container-for-timeVision-vertical-2" $ do
        upPanel <- clickableDiv "flex-item-for-timeVision-vertical-2-up" $ do
          divClass "up-panel-hoover-2" $ text $ T.pack "more subdivisions"
        infoDisplay' upPanel k seg 4
        let upEvent = segmentUp <$ upPanel  
        middlePanel <- clickableDiv "flex-item-for-timeVision-vertical-2-middle" $ do
          divClass "middle-panel-hoover" $ text $ T.pack "bjorklund pattern"
        infoDisplay' middlePanel k seg 4
        let middleEvent = bjorklundUp <$ middlePanel
        downPanel <- clickableDiv "flex-item-for-timeVision-vertical-2-down" $ do
          divClass "down-panel-hoover-2" $ text $ T.pack "less subdivisions"
        infoDisplay' downPanel k seg 4
        let downEvent = segmentDown <$ downPanel
        let cPanelEvent = leftmost [middleEvent,upEvent,downEvent]
        return cPanelEvent
      return x
    rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
        divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
    let rightEvent = tvNextStateRight <$ rightPanel
    let panelEvent = fmap (\x -> x $ Tv 4 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
    return panelEvent
  return x

selectVisualiser (Tv 5 seg k) = divClass "tempo-visualiser" $ do
  beadsTracerCheap k seg
  x <- divClass "flex-container-for-timeVision" $ do
    leftPanel <- clickableDiv "flex-item-for-timeVision-left" $ do -- :: Event t ()
        divClass "left-panel-hoover" $ text $ T.pack "previous metre visualiser"
    let leftEvent = tvNextStateLeft <$ leftPanel -- Event t (TimeVision -> TimeVision)
    centreEvent <- do
      x <- elClass "div" "flex-container-for-timeVision-vertical-2" $ do
        upPanel <- clickableDiv "flex-item-for-timeVision-vertical-2-up" $ do
          divClass "up-panel-hoover-2" $ text $ T.pack "more subdivisions"
        infoDisplay' upPanel k seg 4
        let upEvent = segmentUp <$ upPanel  
        middlePanel <- clickableDiv "flex-item-for-timeVision-vertical-2-middle" $ do
          divClass "middle-panel-hoover" $ text $ T.pack "bjorklund pattern"
        infoDisplay' middlePanel k seg 4
        let middleEvent = bjorklundUp <$ middlePanel
        downPanel <- clickableDiv "flex-item-for-timeVision-vertical-2-down" $ do
          divClass "down-panel-hoover-2" $ text $ T.pack "less subdivisions"
        infoDisplay' downPanel k seg 4
        let downEvent = segmentDown <$ downPanel
        let cPanelEvent = leftmost [middleEvent,upEvent,downEvent]
        return cPanelEvent
      return x
    rightPanel <- clickableDiv "flex-item-for-timeVision-right" $ do -- :: Event t ()
        divClass "right-panel-hoover" $ text $ T.pack "next metre visualiser"
    let rightEvent = tvNextStateRight <$ rightPanel
    let panelEvent = fmap (\x -> x $ Tv 5 seg k) $ leftmost [centreEvent,leftEvent,rightEvent]
    return panelEvent
  return x

beatLim = 33

segmentUp:: TimeVision -> TimeVision
segmentUp (Tv 0 x k) = (Tv 0 (realToFrac ((floor (x+1))`mod`beatLim) :: Rational) k)
segmentUp (Tv 1 x k) = (Tv 1 (realToFrac ((floor (x+1))`mod`beatLim) :: Rational) k)
segmentUp (Tv 2 x k) = (Tv 2 (realToFrac ((floor (x+1))`mod`beatLim) :: Rational) k)
segmentUp (Tv 3 x k) = (Tv 3 (realToFrac ((floor (x+1))`mod`beatLim) :: Rational) k)
segmentUp (Tv 4 x k) = (Tv 4 (realToFrac ((floor (x+1))`mod`beatLim) :: Rational) (realToFrac ((floor k)`mod`((floor x)+1)) :: Rational))
segmentUp (Tv 5 x k) = (Tv 5 (realToFrac ((floor (x+1))`mod`beatLim) :: Rational) (realToFrac ((floor k)`mod`((floor x)+1)) :: Rational))

segmentDown:: TimeVision -> TimeVision
segmentDown (Tv 0 x k) = (Tv 0 (realToFrac ((floor (x-1))`mod`beatLim) :: Rational) k)
segmentDown (Tv 1 x k) = (Tv 1 (realToFrac ((floor (x-1))`mod`beatLim) :: Rational) k)
segmentDown (Tv 2 x k) = (Tv 2 (realToFrac ((floor (x-1))`mod`beatLim) :: Rational) k)
segmentDown (Tv 3 x k) = (Tv 3 (realToFrac ((floor (x-1))`mod`beatLim) :: Rational) k)
segmentDown (Tv 4 x k) = (Tv 4 (realToFrac ((floor (x-1))`mod`beatLim) :: Rational) (realToFrac ((floor k)`mod`((floor x)+1)) :: Rational))
segmentDown (Tv 5 x k) = (Tv 5 (realToFrac ((floor (x-1))`mod`beatLim) :: Rational) (realToFrac ((floor k)`mod`((floor x)+1)) :: Rational))

bjorklundUp:: TimeVision -> TimeVision
bjorklundUp (Tv 4 x k) = Tv 4 x (realToFrac ((floor (k+1))`mod`((floor x)+1)) :: Rational)
bjorklundUp (Tv 5 x k) = Tv 5 x (realToFrac ((floor (k+1))`mod`((floor x)+1)) :: Rational)

tvNextStateRight:: TimeVision -> TimeVision
tvNextStateRight (Tv n x k) = Tv ((n+1)`mod`6) x k

tvNextStateLeft:: TimeVision -> TimeVision
tvNextStateLeft (Tv n x k) = Tv ((n-1)`mod`6) x k


------------------------------------------


-- upLeft <- stop
-- upCentre <- play
-- upright <- pause

-- downLeft <- flip visualiser
-- downCentre <- flip to controlers
-- downRight <- flip visualiser
