{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Timer where

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
-- import Estuary.Types.Context
-- import Estuary.Types.EnsembleResponse
import Estuary.Widgets.Text
import Estuary.Widgets.Reflex
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Types.Live

timerWidget:: MonadWidget t m => Dynamic t Timer -> W t m (Variable t Timer)
timerWidget delta = mdo
  initVal <- sample $ current delta
  dynMode <- holdDyn True $ newModeEv 
  let timerEv = switchDyn $ fmap fst x 
  let newModeEv = switchDyn $ fmap snd x
  x <- flippableWidget (timerControl delta) (timerDisplay delta) True newModeEv 
  variable delta timerEv

timerControl:: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
timerControl delta = divClass "time-widget" $ mdo
  -- liftIO $ putStrLn "timerControl"
  dInit <- sample $ current delta
  let local = fst topContainer
  mergedLocalDelta <- holdDyn dInit {- $ traceEvent "xC" -} $ leftmost [local, updated delta]

  topContainer <- divClass "containerControl" $ do
    columnLeft <- divClass "columnControl" $ do
      smallAreaL <- divClass "small-areaControl" $ do
        flipItem <- buttonWithSettableClass "rowControl ui-buttons other-borders code-font" $ T.pack "display"
        let flipEvent = flipFunc <$ flipItem -- Event t Bool
        programItem <- buttonWithSettableClass "rowControl ui-buttons other-borders code-font" $ T.pack "▶" -- Event t ()
        return (flipEvent, programItem) -- small area :: (Event t Bool, Event t ())
      bigAreaL <- divClass "big-areaControl" $ do
        inputWrapper <- divClass "input-wrapperControl" $ do
          let iText = (formToText . forRendering . form) <$> delta 
          (valTxBx,_) <- textWithLockWidget 3 (constDyn $ False) iText  -- Dyn t Text
          let boton = snd smallAreaL -- Event t ()
          let dynFormed = fmap parseForm valTxBx -- dyn t [(tx, rat)] 
          let forma = (current dynFormed) <@ boton -- Event t Live [(tx,rat)]
          return forma
        return $ textInputFunc <$> inputWrapper -- big area :: Event t (Form -> Timer Timer)     
      return (bigAreaL, fst smallAreaL) -- columnLeft
    columnRight <- divClass "columnControl" $ do
      smallAreaR <- divClass "small-areaControl" $ do
        timerChangeDisplay False mergedLocalDelta
        return () -- small area 
      bigAreaR <- divClass "big-areaControl" $ do
        pure (loopIcon $ (loop <$> mergedLocalDelta)) >>= (divClass "rowControl") -- loop
        loopItem <- clickableDiv "additional-divControl additional-div-topControl" blank
        pure (measureIcons $ (measure <$> mergedLocalDelta)) >>= (divClass "rowControl")
        metreItem <- clickableDiv "additional-divControl additional-div-bottomControl" blank
        let loopEvent = loopFunc <$ (loopItem)
        let metreEvent = measureFunc <$ (metreItem)
        return $ leftmost [loopEvent, metreEvent]
      return bigAreaR -- columnR
    let flippy = id <$  (tag (constant ()) $ snd columnLeft)
    let polyptychEvent = attachWith (\d x -> x d) (current mergedLocalDelta) $ leftmost [columnRight, fst columnLeft, flippy] 
    let flipper = fmap (\x -> x False) $ snd columnLeft 
    return (polyptychEvent, flipper) 
  return topContainer 


timerDisplay:: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
timerDisplay delta = divClass "time-widget" $ mdo
  dInit <- sample $ current delta
  let local = fst top
  mergedLocalDelta <- holdDyn dInit $ leftmost [local,updated delta]
  timerChangeDisplay True mergedLocalDelta
  top <- do
    topContainer <- divClass "containerTimer" $ do 
      flipItem <- clickableDiv "segmentLeftTimer" $ do
        -- divClass "flipTimer" $ text $ T.pack "edit"
        elDynAttr "div" (constDyn ("class" =: "flipTimer")) $ text $ T.pack "edit"
      let flipEvent = flipFunc <$ flipItem  
      timerStateEvents <- divClass "segmentTimer" $ do
        reset <- clickableDiv "rowTopTimer" $ do
          divClass "resetTimer" $ text $ T.pack "reset"
        pausePlay <- clickableDiv "rowBottomTimer" $ do
          divClass "pauseTimer" $ text $ T.pack "pause/resume"
        aTimeReset <- performEvent $ fmap (\ _ -> liftIO $ getCurrentTime) reset
        let resetEvent = resetFunc <$> aTimeReset
        aTimePlay <- performEvent $ fmap (\ _ -> liftIO $ getCurrentTime) pausePlay
        let playPauseEvent = playPauseFunc <$> aTimePlay
        return $ leftmost [resetEvent, playPauseEvent]
      modeChangeItem <- clickableDiv "segmentRightTimer" $ do
        divClass "modeTimer" $ text $ T.pack "change visualiser"
      let modeChangeEvent = visualiserFunc <$ modeChangeItem
      let networkedEvents = leftmost [timerStateEvents, modeChangeEvent]
      return (networkedEvents, flipEvent)
    let flippy = id <$  (tag (constant ()) $ snd topContainer)
    let polyptychEvent = attachWith (\d x -> x d) (current mergedLocalDelta) $ leftmost [fst topContainer,flippy] -- mergeWith is the proper one to use, also here you can use attachWith reverse application &!!!
    let flipper = fmap (\x -> x $ True) $ snd topContainer 
    return (polyptychEvent, flipper)
  return top

timerChangeDisplay:: MonadWidget t m => Bool -> Dynamic t Timer -> W t m ()
timerChangeDisplay False timer = do
  defTimer <- sample $ current timer
  let defN = n defTimer
  dynN <- holdDyn 0 $ updated $  n <$> timer
  let nEv = updated $ n <$> timer
  widgetHold (visualDisplayPeek timer defN) $ visualDisplayPeek timer <$> nEv
  pure ()

timerChangeDisplay True timer = do 
  defTimer <- sample $ current timer
  let defN = n defTimer
  dynN <- holdDyn 0 $ updated $  n <$> timer
  let nEv = updated $ n <$> timer
  widgetHold (visualDisplay timer defN) $ visualDisplay timer <$> nEv
  pure ()

visualDisplay:: MonadWidget t m => Dynamic t Timer -> Int -> W t m ()
visualDisplay delta 0 = do  
  visualiseProgressBarLabel delta
  return ()
visualDisplay delta 1 = do  
  visualiseProgressBar delta
  return ()
visualDisplay delta 2 = do 
  visualiseSandClockLabel delta
  return ()
visualDisplay delta 3 = do 
  visualiseSandClock delta
  return ()
visualDisplay delta 4 = do 
  visualiseTextLabel delta
  return ()
visualDisplay delta 5 = do 
  visualiseText delta
  return ()
visualDisplay delta 6 = do 
  visualiseOnlyLabel delta
  return ()
visualDisplay delta 7 = do 
  visualiseCircle delta
  return ()
visualDisplay delta 8 = do 
  visualiseStack delta
  return ()

visualDisplayPeek:: MonadWidget t m => Dynamic t Timer -> Int -> W t m ()
visualDisplayPeek delta _ = do 
  peek delta
  return ()

calculateCountSorC:: Measure -> Bool -> Timer -> UTCTime -> Rational -> Tempo -> Rational
calculateCountSorC Cycles graphOrText timer wBuildT elapsingCount t = 
    calculateCount graphOrText timer wBuildT elapsingBeat t
    where elapsingBeat = timeToCount t $ addUTCTime (realToFrac elapsingCount) wBuildT
calculateCountSorC Seconds graphOrText timer wBuildT elapsingCount t = 
    calculateCount graphOrText timer wBuildT elapsingCount t
---

----
-- NOTE: holdingCalculation functions and multiTimer functions perform more or less the same purpose but multiTimer depends on external cycling functions. So the looping of the time has to be calculated independently while the holdingCalc functions deal with that internally (by using cycleForm). holdingCalc should be generalised to work also as fallingCalc. 

holdingCalculation:: Bool -> Rational -> Timer -> Tempo -> Rational
holdingCalculation True mark timer t
  | (measure timer) == Seconds = holdingCalculationLooped mark $ Prelude.map snd (forRendering $ form timer)
  | otherwise = holdingCalculationLooped markAsBeat $ Prelude.map snd (forRendering $ form timer)
    where markAsBeat = (freq t) * mark
holdingCalculation False mark timer t
  | (measure timer) == Seconds = holdingCalculationUnlooped mark $ Prelude.map snd (forRendering $ form timer)
  | otherwise = holdingCalculationUnlooped markAsBeat $ Prelude.map snd (forRendering $ form timer)
    where markAsBeat = (freq t) * mark

holdingCalculationUnlooped:: Rational -> [Rational] -> Rational
holdingCalculationUnlooped mark (x:xs) = if mark > (sum (x:xs)) then 0 else holdingCalculation' mark scannedForm
  where scannedForm = Prelude.scanl (+) x xs

holdingCalculationLooped:: Rational -> [Rational] -> Rational
holdingCalculationLooped mark (x:xs) = holdingCalculation' markC scannedForm
  where markC = cycleForm mark (x:xs) 
        scannedForm = Prelude.scanl (+) x xs 

holdingCalculation':: Rational -> [Rational] -> Rational
holdingCalculation' markC [] = 0
holdingCalculation' markC (z:zs) = if z > markC then (z-markC) 
    else holdingCalculation' markC zs   

holdingCalculationP:: Bool -> Rational -> Timer -> Tempo -> Rational
holdingCalculationP True mark timer t 
  | (measure timer) == Seconds = holdingCalculationLoopedP (n timer) mark $ Prelude.map snd (forRendering $ form timer)
  | otherwise = holdingCalculationLoopedP (n timer) markAsBeat $ Prelude.map snd (forRendering $ form timer)
      where markAsBeat = (freq t) * mark
holdingCalculationP False mark timer t 
  | (measure timer) == Seconds = holdingCalculationUnloopedP (n timer) mark $ Prelude.map snd (forRendering $ form timer)
  | otherwise = holdingCalculationUnloopedP (n timer) markAsBeat $ Prelude.map snd (forRendering $ form timer)
      where markAsBeat = (freq t) * mark

-- int is the N necessary for general or segmented timer (checkes which timer we are using)
holdingCalculationUnloopedP:: Int -> Rational -> [Rational] -> Rational
holdingCalculationUnloopedP display mark (x:xs) = if mark > (sum (x:xs)) then 0 else holdingCalculationP' display mark scannedForm (x:xs)
  where scannedForm = Prelude.scanl (+) x xs

holdingCalculationLoopedP:: Int -> Rational -> [Rational] -> Rational
holdingCalculationLoopedP display mark (x:xs) = holdingCalculationP' display markC scannedForm (x:xs)
  where markC = cycleForm mark (x:xs) 
        scannedForm = Prelude.scanl (+) x xs 

holdingCalculationP':: Int -> Rational -> [Rational] -> [Rational] -> Rational
holdingCalculationP' display markC [] [] = 0
holdingCalculationP' display markC (z:zs) (x:xs) = 
          if generalOrSegmentedHold display
          then calculatedSegments 
          else 100 * ((generalForm - markC) / generalForm) -- 100 * ((generalForm - b) / generalForm)
    where percen = 100 * (z - markC) / x
          calculatedSegments = if z > markC then percen else holdingCalculationP' display markC zs xs 
          generalForm = sum (x:xs)

generalOrSegmentedHold:: Int -> Bool -- if new visual representations added this number should correspond to the ones that have a general count. For the moment only 8 has a general count
generalOrSegmentedHold 8 = False
generalOrSegmentedHold _ = True

cycleForm:: Rational -> [Rational] -> Rational
cycleForm mark form = dur * (tr (mark/dur)) 
    where dur = if (sum form == 0) then 1 else sum form
          tr n = n - (realToFrac (floor n) ::Rational)

-- first bool is for calculating percentage (true) or count (false)
calculateCount:: Bool -> Timer -> UTCTime -> Rational -> Tempo -> Rational
calculateCount False delta wBuildT elapsingCount t = -- elapsed count is seconds
  let timeMark = (extractTimeMark . mode) delta
  in case timeMark of -- right UTC (moment to start counting) and left Rational (mark where the count is paused)
      (Left mark) -> holdingCalculation (loop delta) mark delta t
      (Right startMark') -> 
        let startMark = if (measure delta) == Seconds 
                        then realToFrac (diffUTCTime startMark' wBuildT) :: Rational 
                        else timeToCount t startMark'  
            countUp = elapsingCount - startMark
            countForm = Prelude.map snd (forRendering $ form delta) -- [Rat]
            loopedCountUp = loopCountUp' (loop delta) countForm countUp
            countDown = multiTimer 0 countForm loopedCountUp
        in countDown
calculateCount True delta wBuildT elapsingCount t = -- elapsed count is seconds
  let timeMark = (extractTimeMark . mode) delta
  in case timeMark of
      (Left mark) -> holdingCalculationP (loop delta) mark delta t
      (Right startMark') -> 
        let startMark = if (measure delta) == Seconds 
                        then realToFrac (diffUTCTime startMark' wBuildT) :: Rational 
                        else timeToCount t startMark'  
            countUp = elapsingCount - (realToFrac startMark :: Rational)
            countForm = Prelude.map snd (forRendering $ form delta) -- [Rat]
            loopedCountUp = loopCountUp (loop delta) countForm countUp
            countDown = if generalOrSegmentedCount (n delta) then multiTimerPercent 0 countForm loopedCountUp else generalTimerPercent 0 countForm loopedCountUp
        in countDown 

-- label stuff below
calculateLabelSorC:: Measure -> Timer -> UTCTime -> Rational -> Tempo -> Text
calculateLabelSorC Cycles timer wBuildT elapsingCount t = 
    calculateLabel timer wBuildT elapsingBeat t
    where elapsingBeat = timeToCount t $ addUTCTime (realToFrac elapsingCount) wBuildT
calculateLabelSorC Seconds timer wBuildT elapsingCount t = 
    calculateLabel timer wBuildT elapsingCount t


holdingLabel:: Bool -> Rational -> Timer -> Tempo -> Text
holdingLabel True mark timer t 
  | (measure timer) == Seconds = holdingLabelLooped mark (Prelude.map snd (forRendering $ form timer)) $ Prelude.map fst (forRendering $ form timer)
  | otherwise = holdingLabelLooped markAsBeat (Prelude.map snd (forRendering $ form timer)) $ Prelude.map fst (forRendering $ form timer)
      where markAsBeat = (freq t) * mark
holdingLabel False mark timer t 
  | (measure timer) == Seconds = holdingLabelUnlooped mark (Prelude.map snd (forRendering $ form timer)) $ Prelude.map fst (forRendering $ form timer)
  | otherwise = holdingLabelUnlooped markAsBeat (Prelude.map snd (forRendering $ form timer)) $ Prelude.map fst (forRendering $ form timer)
    where markAsBeat = (freq t) * mark 

holdingLabelUnlooped:: Rational -> [Rational] -> [Text] -> Text
holdingLabelUnlooped mark (x:xs) txs = if mark > (sum (x:xs)) then "*" else holdingLabel' mark scannedForm txs
  where scannedForm = Prelude.scanl (+) x xs

holdingLabelLooped:: Rational -> [Rational] -> [Text] -> Text
holdingLabelLooped mark (x:xs) txs = holdingLabel' markC scannedForm txs
  where markC = cycleForm mark (x:xs) 
        scannedForm = Prelude.scanl (+) x xs 

holdingLabel':: Rational -> [Rational] -> [Text] -> Text
holdingLabel' markC [] [] = "*"
holdingLabel' markC (z:zs) (tx:txs) = if z > markC then tx 
    else holdingLabel' markC zs txs
    
calculateLabel:: Timer -> UTCTime -> Rational -> Tempo -> Text
calculateLabel delta wBuildT elapsedCount t = 
  let timeMark = (extractTimeMark . mode) delta
  in case timeMark of -- right UTC (moment to start counting) and left Rational (mark where the count is paused)
      (Left mark) -> holdingLabel (loop delta) mark delta t
      (Right startMark') -> 
        let startMark = if (measure delta) == Seconds 
                        then realToFrac (diffUTCTime startMark' wBuildT) :: Rational 
                        else timeToCount t startMark'
            countUp = elapsedCount - (realToFrac startMark :: Rational)
            countForm = Prelude.map snd (forRendering $ form delta) -- [Rat]
            loopedCountUp = loopCountUp' (loop delta) countForm countUp
            label = genLabel 0 (forRendering $ form delta) loopedCountUp
        in label 


peek:: MonadWidget t m => Dynamic t Timer -> W t m ()
peek delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts  -- :: Event t Rational -- tiempo desde origen
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let countPercent = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  let count' = (calculateCountSorC (measure iDelta) False iDelta wBuildT) <$> beat <*> currentTempo
  let count = formatTextDisplay (measure iDelta) <$> count'
  let label = calculateLabelSorC (measure iDelta) iDelta wBuildT <$> beat <*> currentTempo
  divClass "spot" $ do 
    drawCircle countPercent 
    pure ()
  dynText label
  text $ T.pack "  "
  dynText count
  pure ()

visualiseCircle:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseCircle delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts  -- :: Event t Rational -- tiempo desde origen
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  let label = calculateLabelSorC (measure iDelta) iDelta wBuildT <$> beat <*> currentTempo
  drawCircle count
  pure ()


drawCircle:: MonadWidget t m => Dynamic t Rational -> W t m ()
drawCircle delta = do
 
  let class' = constDyn $ "class" =: "visualiser"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%"
  let h' = constDyn $ "height" =: "100%"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]

  -- define circle attrs
  let cx = constDyn $  "cx" =: "50"
  let cy = constDyn $  "cy" =: "50"
  let fill = constDyn $ "fill" =:"var(--primary-color)"
  let size = (\x -> "r" =: showt (realToFrac (x*0.495) :: Double)) <$> delta
  let circleAttrs = mconcat [cx,cy,size,fill]

  let size' = constDyn $ "r" =: "49.5"
  let stroke = constDyn $ "stroke" =: "var(--secondary-color)"
  let fill' = constDyn $ "fill" =: "transparent"

  let markAttrs = mconcat [cx, cy, size', stroke, fill']

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" markAttrs $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" circleAttrs $ return ()
  
  pure ()


visualiseStack:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseStack delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts  -- :: Event t Rational -- tiempo desde origen
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  let program = (forRendering $ form iDelta)
  drawStack count program 
  pure ()

drawStack:: MonadWidget t m => Dynamic t Rational -> [(Text,Rational)] -> W t m ()
drawStack countdown' program = do
  let countdown = (\z -> realToFrac ((z * (-1))+100) :: Double) <$> countdown'

  let class' = constDyn $ "class" =: "visualiser"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 150 100"
  let vB' = constDyn $ "viewBox" =: "0 0 100 100"
  let par = constDyn $ "preserveAspectRatio" =: "none" 
  let par' = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let txLen = constDyn $ "textLength" =: "75%"
  let attrs = mconcat [class',width,height, vB, par]  -- svg

  let x1Line = constDyn $ "x1" =: "0"
  let x2Line = constDyn $ "x2" =: "150"
  let strokeLine = constDyn $ "stroke" =: "var(--primary-color)"
  let y1Line = (\y1 -> "y1" =: showt y1) <$> countdown
  let y2Line = (\y2 -> "y2" =: showt y2) <$> countdown
  let lineAttrs = mconcat [x1Line, x2Line, strokeLine, y1Line, y2Line]

  let attrs' = mconcat [class',width,height, vB', par', txLen]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs' $ do
    generateLabelForBlocks countdown program

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    generateStackBlocks countdown program
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "line" lineAttrs $ pure ()
  
  pure ()

generateStackBlocks:: MonadWidget t m => Dynamic t Double -> [(Text, Rational)] ->  m ()
generateStackBlocks count program = do
  let heightsAndYs = constDyn <$> heightsAndY (percenForm $ Prelude.map snd program) $ Prelude.map fst program
  x <- simpleList heightsAndYs (generateBlock count)
  return ()

generateLabelForBlocks:: MonadWidget t m => Dynamic t Double -> [(Text, Rational)] ->  m ()
generateLabelForBlocks count program = do
  let heightsAndYs = constDyn <$> heightsAndY (percenForm $ Prelude.map snd program) $ Prelude.map fst program
  x <- simpleList heightsAndYs (generateLabelForBlock count)
  return ()

generateLabelForBlock:: MonadWidget t m => Dynamic t Double -> Dynamic t (Double,Double,Text) ->  m ()
generateLabelForBlock count segment = do
  let label = (\x -> (trd' x)) <$> segment -- Dyn t Text
  -- text attributes
  let textX = constDyn $ "x" =: "45"
  let textY = (\y -> "y" =: (showt $ (snd' y) + ((fst' y)*0.85))) <$> segment
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let fillTx = constDyn $ "fill" =: "var(--primary-color)" 

  let txAttrs = mconcat [txAnchor,fillTx,textX,textY]
  let tspanAttrs = mconcat [txAnchor,fillTx,textX,textY]

  -- span attributes

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspanAttrs $ do
        -- dynText $ fmap (\x -> showt (floor x)) countdown
        dynText label
  return ()


--simpleList :: MonadWidget t m => Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])

generateBlock:: MonadWidget t m => Dynamic t Double -> Dynamic t (Double,Double,Text) ->  m ()
generateBlock count segment = do
  let height = (\x -> "height" =: (showt $ fst' x)) <$> segment
  let y = (\x -> "y" =: (showt $ snd' x)) <$> segment
  let width = constDyn $ "width" =: "150"
  let x = constDyn $ "x" =: "0"
  let stroke = constDyn $ "stroke" =: "var(--background-color)" 
  let fill = fill' <$> count <*> (fst' <$> segment) <*> (snd' <$> segment)
  let op = opacity' <$> count <*> (fst' <$> segment) <*> (snd' <$> segment)
  let attrsRect = mconcat [x,y,width,height,fill,op, stroke]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return ()

  return ()

fill':: Double -> Double -> Double -> Map Text Text
fill' count height y = if (count > (y+height))
                         then ("fill" =: "var(--primary-color)")
                         else ("fill" =: "var(--secondary-color)")

opacity':: Double -> Double -> Double -> Map Text Text
opacity' count height y = if (count >= y) && (count < (y+height)) 
                         then ("opacity" =: "0.5")
                         else ("opacity" =: "0.3")

percenForm:: [Rational] -> [Double]
percenForm count
    | count == [] = [0]
    | otherwise = Prelude.map (\x -> (asD x/ dur)*100) count
    where dur = realToFrac (Prelude.sum $ count) :: Double
          asD x = realToFrac x :: Double 

                        -- (h,y)
heightsAndY:: [Double] -> [Text] -> [(Double,Double,Text)]
heightsAndY heights labels = Prelude.zip3 heights ys labels
    where ys = Prelude.init $ Prelude.scanl (+) 0 heights

fst' (x,_,_) = x
snd' (_,x,_) = x
trd' (_,_,x) = x


visualiseProgressBarLabel:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseProgressBarLabel delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts  -- :: Event t Rational -- tiempo desde origen
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  let label = calculateLabelSorC (measure iDelta) iDelta wBuildT <$> beat <*> currentTempo
  drawProgressBarLabel count label
  pure ()

drawProgressBarLabel:: MonadWidget t m => Dynamic t Rational -> Dynamic t Text -> W t m ()
drawProgressBarLabel countdown tag = do
  let class' = constDyn $ "class" =: "visualiser"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 80"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',width,height, vB, par]  -- svg

  let id = constDyn $ "id" =: "solid" 
  let fltAttrs = mconcat [id]

  let floodColor = constDyn $ "flood-color" =: "var(--background-color)"
  let floodOpacity = constDyn $ "flood-opacity" =: "0.35"
  let result = constDyn $ "result" =: "bg" 
  let feFloodAttrs = mconcat [floodColor, floodOpacity, result]

  let txFill = constDyn $ "fill" =: "var(--secondary-color)"
  let txX = constDyn $ "x" =: "50"
  let txY = constDyn $ "y" =: "60"
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let fontSize = constDyn $ "font-size" =: "2em"
  let txFilter = constDyn $ "filter" =: "url(#solid)"
  -- let fontSz = a dynamic text size with...
  let txAttrs = mconcat [txFill, txX, txY, txAnchor, fontSize, txFilter]


  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      dynText tag
      return () 
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "defs" (constDyn Data.Map.empty) $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "filter" fltAttrs $ do
        elDynAttrNS' (Just "http://www.w3.org/2000/svg") "feFlood" feFloodAttrs $ pure ()
        elDynAttrNS' (Just "http://www.w3.org/2000/svg") "feMerge" (constDyn Data.Map.empty) $ do
          elDynAttrNS' (Just "http://www.w3.org/2000/svg") "feMergeNode" (constDyn $ "in" =: "bg") $ pure ()
          elDynAttrNS' (Just "http://www.w3.org/2000/svg") "feMergeNode" (constDyn $ "in" =: "SourceGraphic") $ pure ()
  
  drawProgressBar countdown
    
  return ()

visualiseProgressBar:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseProgressBar delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts  -- :: Event t Rational -- tiempo desde origen
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  drawProgressBar count
  pure ()

drawProgressBar:: MonadWidget t m => Dynamic t Rational -> W t m ()
drawProgressBar countdown = do
  let class' = constDyn $ "class" =: "visualiser"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 50"
  let par = constDyn $ "preserveAspectRatio" =: "none" 
  let attrs = mconcat [class',width,height, vB, par]  -- svg
  -- rect1
  let x' = constDyn $ "x" =: "0"
  let y' = constDyn $ "y" =: "1"
  let width1 = constDyn $ "width" =: "100"
  let height1 = constDyn $ "height" =: "48"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let fill = constDyn $ "fill" =: "transparent"
  let attrsRect = mconcat [x',y',width1, height1, stroke, fill] 
  -- progress rect
  let x'' = constDyn $ "x" =: "100"
  let y'' = constDyn $ "y" =: "1"
  let height2 = constDyn $ "height" =: "48"
  let opacity = constDyn $ "opacity" =: "0.5"
  let transform = constDyn $ "transform" =: "rotate(180,100,24.75)"
  let fill = constDyn $ "fill" =: "var(--primary-color)"
  let dynWidth = (\x -> "width" =: showt (realToFrac x :: Double)) <$> countdown
  let attrsDynRect = mconcat [x'', y'', height2, opacity, transform, fill, dynWidth]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsDynRect $ return ()
  return ()

----
visualiseSandClockLabel:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseSandClockLabel delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  let label = calculateLabelSorC (measure iDelta) iDelta wBuildT <$> beat <*> currentTempo
  drawSandClockLabel count label
  pure ()


drawSandClockLabel :: MonadWidget t m => Dynamic t Rational -> Dynamic t Text -> W t m ()
drawSandClockLabel countdown tag = do
  -- dynamic stuff
  -- let countdown = fromMaybe 0 <$> countdown' 

  let yFall = countToFallY 50 0 <$> countdown
  let heightFall = countToFallH 50 <$> countdown
  let yHold = countToHoldY 0 100 <$> countdown
  let heightHold = countToHoldH 0 <$> countdown

  -- let class' = constDyn $ "class" =: "visualiser"

  let class' = constDyn $ "class" =: "visualiser"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 100"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let attrs = mconcat [class',width,height, vB, par]
  -- sand falling 
  let x' = constDyn $ "x" =: "0"
  let width' = constDyn $ "width" =: "100%"
  let strokeFall = constDyn $ "fill" =: "var(--primary-color)"
  let mask' = constDyn $ "mask" =: "url(#myMask)"
  let attrsFall = mconcat [mask',strokeFall,x',yFall,width',heightFall]

  -- sand holder
  let widthHold = constDyn $ "width" =: "100%"
  let fillHold = constDyn $ "fill" =: "var(--primary-color)"
  let attrsHold = mconcat [mask',fillHold,x',yHold,widthHold,heightHold]

  let xtx = constDyn $ "x" =: "50%"
  let ytx = constDyn $ "y" =: "95"
  let fs = constDyn $ "font-size" =: "2em" -- this should be dynamic
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let txAttrs = mconcat [fillHold,xtx,ytx,fs,txAnchor]

  let transform = constDyn $ "transform" =: "scale(0.67) translate(27)"
  let layerAttrs = mconcat [transform]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      dynText tag
      return ()
    -- creatMask first
    sandClockMask
    -- sand Falling
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "g" layerAttrs $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsFall $ return () 
      -- sand held
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsHold $ return () 
  return ()
-------

visualiseSandClock:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseSandClock delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  drawSandClock count
  pure ()


drawSandClock :: MonadWidget t m => Dynamic t Rational -> W t m ()
drawSandClock countdown = do

  let yFall = countToFallY 50 0 <$> countdown
  let heightFall = countToFallH 50 <$> countdown
  let yHold = countToHoldY 0 100 <$> countdown
  let heightHold = countToHoldH 0 <$> countdown

  -- let class' = constDyn $ "class" =: "visualiser"
  let class' = constDyn $ "class" =: "visualiser"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 100"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  -- let style = constDyn $ "style" =: ("height: auto; color: white; z-index: 0")
  let attrs = mconcat [class',width,height, vB, par]
  -- sand falling 
  let x' = constDyn $ "x" =: "0"
  let width' = constDyn $ "width" =: "100%"
  let strokeFall = constDyn $ "fill" =: "var(--primary-color)"
  let mask' = constDyn $ "mask" =: "url(#myMask)"
  let attrsFall = mconcat [mask',strokeFall,x',yFall,width',heightFall]

  -- sand holder
  let x' = constDyn $ "x" =: "0"
  let widthHold = constDyn $ "width" =: "100%"
  let fillHold = constDyn $ "fill" =: "var(--primary-color)"
  let attrsHold = mconcat [mask',fillHold,x',yHold,widthHold,heightHold]



  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- creatMask first
    sandClockMask
    -- sand Falling
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsFall $ return () 
      -- sand held
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsHold $ return () 
  return ()

---------
visualiseTextLabel:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseTextLabel delta = do
  -- liftIO $ putStrLn "visualiseTextBuilt"
  -- traceDynamic "delta" $ delta
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  wBuildT <- liftIO getCurrentTime
  elapsedCountPos <- elapsedCounts -- :: Event t Rational
  elapsedCountDyn <- holdDyn 0 elapsedCountPos
  let count' = (calculateCountSorC (measure iDelta) False iDelta wBuildT) <$> elapsedCountDyn <*> currentTempo -- Dyn t Rat
  let count = formatTextDisplay (measure iDelta) <$> count'
  let label = (calculateLabelSorC (measure iDelta) iDelta wBuildT) <$> elapsedCountDyn <*> currentTempo
  drawTextLabel count label
  pure ()

drawTextLabel :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> W t m ()
drawTextLabel countdown tag = do
  -- svg attrs
  let class' = constDyn $ "class" =: "visualiser code-font"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%"
  let h' = constDyn $ "height" =: "100%"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]
  -- text attrs
  -- tspans attrs
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let fill = constDyn $ "fill" =: "var(--primary-color)"
  let x' = constDyn $ "x" =: "50"
  -- tspan1 attrs
  let font1 = constDyn $ "font-size" =: "2em" -- this should be dynamic
  let y' = constDyn $ "y" =: "95"
  let tspan1Attrs = mconcat [txAnchor,fill,x',y',font1]
  -- tspan2 attrs
  let font2 = constDyn $ "font-size" =: "2.8em"
  let y'' = constDyn $ "y" =: "45"
  let tspan2Attrs = mconcat [txAnchor,fill,x',y'',font2]
  let txAttrs = mconcat [txAnchor,fill,x',y']
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspan1Attrs $ do
        dynText tag
        return ()
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspan2Attrs $ do
        -- dynText $ fmap (\x -> showt (floor x)) countdown
        dynText countdown
        return ()
      return ()
    return ()    
  return ()

  ----------
visualiseText:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseText delta = do
  -- liftIO $ putStrLn "visualiseTextBuiltNoLabel"
  -- traceDynamic "delta" $ delta
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  wBuildT <- liftIO getCurrentTime
  elapsedCountPos <- elapsedCounts -- :: Event t Rational
  elapsedCountDyn <- holdDyn 0 elapsedCountPos
  let count' = (calculateCountSorC (measure iDelta) False iDelta wBuildT) <$> elapsedCountDyn <*> currentTempo
  let count = formatTextDisplay (measure iDelta) <$> count'
  drawText count
  pure ()

drawText :: MonadWidget t m => Dynamic t Text -> W t m ()
drawText countdown = do
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
        -- dynText $ fmap (\x -> showt (floor x)) countdown
        dynText countdown
        return ()
      return ()
    return ()    
  return ()
---------
---------
visualiseOnlyLabel:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseOnlyLabel delta = do
  -- liftIO $ putStrLn "visualiseTextBuilt"
  -- traceDynamic "delta" $ delta
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  wBuildT <- liftIO getCurrentTime
  elapsedCountPos <- elapsedCounts -- :: Event t Rational
  elapsedCountDyn <- holdDyn 0 elapsedCountPos
  let label = (calculateLabelSorC (measure iDelta) iDelta wBuildT) <$> elapsedCountDyn <*> currentTempo
  
  drawOnlyLabel label
  pure ()

drawOnlyLabel :: MonadWidget t m => Dynamic t Text -> W t m ()
drawOnlyLabel tag = do
  -- scale tag
  let fontScaled = (fontSize' . T.length) <$> tag
  
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
  let font1 = fontScaled
  let y' = constDyn $ "y" =: "75"
  let tspan1Attrs = mconcat [txAnchor,fill,x',y',font1]

  let txAttrs = mconcat [txAnchor,fill,x',y']

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspan1Attrs $ do
        dynText tag
        return ()
      return ()
    return ()    
  return ()



--------- Helpers for interface
flipFunc:: Bool -> Bool -- this is just not !!! -- instead of this we need a constant False in Display and viceversa
flipFunc True = False
flipFunc False = True

resetFunc:: UTCTime -> Timer -> Timer 
resetFunc u timer = timer {mode = Falling' u}

playPauseFunc:: UTCTime -> Timer -> Timer
playPauseFunc u x = changeMode (mode x) u x


--------- currently working here ------

changeMode:: Mode -> UTCTime -> Timer -> Timer -- 00000this u is the anchor from where start to count
changeMode (Holding' c) u timer = timer {mode = Falling' (addUTCTime (realToFrac (c*(-1))) u )} -- aqui se necesita mas info!!
-- this pattern abpve needs: If (Holding mark) startUTC timer then: substract second mark to startTime. OJO: the mark is ALWAYS seconds!!!

changeMode (Falling' u) u' timer = timer {mode = Holding' $ realToFrac (diffUTCTime u' u)} 
-- pattern above needs: Whenever the count is paused, it Mode should be: Holding secMark

changeMode Halted u timer = timer {mode = Falling' u}


------------------------------------------

visualiserFunc:: Timer -> Timer
visualiserFunc timer = timer {n= (((n timer)+1)`mod`numberOfVis)} -- add the mod numberOfVis later

--- controller funcas
textInputFunc:: [(T.Text,Rational)] -> Timer -> Timer
textInputFunc count timer = timer {form=cuenta}
  where cuenta = evaluate $ edit (form timer) count

-- Measure = Cycles | Seconds
measureFunc:: Timer -> Timer 
measureFunc timer 
  | (measure timer) == Seconds = timer {measure= Cycles}
  | (measure timer) == Cycles = timer {measure= Seconds}

loopFunc:: Timer -> Timer 
loopFunc timer 
  | (loop timer) == True = timer {loop= False}
  | (loop timer) == False = timer {loop= True}

-- this needs to change if other visualisers are added (either on the fly or permanently to estuary).
numberOfVis:: Int
numberOfVis = 9


----- helpers for display
fontSize':: Int -> Map Text Text
fontSize' len 
    | len <= 8 = "font-size" =: "4em"
    | len <= 12 = "font-size" =: "3em"
    | len <= 20 = "font-size" =: "2em"
    | len <= 30 = "font-size" =: "1.5em"
    | otherwise = "font-size" =: "1em"


fontSize:: Int -> Map Text Text
fontSize len 
    | len <= 8 = "font-size" =: "2.5em"
    | len <= 12 = "font-size" =: "2.0em"
    | len <= 20 = "font-size" =: "1.5em"
    | len <= 30 = "font-size" =: "1em"
    | otherwise = "font-size" =: "0.5em"

countToFallY:: Rational -> Rational -> Rational -> Map Text Text
countToFallY defH defY percent = 
  let y' = realToFrac (defY + (defH * (realToFrac percent)))/100 :: Double
      y = (realToFrac defH :: Double) + (y'*(-1))
  in "y" =: (showt y)

countToFallH:: Rational -> Rational -> Map Text Text
countToFallH defH percent = 
  let h = realToFrac (round $ defH * (realToFrac percent))/100 :: Double
  in "height" =: (showt h)
           
countToHoldY:: Rational -> Rational -> Rational -> Map Text Text
countToHoldY defH defY percent = -- percent es una cuenta regresiva del 100 al 0
  let countUp = realToFrac (100 + (percent*(-1))) :: Double
      halfClock = countUp/2
      result = (realToFrac defY :: Double) - halfClock
  in "y" =: (showt result)

countToHoldH:: Rational -> Rational -> Map Text Text
countToHoldH defH percent = 
  let countUp = realToFrac (100 + (percent*(-1))) :: Double 
      halfClock = countUp/2
  in "height" =: showt halfClock

sandClockMask:: MonadWidget t m => W t m ()
sandClockMask = do
  -- rect mask
  let x = constDyn $ "x" =: "0"
  let y = constDyn $ "y" =: "0"
  let width' = constDyn $ "width" =: "100%"
  let height' = constDyn $ "height" =: "100%"
  let fill' = constDyn $ "fill" =: "black"
  let attrsRect = mconcat [x,y,width',height',fill']
  -- clock shape attributes
  let points' = constDyn $ points [(0,100),(100,100),(50,50),(0,0),(100,0)]
  let stroke' = constDyn $ "stroke" =: "white"
  let fill'' = constDyn $ "fill" =: "white"
  let attrsClock = mconcat [stroke',points',fill'']
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "mask" (constDyn $ "id" =: "myMask") $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return () 
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "polygon" attrsClock $ return () 
    return ()
  return ()

countToPercent:: Int -> Int -> NominalDiffTime -> Int
countToPercent newSize target grains = if target == 0 then 0 else round $ (grains / (realToFrac target)) * (realToFrac newSize)

-------- points to make polygons or paths

points :: [(Int,Int)] -> Map Text Text
points [] = Data.Map.empty
points x = "points" =: (coordToText x)

coordToText:: [(Int,Int)] -> Text
coordToText p = Prelude.foldl (\ x y -> x <> " " <> (ptsToCoord y)) "" p

ptsToCoord:: (Int,Int) -> Text
ptsToCoord (x,y) = T.pack (show x) <> (T.pack ",") <> T.pack (show y)


----

-- this creates ticks from the moment estuary is built, just that
elapsedCounts:: MonadWidget t m => W t m (Event t  Rational)
elapsedCounts = do
  widgetBuildTime <- liftIO getCurrentTime
  tick <- tickLossy 0.1 widgetBuildTime
  pure $ fmap realToFrac $ fmap (\x -> diffUTCTime x widgetBuildTime) $ fmap _tickInfo_lastUTC tick

------- Helpers for engine

-- data Mode = Falling' | Halted | Holding'
extractTimeMark:: Mode -> Either Rational UTCTime
extractTimeMark (Falling' u) = Right u
extractTimeMark (Holding' mark) = Left mark
extractTimeMark Halted = Left $ realToFrac 0

-- 
-- this needs to be precisely mapped to the timers
generalOrSegmentedCount:: Int -> Bool 
generalOrSegmentedCount 8 = False -- stack timer, the only one that shows the whole program
generalOrSegmentedCount _ = True

-- this generates only whole numbers (less precise, more economic??)
loopCountUp':: Bool -> [Rational] -> Rational -> Rational
loopCountUp' True xs b = realToFrac (mod (floor b) $ floor $ sum xs) :: Rational
loopCountUp' False _ b = b

-- this generates all possible rationals (more expensive in terms of computation, more accurate??)
loopCountUp:: Bool -> [Rational] -> Rational -> Rational
loopCountUp True xs b = (unfloored - floored) * (sum xs)
  where floored = realToFrac (floor (b /sum xs)) :: Rational
        unfloored = b / sum xs
loopCountUp False _ b = b

formToText:: [(T.Text,Rational)] -> T.Text
formToText form = T.init $ T.init $ T.concat $ Prelude.map toText form
  where toText x = (fst x) <> " = " <> (showt $ (floor $ snd x  :: Int)) <> ", "

parseForm:: T.Text -> [(T.Text,Rational)] -- parser needs to accept "my thingy" as left of =
parseForm tx = 
    let x = Prelude.map (\x -> (fst x,T.drop 1 $ snd x)) $ Prelude.map (T.breakOn $ T.pack "=") $ T.split (==',') $ T.strip tx
        label = Prelude.map fst x
        durs' = Prelude.map snd x
        durs = Prelude.map fromIntegral $ Data.Maybe.mapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) durs'  
    in invalidFormFilter $ Prelude.zip label durs

invalidFormFilter:: [(T.Text,Rational)] -> [(T.Text,Rational)]
invalidFormFilter form 
  | form == [] = [("invalid form", 0)]
  | otherwise = form

-- general timer will create a countdown of the whole form ([2,3,5] will create a countdown from 10) while multiTimerPercent will create a countdown for each section of the program
generalTimerPercent:: Rational -> [Rational] -> Rational -> Rational
generalTimerPercent startPoint xs b 
  | (xs == []) = 0
  | otherwise = 100 * ((generalForm - b) / generalForm)
      where generalForm = sum xs

multiTimerPercent:: Rational -> [Rational] -> Rational -> Rational -- output represents percentage
multiTimerPercent startPoint xs b
  | (xs==[]) = 0
  | otherwise = if ts > b then tsPercent else multiTimerPercent ts (Prelude.tail xs) b
      where ts = Prelude.head $ Prelude.tail $ Prelude.scanl (+) startPoint xs 
            tsPercent = 100 *(ts - b) / (Prelude.head xs)

multiTimer:: Rational -> [Rational] -> Rational -> Rational -- Rational
multiTimer startPoint xs b
  | (xs==[]) = 0
  | otherwise = if ts > b then ts - b else multiTimer ts (Prelude.tail xs) b
      where ts = Prelude.head $ Prelude.tail $ Prelude.scanl (+) startPoint xs 

genLabel:: Rational -> [(Text,Rational)] -> Rational -> T.Text -- here cut if it is too long...
genLabel startPoint x b 
  | (x==[]) = pack ""
  | otherwise = 
      let ts = Prelude.tail $ Prelude.scanl (+) startPoint $ Prelude.map snd x 
      in if (Prelude.head ts) > b then Prelude.fst $ Prelude.head x else genLabel (Prelude.head ts) (Prelude.tail x) b


formatTextDisplay :: Measure ->  Rational -> Text
formatTextDisplay Seconds x = showt (ceiling x `div` 60 :: Int) <> ":" <> (add0Mod x)
formatTextDisplay Cycles x = showt (realToFrac (ceiling x) :: Double)

add0Mod:: Rational -> Text
add0Mod x = if modulo < 10 then ("0" <> (showt modulo)) else showt modulo 
  where modulo = (ceiling x) `mod` (60 :: Int)

loopIcon :: MonadWidget t m => Dynamic t Bool -> W t m ()
loopIcon d = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "50%"
  let vB = constDyn $  "viewBox" =: "-2 -2 28 28"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke =  setStroke <$> d
  let attrs = mconcat [class',width,height, vB, par, stroke]

  let opacity = boolOpacity <$> d
  let path = constDyn $ "d" =: "M24 20h-21.888l2.885 2.247-.665.753-4.475-3.503 4.478-3.497.665.753-2.882 2.247h20.882v-11h1v12zm-2.118-16l-2.882-2.247.665-.753 4.478 3.497-4.475 3.503-.665-.753 2.885-2.247h-20.888v11.145h-1v-12.145h21.882z"
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

setStroke:: Bool -> Map Text Text
setStroke True = "stroke" =: "var(--primary-color)"
setStroke False = "stroke" =: "var(--secondary-color)"

pathChoice:: Measure -> Map Text Text
pathChoice Seconds = "d" =: "M15 1c6.623 0 12 5.377 12 12s-5.377 12-12 12-12-5.377-12-12 5.377-12 12-12zm0 1c6.071 0 11 4.929 11 11s-4.929 11-11 11-11-4.929-11-11 4.929-11 11-11zm0 11h6v1h-7v-9h1v8z"
pathChoice Cycles = "d" =: "M7 20 L13 3 L17 3 L25 25 L5 25 L7 20 L23 20 L15 20 L15 3 L17 3 L13 3 L15 3 L15 22 L10 5 Q 8 4.5 9.25 3 L10 5 Q 11 3 9.25 3 L14.75 21 L15.5 21 L15.5 22.5 L14.5 22.5 L14.5 20.8"

measureIcons:: MonadWidget t m => Dynamic t Measure -> W t m ()
measureIcons measure = do 
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "50%"
  let vB = constDyn $  "viewBox" =: "0 0 30 30"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let fill = constDyn $ "fill" =: "transparent"

  let path = pathChoice <$> measure

  let attrs = mconcat [class',width,height, vB, par, stroke, fill]
  let pathAttrs = mconcat [path,fill,stroke]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" pathAttrs $ pure () 
  pure ()

clockIcon:: MonadWidget t m => W t m ()
clockIcon = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "-2 -2 30 30"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let fill = constDyn $ "fill" =: "transparent"

  let attrs = mconcat [class',width,height, vB, par, stroke, fill]
  let d1 = "d" =: "M12 0c6.623 0 12 5.377 12 12s-5.377 12-12 12-12-5.377-12-12 5.377-12 12-12zm0 1c6.071 0 11 4.929 11 11s-4.929 11-11 11-11-4.929-11-11 4.929-11 11-11zm0 11h6v1h-7v-9h1v8z"
  let path = constDyn $ pathBool True d1 d1

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

metronomeIcon:: MonadWidget t m => W t m ()
metronomeIcon = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "50%"
  let vB = constDyn $  "viewBox" =: "0 0 512 512"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  -- let fill = constDyn $ "fill" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke]
  let d1 = "d" =: "M442.032,192.56c-6.281-6.595-16.721-6.849-23.317-0.57l-8.089,7.703c-1.728-0.656-3.563-1.042-5.448-1.089 c-4.381-0.109-8.609,1.528-11.776,4.544l-13.577,12.931c-4.748,4.521-6.19,11.192-4.296,17.036l-19.077,18.167L328.06,59.809 c-0.755-5.087-3.838-9.531-8.342-12.018l-82.82-45.736c-4.961-2.74-10.984-2.74-15.945,0l-82.82,45.737 c-4.503,2.487-7.586,6.931-8.342,12.018L65.598,492.733c-0.704,4.744,0.694,9.738,3.826,13.37 c3.132,3.632,7.691,5.898,12.487,5.898h294.03c4.796,0,9.355-2.266,12.487-5.898c3.134-3.632,4.53-8.535,3.826-13.281 L362.371,291.24l35.906-34.215c1.881,0.714,3.865,1.078,5.855,1.078c4.086,0,8.178-1.514,11.371-4.554l13.579-12.932 c3.169-3.017,5.008-7.17,5.114-11.542c0.046-1.883-0.25-3.736-0.82-5.494l8.089-7.704 C448.058,209.595,448.313,199.156,442.032,192.56z M333.152,319.021l7.258,48.948h-58.656L333.152,319.021z M228.926,35.332 l46.871,25.884h-93.741L228.926,35.332z M158.037,94.2h141.777l27.489,184.91l-49.315,47.031V174.642 c0-9.108-7.526-16.672-16.634-16.672h-64.858c-9.108,0-16.361,7.564-16.361,16.672v193.327h-62.693L158.037,94.2z M245.005,190.953v177.015H213.12V190.953H245.005z M101.029,479.016l11.523-78.063H345.3l11.522,78.063H101.029z"
  let path = constDyn $ pathBool True d1 d1

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

pathBool:: Bool -> Map Text Text -> Map Text Text -> Map Text Text
pathBool True d1 d2 = d1
pathBool False d1 d2 = d2

flipIcon':: MonadWidget t m => Dynamic t Bool -> W t m ()
flipIcon' bool = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 50 50"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMin meet" 
  let stroke = constDyn $ "stroke" =: "var(--secondary-color)"
  let fill = constDyn $ "fill" =: "transparent"
  let opacity = constDyn $ "opacity" =: "0.3"

  let attrs = mconcat [class',width,height, vB, par, stroke, fill, opacity]
  let d1 = "d" =: "M 41.84497,3.9322539 C 32.723522,3.918239 23.597873,3.8236263 14.47905,3.8599597 10.280951,4.2162766 6.1500037,6.4966924 4.2135938,10.341822 2.6280799,13.43598 1.6417214,17.041898 2.5366669,20.49355 c 0.7163944,4.201267 3.8008953,7.825904 7.8377371,9.192912 1.894472,0.755219 3.915495,1.088006 5.946721,0.955982 8.744041,-0.02685 17.488083,-0.05369 26.232124,-0.08054 M 54.16108,17.324791 C 54.278329,23.549179 49.464093,29.380126 43.330041,30.443219 37.317176,31.715215 30.75789,28.207968 28.482775,22.496548 25.952046,16.775941 28.170208,9.4970044 33.462324,6.1609774 38.536398,2.690233 45.937178,3.4361127 50.217793,7.8477222 52.723654,10.318389 54.174719,13.80574 54.16108,17.324791 Z"
  let path = constDyn $ pathBool True d1 d1
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

flipIcon:: MonadWidget t m => Dynamic t Bool -> W t m ()
flipIcon bool = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 50 50"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  -- let fill = constDyn $ "fill" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke]
  let d1 = "d" =: "M 41.84497,3.9322539 C 32.723522,3.918239 23.597873,3.8236263 14.47905,3.8599597 10.280951,4.2162766 6.1500037,6.4966924 4.2135938,10.341822 2.6280799,13.43598 1.6417214,17.041898 2.5366669,20.49355 c 0.7163944,4.201267 3.8008953,7.825904 7.8377371,9.192912 1.894472,0.755219 3.915495,1.088006 5.946721,0.955982 8.744041,-0.02685 17.488083,-0.05369 26.232124,-0.08054 M 54.16108,17.324791 C 54.278329,23.549179 49.464093,29.380126 43.330041,30.443219 37.317176,31.715215 30.75789,28.207968 28.482775,22.496548 25.952046,16.775941 28.170208,9.4970044 33.462324,6.1609774 38.536398,2.690233 45.937178,3.4361127 50.217793,7.8477222 52.723654,10.318389 54.174719,13.80574 54.16108,17.324791 Z"
  let path = constDyn $ pathBool True d1 d1

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

boolOpacity:: Bool -> Map Text Text
boolOpacity False = "style" =: "filter: opacity(50%)"
boolOpacity True = "style" =: "filter: opacity(100%)"