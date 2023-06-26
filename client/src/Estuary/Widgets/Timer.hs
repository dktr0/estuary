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


timerWidget:: MonadWidget t m => Dynamic t Timer -> W t m (Variable t Timer)
timerWidget delta = mdo
  -- liftIO $ putStrLn "timerWidget"
  initVal <- sample $ current delta
  dynMode <- holdDyn True $ newModeEv -- changes from previous to false
  let gatedDisplay = gate (current dynMode) $ leftmost [timerEv,updated delta]
  let gatedControl = gate (current (not <$> dynMode)) $ leftmost [timerEv,updated delta]
  deltaForControl <- holdDyn initVal {- $ traceEvent "deltaForControl" -} $ gatedDisplay
  deltaForDisplay <- holdDyn initVal {- $ traceEvent "deltaForDisplay" -} $ gatedControl
  let timerEv = switchDyn $ fmap fst x -- :: Event t Timer -- timer from controler
  -- let timerEv = fst x
  let newModeEv = {- traceEvent "newModeEv" $ -} switchDyn $ fmap snd x -- :: Event t Bool -- false
  -- let newModeEv = never
  x <- flippableWidget (timerControl deltaForControl) (timerDisplay deltaForDisplay) True newModeEv -- D t (E t Timer, E t Bool) -- False, watching the controler
  -- x <- timerDisplay deltaForDisplay
  variable delta timerEv



timerControl:: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
-- timerControl':: MonadWidget t m => Dynamic t Timer -> W t m ()
timerControl delta = divClass "timer-Visualiser" $ mdo
  liftIO $ putStrLn "timerControl"
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
          let iText = (formToText . form) <$> delta 
          (valTxBx,_) <- textWithLockWidget 3 ((lock . mode) <$> delta) iText 
          let boton = snd smallAreaL
          let txPressed = tag (current $ valTxBx) boton -- Event t Text
          return $ parseForm <$> txPressed -- Event t [(Text,Rat)]
        return $ textInputFunc <$> inputWrapper -- big area :: Event t (Form -> Timer Timer)     
      return (bigAreaL, fst smallAreaL) -- columnLeft


    columnRight <- divClass "columnControl" $ do
      smallAreaR <- divClass "small-areaControl" $ do
        peek mergedLocalDelta
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
    return (polyptychEvent, flipper) -- (Timer, Bool) -- return of topRowContainer

  return topContainer -- mdo last return



timerControl':: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
timerControl' delta = divClass "timer-Visualiser" $ mdo
  -- liftIO $ putStrLn "timerControl"
  dInit <- sample $ current delta
  let local = fst topContainer
  mergedLocalDelta <- holdDyn dInit {- $ traceEvent "xC" -} $ leftmost [local, updated delta]  
  iconDisplay mergedLocalDelta

  topContainer <- divClass "flex-container-col" $ do
    fstRowWrap <- divClass "flex-item-col" $ do
      divClass "flex-container-row" $ do

        txInputArea <- divClass "flex-container-col" $ do
          let iText = (formToText . form) <$> delta 
          txVal <- divClass "divForText" $ do
            (valTxBx,_) <- textWithLockWidget 3 ((lock . mode) <$> delta) iText 
            return valTxBx -- Dynamic t Text
          boton <- clickableDiv "flex-item-col" blank 
          let txPressed = tag (current $ txVal) boton -- Event t Text
          return $ parseForm <$> txPressed -- Event t [(Text,Rat)]
        let textInputEvent = textInputFunc <$> txInputArea -- Event t (Form -> Timer -> Timer)
        loopItem <- clickableDiv "flex-item-row" blank
        let loopEvent = loopFunc <$ (loopItem)
        return $ leftmost [loopEvent,textInputEvent] -- Event t (Timer -> Timer)
    sndRowWrap <- divClass "flex-item-col" $ do 
      divClass "flex-container-row" $ do
        flipItem <- clickableDiv "flex-item-row" blank -- Event t ()
        let flipEvent = flipFunc <$ flipItem
        measureItem <- clickableDiv "flex-item-row" blank -- :: Event t ()
        let measureEvent = measureFunc <$ (measureItem)
        return (measureEvent,flipEvent) 

    let flippy = id <$  (tag (constant ()) $ snd sndRowWrap)
    let polyptychEvent = attachWith (\d x -> x d) (current mergedLocalDelta) $ leftmost [fstRowWrap, fst sndRowWrap, flippy] 
    let flipper = fmap (\x -> x False) $ snd sndRowWrap 
    return (polyptychEvent, flipper) -- (Timer, Bool) -- return of topRowContainer
  return topContainer -- final return



timerDisplay:: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
-- timerDisplay':: MonadWidget t m => Dynamic t Timer -> W t m ()
timerDisplay delta = divClass "timer-Visualiser" $ mdo
  -- liftIO $ putStrLn "timerDisplay"
  dInit <- sample $ current delta
  let local = fst top
  mergedLocalDelta <- holdDyn dInit {- $ traceEvent "xD" -} $ leftmost [local,updated delta]
  timerChangeDisplay mergedLocalDelta

-- flip icon might be useful at some point  
  -- divClass "icon" $ do
  --   pure (flipIcon' $ constDyn True) >>= (divClass "iconFlippedFlip") -- flip icon
  top <- do
    topContainer <- divClass "containerTimer" $ do 
      flipItem <- clickableDiv "segmentTimer" blank
      -- flipItem <- flipItemWithinClickableAndTooltip $ text "que hay detras de la ventana?"
      let flipEvent = flipFunc <$ flipItem  
      timerStateEvents <- divClass "segmentTimer" $ do
        reset <- clickableDiv "rowTimer" blank
        pausePlay <- clickableDiv "rowTimer" blank
        aTimeReset <- performEvent $ fmap (\ _ -> liftIO $ getCurrentTime) reset
        let resetEvent = resetFunc <$> aTimeReset
        aTimePlay <- performEvent $ fmap (\ _ -> liftIO $ getCurrentTime) pausePlay
        let playPauseEvent = playPauseFunc <$> aTimePlay
        return $ leftmost [resetEvent, playPauseEvent]
      modeChangeItem <- clickableDiv "segmentTimer" blank
      let modeChangeEvent = visualiserFunc <$ modeChangeItem
      let networkedEvents = leftmost [timerStateEvents, modeChangeEvent]
      return (networkedEvents, flipEvent)
    let flippy = id <$  (tag (constant ()) $ snd topContainer)
    let polyptychEvent = attachWith (\d x -> x d) (current mergedLocalDelta) $ leftmost [fst topContainer,flippy] -- mergeWith is the proper one to use, also here you can use attachWith reverse application &!!!
    let flipper = fmap (\x -> x $ True) $ snd topContainer 
    return (polyptychEvent, flipper)
  return top


timerDisplay':: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
timerDisplay' delta = divClass "timer-Visualiser" $ mdo
  --liftIO $ putStrLn "timerDisplay"
  dInit <- sample $ current delta
  let local = fst topContainer
  mergedLocalDelta <- holdDyn dInit {- $ traceEvent "xD" -} $ leftmost [local,updated delta]
  timerChangeDisplay mergedLocalDelta

-- flip icon might be useful at some point  
  -- divClass "icon" $ do
  --   pure (flipIcon' $ constDyn True) >>= (divClass "iconFlippedFlip") -- flip icon

  topContainer <- divClass "flex-container-col" $ do
    fstRowWrap <- divClass "flex-item-col" $ do
      divClass "flex-container-row" $ do
        -- let w = divClass "hola" $ text $ T.pack "moi"
        -- tooltip w $ text $ T.pack "mui"
        resetItem <- clickableDiv "flex-item-row" blank -- :: Event t ()
        aTimeReset <- performEvent $ fmap (\ _ -> liftIO $ getCurrentTime) resetItem
        playPauseItem <- clickableDiv "flex-item-row" blank -- Event t ()
        aTimePlay <- performEvent $ fmap (\ _ -> liftIO $ getCurrentTime) playPauseItem
  --      let aTime' = traceEvent "aTime with playPauseItem and resetItem" $ aTime
        let playPauseEvent = playPauseFunc <$> aTimePlay
        let resetEvent = resetFunc <$> aTimeReset
        return $ leftmost [resetEvent,playPauseEvent]
    sndRowWrap <- divClass "flex-item-col" $ do 
      divClass "flex-container-row" $ do
        flipItem <- clickableDiv "flex-item-row" blank -- :: Event t ()
        let flipEvent = flipFunc <$ flipItem
        visualisationItem <- clickableDiv "flex-item-row" blank
        let visualisationEvent = visualiserFunc <$ visualisationItem
        return (visualisationEvent, flipEvent) -- open path for playPause      
    let flippy = id <$  (tag (constant ()) $ snd sndRowWrap)
    let polyptychEvent = attachWith (\d x -> x d) (current mergedLocalDelta) $ leftmost [fstRowWrap, fst sndRowWrap, flippy] -- mergeWith is the proper one to use, also here you can use attachWith reverse application &!!!
    let flipper = fmap (\x -> x $ True) $ snd sndRowWrap 
    return (polyptychEvent, flipper)
  return topContainer

timerChangeDisplay:: MonadWidget t m => Dynamic t Timer -> W t m ()
timerChangeDisplay timer = do 
--  liftIO $ putStrLn "timerChangeDisplay"
  --timer' <- traceDynamic "timer'" timer
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

-- think whether is better to pass UTC coming from the ticklossy rather than the Rational...
calculateCountSorC:: Measure -> Bool -> Timer -> UTCTime -> Rational -> Tempo -> Rational
calculateCountSorC Cycles textOrGraph timer wBuildT elapsingCount t = 
    calculateCount textOrGraph timer wBuildT elapsingBeat t
    where elapsingBeat = timeToCount t $ addUTCTime (realToFrac elapsingCount) wBuildT
calculateCountSorC Seconds textOrGraph timer wBuildT elapsingCount t = 
    calculateCount textOrGraph timer wBuildT elapsingCount t
---

----
-- NOTE: holdingCalculation functions and multiTimer functions perform more or less the same purpose but multiTimer depends on external cycling functions. So the looping of the time has to be calculated independently while the holdingCalc functions deal with that internally (by using cycleForm). holdingCalc should be generalised to work also as fallingCalc. 

holdingCalculation:: Bool -> Rational -> Timer -> Tempo -> Rational
holdingCalculation True mark timer t
  | (measure timer) == Seconds = holdingCalculationLooped mark $ Prelude.map snd (form timer)
  | otherwise = holdingCalculationLooped markAsBeat $ Prelude.map snd (form timer)
    where markAsBeat = (freq t) * mark
holdingCalculation False mark timer t
  | (measure timer) == Seconds = holdingCalculationUnlooped mark $ Prelude.map snd (form timer)
  | otherwise = holdingCalculationUnlooped markAsBeat $ Prelude.map snd (form timer)
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
  | (measure timer) == Seconds = holdingCalculationLoopedP mark $ Prelude.map snd (form timer)
  | otherwise = holdingCalculationLoopedP markAsBeat $ Prelude.map snd (form timer)
      where markAsBeat = (freq t) * mark
holdingCalculationP False mark timer t 
  | (measure timer) == Seconds = holdingCalculationUnloopedP mark $ Prelude.map snd (form timer)
  | otherwise = holdingCalculationUnloopedP markAsBeat $ Prelude.map snd (form timer)
      where markAsBeat = (freq t) * mark

holdingCalculationUnloopedP:: Rational -> [Rational] -> Rational
holdingCalculationUnloopedP mark (x:xs) = if mark > (sum (x:xs)) then 0 else holdingCalculationP' mark scannedForm (x:xs)
  where scannedForm = Prelude.scanl (+) x xs

holdingCalculationLoopedP:: Rational -> [Rational] -> Rational
holdingCalculationLoopedP mark (x:xs) = holdingCalculationP' markC scannedForm (x:xs)
  where markC = cycleForm mark (x:xs) 
        scannedForm = Prelude.scanl (+) x xs 

holdingCalculationP':: Rational -> [Rational] -> [Rational] -> Rational
holdingCalculationP' markC [] [] = 0
holdingCalculationP' markC (z:zs) (x:xs) = if z > markC then percen 
    else holdingCalculationP' markC zs xs   
    where percen = 100 * (z - markC) / x

cycleForm:: Rational -> [Rational] -> Rational
cycleForm mark form = dur * (tr (mark/dur)) 
    where dur = if (sum form == 0) then 1 else sum form
          tr n = n - (realToFrac (floor n) ::Rational)

cyclesOrSecs:: Measure -> Rational -> Tempo -> Rational
cyclesOrSecs Cycles n t = n / (freq t)
cyclesOrSecs Seconds n t = n * (freq t)

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
            countForm = Prelude.map snd (form delta) -- [Rat]
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
            countForm = Prelude.map snd (form delta) -- [Rat]
            loopedCountUp = loopCountUp (loop delta) countForm countUp
            countDown = multiTimerPercent 0 countForm loopedCountUp
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
  | (measure timer) == Seconds = holdingLabelLooped mark (Prelude.map snd (form timer)) $ Prelude.map fst (form timer)
  | otherwise = holdingLabelLooped markAsBeat (Prelude.map snd (form timer)) $ Prelude.map fst (form timer)
      where markAsBeat = (freq t) * mark
holdingLabel False mark timer t 
  | (measure timer) == Seconds = holdingLabelUnlooped mark (Prelude.map snd (form timer)) $ Prelude.map fst (form timer)
  | otherwise = holdingLabelUnlooped markAsBeat (Prelude.map snd (form timer)) $ Prelude.map fst (form timer)
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
            countForm = Prelude.map snd (form delta) -- [Rat]
            loopedCountUp = loopCountUp' (loop delta) countForm countUp
            label = genLabel 0 (form delta) loopedCountUp
        in label 


peek:: MonadWidget t m => Dynamic t Timer -> W t m ()
peek delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- elapsedCounts  -- :: Event t Rational -- tiempo desde origen
  beat <- holdDyn 0 beatPosition
  wBuildT <- liftIO getCurrentTime
  let count = (calculateCountSorC (measure iDelta) True iDelta wBuildT) <$> beat <*> currentTempo
  let label = calculateLabelSorC (measure iDelta) iDelta wBuildT <$> beat <*> currentTempo
  dynText label
  pure ()

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
  -- rect1
  let x' = constDyn $ "x" =: "0"
  let y' = constDyn $ "y" =: "12"
  let width1 = constDyn $ "width" =: "100"
  let height1 = constDyn $ "height" =: "30"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let attrsRect = mconcat [x',y',width1, height1, stroke] 
  -- progress rect
  let x'' = constDyn $ "x" =: "100"
  let y'' = constDyn $ "y" =: "0"
  let height2 = constDyn $ "height" =: "30"
  let opacity = constDyn $ "opacity" =: "0.5"
  let transform = constDyn $ "transform" =: "rotate(180,100,21)"
  let fill = constDyn $ "fill" =: "var(--primary-color)"
  let dynWidth = (\x -> "width" =: showt (realToFrac x :: Double)) <$> countdown
  let attrsDynRect = mconcat [x'', y'', height2, opacity, transform, fill, dynWidth]
  -- tag text
  let txFill = constDyn $ "fill" =: "var(--primary-color)"
  let txX = constDyn $ "x" =: "50"
  let txY = constDyn $ "y" =: "72"
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let fontSize = constDyn $ "font-size" =: "2em"
  -- let fontSz = a dynamic text size with...
  let txAttrs = mconcat [txFill, txX, txY, txAnchor, fontSize]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      dynText tag
      return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsDynRect $ return ()
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
  let vB = constDyn $  "viewBox" =: "0 0 100 80"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let attrs = mconcat [class',width,height, vB, par]  -- svg
  -- rect1
  let x' = constDyn $ "x" =: "0"
  let y' = constDyn $ "y" =: "12"
  let width1 = constDyn $ "width" =: "100"
  let height1 = constDyn $ "height" =: "30"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let attrsRect = mconcat [x',y',width1, height1, stroke] 
  -- progress rect
  let x'' = constDyn $ "x" =: "100"
  let y'' = constDyn $ "y" =: "0"
  let height2 = constDyn $ "height" =: "30"
  let opacity = constDyn $ "opacity" =: "0.5"
  let transform = constDyn $ "transform" =: "rotate(180,100,21)"
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

  let transform = constDyn $ "transform" =: "scale(0.67) translate(27)"
  let layerAttrs = mconcat [transform]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- creatMask first
    sandClockMask
    -- sand Falling
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "g" layerAttrs $ do
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
  let font2 = constDyn $ "font-size" =: "2.8em"
  let y'' = constDyn $ "y" =: "45"
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
  let fontScaled = (fontSize . T.length) <$> tag
  
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
  let y' = constDyn $ "y" =: "50"
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
textInputFunc count timer = timer {form=count}

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
numberOfVis = 7


----- helpers for display

fontSize:: Int -> Map Text Text -- this is not attach to anything yet... follow throu...
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
  let points' = constDyn $ points [(5,95),(95,95),(45,45),(5,5),(95,5)]
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
-- Elapsed seconds makes most sense. Basically pass the count of elapsed seconds from builtime and then make the conversions locally (to beats). You have to figure out where this operations makes the most sense, candidate: calculateCount

--- I might have to use only a tick and the UTC time of 'last tick'
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



-- this generates only whole numbers (less precise, more economic??)
-- needs to be changed to Maybes if wwe want to use it as the one below
loopCountUp':: Bool -> [Rational] -> Rational -> Rational
loopCountUp' True xs b = realToFrac (mod (floor b) $ floor $ sum xs) :: Rational
loopCountUp' False _ b = b

-- this generates all possible rationals (more expensive in terms of computation, more accurate??)
loopCountUp:: Bool -> [Rational] -> Rational -> Rational
loopCountUp True xs b = (unfloored - floored) * (sum xs)
  where floored = realToFrac (floor (b /sum xs)) :: Rational
        unfloored = b / sum xs
loopCountUp False _ b = b -- correct: 2,1,0,3,2,1,0,1,0 of [3,4,2] to: 3,2,1,4,3,2,1,2,1,0

lock:: Mode -> Bool
lock (Falling' _) = True
lock _ = False

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




-- icons 
iconDisplay:: MonadWidget t m => Dynamic t Timer -> W t m ()
iconDisplay  x = do
  divClass "icons" $ do
    divClass "icons-row" $ do
        divClass "iconTopLeft" $ do
          divClass "flex-container-col" $ do
            divClass "flex-item-col-Form" blank
            pure (refreshIcon $ constDyn True) >>= (divClass "flex-item-col-Form") -- text 
        pure (loopIcon $ (loop <$> x)) >>= (divClass "iconTopRight") -- loop
    divClass "icons-row" $ do
      pure (flipIcon $ constDyn True) >>= (divClass "iconBottomLeft") -- flip
      pure (measureIcons $ (measure <$> x)) >>= (divClass "iconBottomRight") -- measure

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

refreshIcon:: MonadWidget t m => Dynamic t Bool -> W t m ()
refreshIcon bool = do
  let class' = constDyn $ "class" =: "iconForm"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 122.61 122.88"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMax meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke]
  let d1 = constDyn $ "d" =: "M50 0 L5 30 L95 30 Z"
  let d2 = constDyn $ "d" =: "M50 25 L5 55 L95 55 Z"
  -- let d = constDyn $ "d" =: "M111.9,61.57a5.36,5.36,0,0,1,10.71,0A61.3,61.3,0,0,1,17.54,104.48v12.35a5.36,5.36,0,0,1-10.72,0V89.31A5.36,5.36,0,0,1,12.18,84H40a5.36,5.36,0,1,1,0,10.71H23a50.6,50.6,0,0,0,88.87-33.1ZM106.6,5.36a5.36,5.36,0,1,1,10.71,0V33.14A5.36,5.36,0,0,1,112,38.49H84.44a5.36,5.36,0,1,1,0-10.71H99A50.6,50.6,0,0,0,10.71,61.57,5.36,5.36,0,1,1,0,61.57,61.31,61.31,0,0,1,91.07,8,61.83,61.83,0,0,1,106.6,20.27V5.36Z"


  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    -- elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d1 $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d2 $ return ()
    return ()
  return ()

boolOpacity:: Bool -> Map Text Text
boolOpacity False = "style" =: "filter: opacity(50%)"
boolOpacity True = "style" =: "filter: opacity(100%)"
