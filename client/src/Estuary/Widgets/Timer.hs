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


-- data Timer = Timer {
--   n:: Int,
--   form:: [(Text,Rational)],
--   mode:: Mode,
--   loop:: Bool,
--   measure:: Measure
-- } deriving (Show,Eq,Ord,Generic)

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
timerControl delta = divClass "timer-Visualiser" $ mdo
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
          -- let parsed = parseForm <$> txPressed -- Event t [(Text,Rat)]
          return $ parseForm <$> txPressed -- Event t [(Text,Rat)]
          -- attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
        
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
    let flipper = fmap (\x -> x False) $ snd sndRowWrap -- this shopuld be only a Bool no need for flipFunc
    return (polyptychEvent, flipper) -- (Timer, Bool) -- return of topRowContainer
  return topContainer -- final return

timerDisplay:: MonadWidget t m => Dynamic t Timer -> W t m (Event t Timer, Event t Bool)
timerDisplay delta = divClass "timer-Visualiser" $ mdo
  --liftIO $ putStrLn "timerDisplay"
  dInit <- sample $ current delta
  let local = fst topContainer
  mergedLocalDelta <- holdDyn dInit {- $ traceEvent "xD" -} $ leftmost [local,updated delta]
  timerChangeDisplay mergedLocalDelta
  
  divClass "icon" $ do
    pure (flipIcon' $ constDyn True) >>= (divClass "iconFlippedFlip") -- flip icon

  topContainer <- divClass "flex-container-col" $ do
    fstRowWrap <- divClass "flex-item-col" $ do
      divClass "flex-container-row" $ do
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
                ------ ************** -------
        flipItem <- clickableDiv "flex-item-row" blank -- :: Event t ()
        let flipEvent = flipFunc <$ flipItem

        -- ** --

        visualisationItem <- clickableDiv "flex-item-row" blank
        let visualisationEvent = visualiserFunc <$ visualisationItem
        return (visualisationEvent, flipEvent) -- open path for playPause
      
    let flippy = id <$  (tag (constant ()) $ snd sndRowWrap)
    let polyptychEvent = attachWith (\d x -> x d) (current mergedLocalDelta) $ leftmost [fstRowWrap, fst sndRowWrap, flippy] -- mergeWith is the proper one to use, also here you can use attachWith reverse application &!!!
    let flipper = fmap (\x -> x $ True) $ snd sndRowWrap  -- this true???
    return (polyptychEvent, flipper)
  return topContainer


-- here something about holding onto the proper display...... :/
-- need to get a dynamic that represents a flip. Meaning that flip produces a new representation of the timer always..... fun times....
timerChangeDisplay:: MonadWidget t m => Dynamic t Timer -> W t m ()
timerChangeDisplay timer = do 
--  liftIO $ putStrLn "timerChangeDisplay"
  --timer' <- traceDynamic "timer'" timer
  defTimer <- sample $ current timer
  -- liftIO $ putStrLn $ show defMode
  let defN = n defTimer
  dynN <- holdDyn 0 $ updated $  n <$> timer
  let nEv = updated $ n <$> timer
  -- visualDisplay beat t timer' <$> dynN
  -- visualDisplay beat t timer' 2
  --text "adio"
  -- widgetHold_ (text "default") $ testy timer <$> nEv
  widgetHold (visualDisplay timer defN) $ visualDisplay timer <$> nEv
  pure ()

visualDisplay:: MonadWidget t m => Dynamic t Timer -> Int -> W t m ()
visualDisplay delta 0 = do  -- beat tempo generate it as close to the poitn where you need them as possible!!! engineDisplay
  visualiseProgressBar delta
  return ()

visualDisplay delta 1 = do 
  visualiseSandClock delta
  return ()

visualDisplay delta 2 = do -- here we dont need beat and tempo. Remove the pipeline
  visualiseText delta
  return ()

visualDisplay delta _ = do
  visualiseText delta
  return ()

----

calculateCount:: Bool -> Timer -> Rational -> Tempo -> Rational
calculateCount True delta beat t =  -- True calculates percentage for sandclock and progress bar
  let timeMark = (extractTimeMark . mode) delta
  in case timeMark of 
      (Left mark) -> (freq t) * mark -- elapsed upwards count, this needs to be elapsed downwarss count!!!
      (Right startMark) -> 
        let countUp = beat - (timeToCount t startMark)
            countForm = Prelude.map snd (form delta) -- [Rat]
            looper = loopBool (loop delta) countForm countUp
            countDown = multiTimerPercent 0 countForm looper
        in countDown

calculateCount False delta beat t = -- calculates concrete counts for the numeric interface
  let timeMark = (extractTimeMark . mode) delta
  in case timeMark of 
      (Left mark) -> (freq t) * mark
      (Right startMark) -> 
        let countUp = beat - (timeToCount t startMark)
            countForm = Prelude.map snd (form delta) -- [Rat]
            looper = loopBool' (loop delta) countForm countUp
            countDown = multiTimer 0 countForm looper
        in countDown

--let countdownP = (\x y -> multiTimerPercent 0 x <$> y) <$> countDyn <*> countFromBEventLooped 


-- let countFromBEventLooped = loopBool <$> (loop <$> delta) <*> countDyn <*> countFromBEvent


  --     beatAtPlayEv = timeToCount t <$> startTime
  -- in (\bpe -> beat - bpe) <$> beatAtPlayEv
    
calculateLabel:: Timer -> Rational -> Tempo -> Maybe Text
calculateLabel delta beat t = Just "nada"

----

visualiseProgressBar:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseProgressBar delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  let count = (calculateCount True iDelta) <$> beat <*> currentTempo
  let label = calculateLabel iDelta <$> beat <*> currentTempo
  drawProgressBar count label
  pure ()


drawProgressBar:: MonadWidget t m => Dynamic t Rational -> Dynamic t (Maybe Text) -> W t m ()
drawProgressBar countdown tag' = do
  -- let countdown = fromMaybe 0 <$> countdown' 
  -- countdown <- traceDynamicWith (\x -> "cd: " <> (show (realToFrac x :: Float))) $ countdown''
  let tag = fromMaybe "" <$> tag'

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


visualiseSandClock:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseSandClock delta = do
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  let count = (calculateCount True iDelta) <$> beat <*> currentTempo
  let label = calculateLabel iDelta <$> beat <*> currentTempo
  drawSandClock count label
  pure ()


drawSandClock :: MonadWidget t m => Dynamic t Rational -> Dynamic t (Maybe Text) -> W t m ()
drawSandClock countdown tag' = do
  -- dynamic stuff
  -- let countdown = fromMaybe 0 <$> countdown' 
  let tag = fromMaybe "" <$> tag'

  let yFall = countToFallY 50 0 <$> countdown
  let heightFall = countToFallH 50 <$> countdown
  let yHold = countToHoldY 0 100 <$> countdown
  let heightHold = countToHoldH 0 <$> countdown

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
  let attrsFall = mconcat [mask',class',strokeFall,x',yFall,width',heightFall]

  -- sand holder
  let widthHold = constDyn $ "width" =: "100%"
  let fillHold = constDyn $ "fill" =: "var(--primary-color)"
  let attrsHold = mconcat [mask',class',fillHold,x',yHold,widthHold,heightHold]

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


visualiseText:: MonadWidget t m => Dynamic t Timer -> W t m ()
visualiseText delta = do
  liftIO $ putStrLn "visualiseTextBuilt"
  traceDynamic "delta" $ delta
  iDelta <- sample $ current delta
  currentTempo <- Estuary.Widgets.W.tempo
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  let count = (calculateCount False iDelta) <$> beat <*> currentTempo
  let label = calculateLabel iDelta <$> beat <*> currentTempo
  drawText count label
  pure ()


drawText :: MonadWidget t m => Dynamic t Rational -> Dynamic t (Maybe Text) -> W t m ()
drawText countdown tag' = do
  -- let countdown = fromMaybe 0 <$> countdown' 
  let tag = fromMaybe "" <$> tag'

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
        dynText $ fmap (\x -> diffTimeToText $ (realToFrac x :: NominalDiffTime)) countdown
        return ()
      return ()
    return ()    
  return ()

--------- Helpers for interface
flipFunc:: Bool -> Bool -- this is just not !!! -- instead of this we need a constant False in Display and viceversa
flipFunc True = False
flipFunc False = True

resetFunc:: UTCTime -> Timer -> Timer -- prelude func id: function that returns its input!!!
resetFunc u timer = timer {mode = Falling' u}

playPauseFunc:: UTCTime -> Timer -> Timer -- this needs to be Timer -> Timer ???
playPauseFunc u x = funki (mode x) u x

funki:: Mode -> UTCTime -> Timer -> Timer
funki (Holding' c) u timer = timer {mode = Falling' (addUTCTime (realToFrac (c*(-1))) u )} -- aqui se necesita mas info!!
funki (Falling' u) u' timer = timer {mode = Holding' $ realToFrac (diffUTCTime u' u)} 
funki Halted u timer = timer {mode = Falling' u}

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

-- this needs to change if other visualisers are inputed on the fly.
numberOfVis:: Int
numberOfVis = 3


----- helpers for display

fontSize:: Int -> Map Text Text -- this is not attach to anything yet... follow throu...
fontSize len 
    | len <= 12 = "font-size" =: "1em"
    | len <= 18 = "font-size" =: "0.75em"
    | len <= 24 = "font-size" =: "0.5em"
    | len <= 30 = "font-size" =: "0.25em"
    | otherwise = "font-size" =: "0.125em"

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

--- I might have to use only a tick and the UTC time of 'last tick'
currentBeat:: MonadWidget t m => W t m (Event t Rational)
currentBeat = do
  currentTempo <- Estuary.Widgets.W.tempo -- OJO: se repito dos veces este patron, no deberia
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.1 widgetBuildTime
  pure $ attachWith timeToCount (current currentTempo) $ fmap _tickInfo_lastUTC tick

------- Helpers for engine

-- data Mode = Falling' | Halted | Holding'
extractTimeMark:: Mode -> Either Rational UTCTime
extractTimeMark (Falling' u) = Right u
extractTimeMark (Holding' mark) = Left mark
extractTimeMark Halted = Left $ realToFrac 0


-- part of engine display
  -- timeOfFall <- performEvent $ fmap (liftIO . generateStartTime) $ updated $ mode <$> delta
  -- let startTime = fmap extractStartTime $ timeOfFall -- Dynamic t (Maybe UTC)
  -- let startTimeInBeats = attachWith (\t startT -> timeToCount t <$> startT) (current tempo) $ startTime -- Event t (Maybe Rational)
  -- beatAtPlayEvent <- holdDyn (Just 0) $ traceEventWith (\x -> "fall " <> (show (fmap (\x -> realToFrac x :: Float) x))) $ startTimeInBeats -- Dyn t (Maybe Rational)
  -- let countFromBEvent' = (\b lb -> fmap (b-) lb) <$> beat <*> beatAtPlayEvent 
  -- countFromBEvent <- traceDynamicWith (\x -> "count " <> (show (fmap (\x -> realToFrac x :: Float) x))) countFromBEvent'
  -- return (countFromBEvent, constDyn $ Just "nada" )


-- engineDisplays:: MonadWidget t m => Dynamic t Rational -> Dynamic t Tempo -> Dynamic t Timer -> W t m (Dynamic t (Maybe Rational), Dynamic t (Maybe Text))
-- engineDisplays beat tempo delta = mdo
--   tempDiv <- divClass "temporaryDiv" $ do
-- --    traceDynamic "delta" delta
--     -- get the tick from inside the widget
--     let textos = constDyn "intro = 2, the lovely repetition = 3, outro = 1"
--     (valTxBx,_) <- textWithLockWidget 2 (constDyn False) textos -- Dyn t Text
--   --  boton <- button "test" -- Event ()
--     let startTime = fmap extractStartTime (mode <$> delta) -- Dynamic t (Maybe UTC)
--     let startTimeInBeats = attachWith (\t startT -> timeToCount t <$> startT) (current tempo) $ updated startTime -- Event t (Maybe Rational)
--     let beatAtPlayEvent = startTimeInBeats -- attachWith (\b stb -> fmap (b -) stb) (current beat) startTimeInBeats

-- --    let beatAtBEvent = tag (current $ beat) boton -- Event t Rational

--     lastBEventDyn <- holdDyn (Just 0) $ leftmost [beatAtPlayEvent] -- Dynamic t Rational

--     let txPressed = tag (current $ valTxBx) beatAtPlayEvent
--     targetDyn <- holdDyn [] $ fmap parseForm txPressed -- Dyn t [(Text,Rational)]
--     let countDyn = fmap (\x -> snd x) <$> targetDyn -- Dyn t [Rational]

-- --    countDyn' <- traceDynamic "countDyn" $ countDyn


--     let countFromBEvent = (\b lb -> fmap (b-) lb) <$> beat <*> lastBEventDyn

--     countFromBEvent' <- traceDynamicWith (\x -> "substract startBeat to beat: " <> (show $ fmap (\x -> realToFrac x :: Float) x)) countFromBEvent


--     let countFromBEventLooped = loopBool <$> (loop <$> delta) <*> countDyn <*> countFromBEvent

--     -- let inSecsBeat = countToTime <$> tempo <*> beat
--     -- let inSecsLastBEventDyn = countToTime <$> tempo <*> lastBEventDyn

-- --    let countdown = multiTimer 0 <$> countDyn <*> countFromBEventLooped
--     let countdownP = (\x y -> multiTimerPercent 0 x <$> y) <$> countDyn <*> countFromBEventLooped 
--     let label = (\x y -> genLabel 0 x <$> y) <$> targetDyn <*> countFromBEventLooped

--  --   let countFromBEventInSecs = diffUTCTime <$> inSecsBeat <*> inSecsLastBEventDyn
--     return (countdownP,label)
--   return tempDiv

-- this generates only whole numbers (less precise, more economic??)
-- needs to be changed to Maybes if wwe want to use it as the one below
loopBool':: Bool -> [Rational] -> Rational -> Rational
loopBool' True xs b = realToFrac (mod (floor b) $ floor $ sum xs) :: Rational
loopBool' False _ b = b

-- this generates all possible rationals (more expensive in terms of computation, more accurate??)
loopBool:: Bool -> [Rational] -> Rational -> Rational
loopBool True xs b = (unfloored - floored) * (sum xs)
  where floored = realToFrac (floor (b /sum xs)) :: Rational
        unfloored = b / sum xs
loopBool False _ b = b -- correct: 2,1,0,3,2,1,0,1,0 of [3,4,2] to: 3,2,1,4,3,2,1,2,1,0

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

diffTimeToText :: NominalDiffTime -> Text
diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> (add0Mod x)

add0Mod:: NominalDiffTime -> Text
add0Mod x = if modulo < 10 then ("0" <> (showt modulo)) else showt modulo 
  where modulo = (floor x) `mod` (60 :: Int)

-- icons 
iconDisplay:: MonadWidget t m => Dynamic t Timer -> W t m ()
iconDisplay  x = do
  divClass "icons" $ do
    divClass "icons-row" $ do
        divClass "iconTopLeft" $ do
          divClass "flex-container-col" $ do
            divClass "flex-item-col-Form" blank
            pure (structureIcon $ constDyn True) >>= (divClass "flex-item-col-Form") -- text 
        pure (loopIcon $ (loop <$> x)) >>= (divClass "iconTopRight") -- loop
    divClass "icons-row" $ do
      pure (flipIcon $ constDyn True) >>= (divClass "iconBottomLeft") -- flip
      pure (measureIcons $ (measure <$> x)) >>= (divClass "iconBottomRight") -- measure

loopIcon :: MonadWidget t m => Dynamic t Bool -> W t m ()
loopIcon d = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
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
  let height = constDyn $ "height" =: "100%"
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
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 512 512"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let fill = constDyn $ "fill" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke, fill]
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
  let fill = constDyn $ "fill" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke, fill]
  let d1 = "d" =: "M 41.84497,3.9322539 C 32.723522,3.918239 23.597873,3.8236263 14.47905,3.8599597 10.280951,4.2162766 6.1500037,6.4966924 4.2135938,10.341822 2.6280799,13.43598 1.6417214,17.041898 2.5366669,20.49355 c 0.7163944,4.201267 3.8008953,7.825904 7.8377371,9.192912 1.894472,0.755219 3.915495,1.088006 5.946721,0.955982 8.744041,-0.02685 17.488083,-0.05369 26.232124,-0.08054 M 54.16108,17.324791 C 54.278329,23.549179 49.464093,29.380126 43.330041,30.443219 37.317176,31.715215 30.75789,28.207968 28.482775,22.496548 25.952046,16.775941 28.170208,9.4970044 33.462324,6.1609774 38.536398,2.690233 45.937178,3.4361127 50.217793,7.8477222 52.723654,10.318389 54.174719,13.80574 54.16108,17.324791 Z"
  let path = constDyn $ pathBool True d1 d1

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

structureIcon:: MonadWidget t m => Dynamic t Bool -> W t m ()
structureIcon bool = do
  let class' = constDyn $ "class" =: "iconForm"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 60"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMax meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke]
  let d1 = constDyn $ "d" =: "M50 0 L5 30 L95 30 Z"
  let d2 = constDyn $ "d" =: "M50 25 L5 55 L95 55 Z"

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d1 $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d2 $ return ()
    return ()
  return ()


boolOpacity:: Bool -> Map Text Text
boolOpacity False = "style" =: "filter: opacity(50%)"
boolOpacity True = "style" =: "filter: opacity(100%)"
