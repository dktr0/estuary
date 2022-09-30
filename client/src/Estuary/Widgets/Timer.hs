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

visualiseTimerWidget:: MonadWidget t m => Dynamic t Timer -> W t m (Variable t Timer,Bool)
visualiseTimerWidget (delta,bool) = mdo
  v <- variable delta $ localEdits'
  initialValue <- sample $ current delta
  let initialWidget = flipableWidget (controlInterface initialValue)...
  let remoteOrLocalEdits = leftmost [updated delta, localEdits']
  let updatedWidgets = fmap (flippableWidget controlInterface...) remoteOrLocalEdits -- gets an event t timer
  localEdits <- widgetHold initialWidget updatedWidgets
 -- localEdits <- widgetHold initialWidget $ traceEventWith show updatedWidgets -- m (Dynamic t (Event t TimeVi)) 
  let localEdits' = switchDyn localEdits -- Event t TimeVision
  return v

timerTracer:: MonadWidget t m => Timer ->  W t m ()
timerTracer (Finite 0 xs mode loop measure) = do -- xs seq of counts, mode (playingstopped) loop measure
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  beat' <- holdUniqDyn beat
--  beat' <- traceDynamicWith (\x -> "beat of cyclicTracer: " ++ show (realToFrac x :: Double)) beat
  -- timer beat' currentTempo
  --visualiseText beat' $ constDyn "mu"
  timer beat' currentTempo loop
  return ()

-- selectVisualiser and controllerInterface have to produce a tuple: (Event t Timer, Event t Bool)
-- the snd of this tuple has to be fed into a function:: Bool -> Bool where pattern matching true with false and viceversa
-- the flippablewidget then goes into


-- this for playFunc and resetFunc
-- performEvent :: Event t (Performable m a) -> m (Event t a)
-- localChanges <- performEvent $ fmap (liftIO . stopWatchToNextState) y
selectVisualiser :: MonadWidget t m => (Timer,Bool) -> W t m (Event t Timer, Event t Bool)
selectVisualiser (Finite 0 xs mode loop measure) = divClass "timer-Visualiser" $ do
  timerTracer (Finite 0 xs mode loop measure)

  topRowContainer <- divClass "flex-container-col" $ do
    fstRowWrap <- divClass "flex-item-col" $ do
      firstRowEvent <- divClass "flex-container-row" $ do
        resetItem <- clickableDiv "flex-item-row" blank -- :: Event t ()
        let resetEvent = resetFunc <$ resetItem -- Event t (Timer -> Timer)
        flip <- divClass "flex-item-row" blank -- :: Event t ()
        -- let flipEvent = tag () $ flip -- toggle ??
        -- here flipto controlers with flipableWidget
        -- flippableWidget :: (Adjustable t m, Reflex t, MonadHold t m) => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
        -- flippableWidget b1 b2 i e = widgetHold (bool b1 b2 i) $ fmap (bool b1 b2) e
        return resetEvent
      return firstRowEvent
    sndRowWrap <- divClass "flex-item-col" $ do 
      secondRowEvent <- divClass "flex-container-row" $ do
        playPauseItem <- clickableDiv "flex-item-row" blank -- Event t ()
        visualisationItem <- clickableDiv "flex-item-row" blank
        let visualisationEvent = resetFunc <$ visualisationItem
        let rightEvents = leftmost [visualisationEvent] -- open path for playPause
        return rightEvents
      return secondRowEvent
    let polyptychEvent = fmap (\x -> x $ Finite 0 xs mode loop measure) $ leftmost [fstRowWrap,sndRowWrap]
    return polyptychEvent
  return topRowContainer

controlInterface:: MonadWidget t m => (Timer,Bool) -> W t m (Event t Timer, Event t Bool)
controlInterface (Finite 0 xs mode loop measure) = divClass "timer-Visualiser" $ do
  divClass "icons" $ do
    divClass "icons-row" $ do
        pure (structureIcon $ constDyn True) >>= (divClass "iconTopLeft") -- text -- needs to rework this bottom only
        pure (loopIcon $ constDyn True) >>= (divClass "iconTopRight") -- loop
        return ()
    divClass "icons-row" $ do
      pure (flipIcon $ constDyn True) >>= (divClass "iconBottomLeft") -- flip
      pure (tempoIcon $ constDyn True) >>= (divClass "iconBottomRight") -- tempo
      return ()
    return ()

  topRowContainer <- divClass "flex-container-col" $ do
    fstRowWrap <- divClass "flex-item-col" $ do
      firstRowEvent <- divClass "flex-container-row" $ do
        txInputArea <- divClass "flex-container-col" $ do
          let textos = constDyn "intro = 20, the lovely repetition = 30, outro = 10"
          (valTxBx,_) <- textWithLockWidget 3 (constDyn False) textos
          boton <- clickableDiv "flex-item-col" blank 
          let txPressed = tag (current $ valTxBx) boton -- Event t Text
          let parsed = parseForm <$> txPressed -- Event t [(Text,Rat)]
          parsed' <- hold [("",0)] parsed
          parsed'' <- sample parsed'
          return (parsed'',boton)
        let textInputEvent = (textInputFunc $ fst txInputArea) <$ (snd txInputArea) -- Event t (Timer -> Timer)
        measureItem <- clickableDiv "flex-item-row" blank -- :: Event t ()
        let measureEvent = measureFunc <$ measureItem
        return $ leftmost [measureEvent,textInputEvent]
      return firstRowEvent
    sndRowWrap <- divClass "flex-item-col" $ do 
      secondRowEvent <- divClass "flex-container-row" $ do
        flipItem <- clickableDiv "flex-item-row" blank -- Event t ()
        let flipEvent = tag (constant $ True) $ flipItem
        -- flippableWidget:: (Adjustable t m, Reflex t, MonadHold t m) => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
        -- flippableWidget b1 b2 i e = widgetHold (bool b1 b2 i) $ fmap (bool b1 b2) e
        flippableWidget (selectVisualiser (Finite 0 xs mode loop measure)) (controlInterface (Finite 0 xs mode loop measure)) False $ flipEvent
        -- flippable .... etc...
        loopItem <- clickableDiv "flex-item-row" blank
        let loopEvent = loopFunc <$ loopItem
        let rightEvents = (loopEvent,flipEvent) 
        return rightEvents
      return secondRowEvent
    let polyptychEvent = fmap (\x -> x $ Finite 0 xs mode loop measure) $ leftmost [fstRowWrap,fst sndRowWrap]
    return (polyptychEvent, snd sndRowWrap) -- (Timer, Bool)
  return topRowContainer


-- I suspect that Falling needs a starting time as Holding has...
-- data CurrentMode = Falling' UTCTime | Halted | Holding' UTCTime Rational

resetFunc:: Timer -> Timer
resetFunc (Finite n xs (Holding' pauseTime countTime) loop m) = Finite n xs (Holding' pauseTime 0) loop m
resetFunc (Finite n xs (Falling' fallStartMark) loop m) = Finite n xs (Falling' fallStartMark) loop m
              --  where newFallingTime = getCurrentUTC somehow... -- change this!
resetFunc (Finite n xs Halted loop m) = Finite n xs Halted loop m

-- CurrentMode = Falling' UTCTime | Halted | Holding' UTCTime Rational

playPauseFunc:: UTCTime -> Timer -> Timer -- this needs to be Timer -> Timer !!!!!!!!!
playPauseFunc playTime (Finite n xs Halted loop m) = Finite n xs (Falling' playTime) loop m

-- playFunc (Finite n xs (Falling' fallStartMark) loop m) = Finite n xs (Holding' fallStartMark 30) loop m 
-- playFunc (Finite n xs Halted loop m) = Finite n xs Halted loop m 

visualiserFunc:: Timer -> Timer
visualiserFunc (Finite n xs mode loop m) = Finite ((n+1)`mod`numberOfVis) xs mode loop m

--- controller funcas
textInputFunc:: [(T.Text,Rational)] -> Timer -> Timer
textInputFunc count (Finite n xs mode loop measure) = Finite n count mode True Seconds


-- Measure = Cycles | Seconds
measureFunc:: Timer -> Timer 
measureFunc (Finite n xs mode bool Seconds) = (Finite n xs mode True Cycles)
measureFunc (Finite n xs mode bool Cycles) = (Finite n xs mode True Seconds)

loopFunc:: Timer -> Timer 
loopFunc (Finite n xs mode True m) = Finite n xs mode False m
loopFunc (Finite n xs mode False m) = Finite n xs mode True m


-- this need sto change if other visualisers are inputed on the fly.
numberOfVis:: Int
numberOfVis = 2

visualiseText :: MonadWidget t m => Dynamic t Rational -> Dynamic t Text -> W t m ()
visualiseText countdown tag = do
  -- svg attrs
  let class' = constDyn $ "class" =: "visualiser code-font"
  let vB = constDyn $ "viewBox" =: "0 0 100 100"
  let w' = constDyn $ "width" =: "100%;"
  let h' = constDyn $ "height" =: "100%;"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet"
  let attrs = mconcat [class',w',h',vB,par]
  -- text attrs
  -- let txAttrs = constDyn $ "" =: ""
  -- tspans attrs
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let fill = constDyn $ "fill" =: "var(--primary-color)"
  let x' = constDyn $ "x" =: "50%"
  -- tspan1 attrs
  let font1 = constDyn $ "font-size" =: "1em"
  let y' = constDyn $ "y" =: "30%"
  let tspan1Attrs = mconcat [txAnchor,fill,x',y',font1]
  -- tspan2 attrs
  let font2 = constDyn $ "font-size" =: "2em"
  let y'' = constDyn $ "y" =: "75%"
  let tspan2Attrs = mconcat [txAnchor,fill,x',y'',font2]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" tspan1Attrs $ do
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspan1Attrs $ do
        dynText tag
        return ()
      elDynAttrNS' (Just "http://www.w3.org/2000/svg") "tspan" tspan2Attrs $ do
        dynText $ fmap (\x -> diffTimeToText $ (realToFrac x :: NominalDiffTime)) countdown
        return ()
      return ()
    return ()    
  return ()

visualiseProgressBar:: MonadWidget t m => Dynamic t Rational -> Dynamic t Text -> W t m ()
visualiseProgressBar countdown tag = do
  let class' = constDyn $ "class" =: "visualiser"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 100"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let attrs = mconcat [class',width,height, vB, par]  -- svg
  -- rect1
  let x' = constDyn $ "x" =: "0"
  let y' = constDyn $ "y" =: "20"
  let width1 = constDyn $ "width" =: "100"
  let height1 = constDyn $ "height" =: "20"
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let attrsRect = mconcat [x',y',width1, height1, stroke] 
  -- progress rect
  let x'' = constDyn $ "x" =: "100"
  let y'' = constDyn $ "y" =: "40"
  let height2 = constDyn $ "height" =: "20"
  let opacity = constDyn $ "opacity" =: "0.5"
  let transform = constDyn $ "transform" =: "rotate(180,100,40)"
  let fill = constDyn $ "fill" =: "var(--primary-color)"
  let dynWidth = (\x -> "width" =: showt (realToFrac x :: Double)) <$> countdown
  let attrsDynRect = mconcat [x'', y'', height2, opacity, transform, fill, dynWidth]
  -- tag text
  let txFill = constDyn $ "fill" =: "var(--primary-color)"
  let txX = constDyn $ "x" =: "50"
  let txY = constDyn $ "y" =: "13"
  let txAnchor = constDyn $ "text-anchor" =: "middle"
  let txAttrs = mconcat [txFill, txX, txY, txAnchor]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      dynText tag
      return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsRect $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsDynRect $ return ()
  return ()


visualiseSandClock :: MonadWidget t m => Dynamic t Rational -> Dynamic t Text -> W t m ()
visualiseSandClock countdown tag = do
  -- dynamic stuff
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
  let width' = constDyn $ "width" =: "100"
  let strokeFall = constDyn $ "fill" =: "var(--primary-color)"
  let mask' = constDyn $ "mask" =: "url(#myMask)"
  let attrsFall = mconcat [mask',class',strokeFall,x',yFall,width',heightFall]

  -- sand holder
  let widthHold = constDyn $ "width" =: "100"
  let fillHold = constDyn $ "fill" =: "var(--primary-color)"
  let attrsHold = mconcat [mask',class',fillHold,x',yHold,widthHold,heightHold]

  let xtx = constDyn $ "x" =: "-75%"
  let ytx = constDyn $ "y" =: "50%"
  let fs = constDyn $ "font-size" =: "1em"

  let txAttrs = mconcat [fillHold,xtx,ytx,fs]

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
      dynText tag
      return ()
    -- creatMask first
    sandClockMask
    -- sand Falling
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsFall $ return () 
    -- sand held
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" attrsHold $ return () 

  return ()

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
  let width' = constDyn $ "width" =: "100"
  let height' = constDyn $ "height" =: "100"
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
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  tick <- tickLossy 0.1 widgetBuildTime
  pure $ attachWith timeToCount (current currentTempo) $ fmap _tickInfo_lastUTC tick


timer:: MonadWidget t m => Dynamic t Rational -> Dynamic t Tempo -> Bool -> W t m ()
timer beat tempo loop = mdo 
  tempDiv <- divClass "temporaryDiv" $ do
    -- get the tick from inside the widget
    let textos = constDyn "intro = 20, the lovely repetition = 30, outro = 10"
    (valTxBx,_) <- textWithLockWidget 2 (constDyn False) textos -- Dyn t Text
    boton <- button "test" -- Event ()
    -- terrible loop mechanism, this will change once the definition is concieved
    tru <- button "loop"
    fals <- button "once"

    let si = tag (constant True)  tru -- Event t Bool -- constant :: a -> Behaviour a
    let no = tag (constant False) fals -- Event t Bool
    -- stateOfLoop <- holdDyn False $ leftmost [si,no]

    let beatAtBEvent = tag (current $ beat) boton -- Event t Rational
    lastBEventDyn <- holdDyn 0 beatAtBEvent -- Dynamic t Rational

    let txPressed = tag (current $ valTxBx) boton
    targetDyn <- holdDyn [] $ fmap parseForm txPressed -- Dyn t [(Text,Rational)]
    let countDyn = fmap (\x -> snd x) <$> targetDyn -- Dyn t [Rational]

    let countFromBEvent = (\x y -> x - y) <$> beat <*> lastBEventDyn

    let countFromBEventLooped = loopBool loop <$> countDyn <*> countFromBEvent

    let inSecsBeat = countToTime <$> tempo <*> beat
    let inSecsLastBEventDyn = countToTime <$> tempo <*> lastBEventDyn

    let countdown = multiTimer 0 <$> countDyn <*> countFromBEventLooped
    let countdownP = multiTimerPercent 0 <$> countDyn <*> countFromBEventLooped 
    let label = genLabel 0 <$> targetDyn <*> countFromBEventLooped

    let countFromBEventInSecs = diffUTCTime <$> inSecsBeat <*> inSecsLastBEventDyn
    return ((countdownP,label))

  traceDynamicWith (\x -> "count: " ++ show (realToFrac x :: Double)) $ fst tempDiv

--  visualiseText (fst tempDiv) $ snd tempDiv
  visualiseProgressBar (fst tempDiv) $ snd tempDiv
  return ()

-- --  dynText $ fmap (\x -> showt (realToFrac x ::Double)) beat -- this shows beats from booting estuary
--   divClass "." $ do 
--     dynText label
--     dynText $ fmap (\x -> showt $ (realToFrac x :: Double)) countdown
--     text "| is the countdown(s) (in beats) |"
--     return ()
--   divClass "." $ do 
--     dynText $ fmap (\x -> showt $ (realToFrac x ::Double)) countFromBEventInSecs 
--     text "| is the count up from button pressed in seconds |"
--     return ()
--   divClass "." $ do 
--     dynText $ fmap (\x -> showt $ (realToFrac x ::Double)) countFromBEvent 
--     text "| is the count up from button pressed in beats |"
--     return ()

-- this generates only whole numbers (less precise, more economic??)
loopBool':: Bool -> [Rational] -> Rational -> Rational
loopBool' True xs b = realToFrac (mod (floor b) $ floor $ sum xs) :: Rational
loopBool' False _ b = b

-- this generates all possible rationals (more expensive in terms of computation, more accurate??)
loopBool:: Bool -> [Rational] -> Rational -> Rational
loopBool True xs b = ((b / (sum xs)) - (realToFrac (floor (b / (sum xs))) :: Rational)) * (sum xs)
loopBool False _ b = b

parseForm:: T.Text -> [(T.Text,Rational)] -- parser needs to accept "my thingy" as left of =
parseForm tx = 
    let x = Prelude.map (\x -> (fst x,T.drop 1 $ snd x)) $ Prelude.map (T.breakOn $ T.pack "=") $ T.split (==',') $ T.strip tx
        label = Prelude.map fst x
        durs' = Prelude.map snd x
        durs = Prelude.map fromIntegral $ Data.Maybe.mapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) durs'  
    in Prelude.zip label durs

multiTimerPercent:: Rational -> [Rational] -> Rational -> Rational -- output represents percentage
multiTimerPercent startPoint xs b
  | (xs==[]) = 0
  | otherwise = if ts > b then tsPercent else multiTimerPercent ts (Prelude.tail xs) b
      where ts = Prelude.head $ Prelude.tail $ Prelude.scanl (+) startPoint xs 
            tsPercent = (100 *(ts - b)) / (Prelude.head xs)

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

loopIcon :: MonadWidget t m => Dynamic t Bool -> W t m ()
loopIcon bool = do
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "-2 -2 28 28"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"
  let attrs = mconcat [class',width,height, vB, par, stroke]

  let opacity = boolOpacity <$> bool
  let path = constDyn $ "d" =: "M24 20h-21.888l2.885 2.247-.665.753-4.475-3.503 4.478-3.497.665.753-2.882 2.247h20.882v-11h1v12zm-2.118-16l-2.882-2.247.665-.753 4.478 3.497-4.475 3.503-.665-.753 2.885-2.247h-20.888v11.145h-1v-12.145h21.882z"
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" path $ return () 
    return ()
  return ()

tempoIcon:: MonadWidget t m => Dynamic t Bool -> W t m ()
tempoIcon bool = do
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
  let class' = constDyn $ "class" =: "icons"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 145 154"
  let par = constDyn $ "preserveAspectRatio" =: "xMidYMid meet" 
  let stroke = constDyn $ "stroke" =: "var(--primary-color)"

  let attrs = mconcat [class',width,height, vB, par, stroke]
  let d1 = constDyn $ "d" =: "m 94.13483,57.58351 c -13.8612,0.0785 -27.722409,0.15693 -41.583613,0.2354 6.862648,-12.04339 13.725295,-24.086779 20.587943,-36.130169 6.99856,11.964923 13.99711,23.929846 20.99567,35.894769 z"
  let d2 = constDyn $ "d" =: "m 110.5332,94.263672 c -25.917818,0.069 -51.93054,-0.02851 -77.789059,0.144531 -7.886719,13.964847 -15.773438,27.929687 -23.6601566,41.894527 41.9570316,0 83.9140636,0 125.8710956,0 -8.14063,-14.01302 -16.28125,-28.02604 -24.42188,-42.039058 z m -0.14258,0.248047 c 8.04427,13.847661 16.08855,27.695311 24.13282,41.542971 -41.671225,0 -83.34245,0 -125.0136744,0 7.8053384,-13.82031 15.6106774,-27.64063 23.4160154,-41.46094 25.821613,-0.02734 51.643226,-0.05469 77.464839,-0.08203 z"
  let d3 = constDyn $ "d" =: "m 51.015257,61.644439 c 14.248963,0.01353 28.497926,0.02707 42.746889,0.0406 5.476255,10.603887 10.952514,21.207773 16.428764,31.81166 -25.656043,-0.05575 -51.312086,-0.1115 -76.968129,-0.16725 5.930825,-10.56167 11.861651,-21.12334 17.792476,-31.68501 z"
--  let d4 = constDyn $ "d" =: "M 50.939453,61.513672 C 44.956953,72.160567 38.980265,82.810804 33,93.458984 c 25.802083,0.05664 51.604167,0.113281 77.40625,0.169922 C 104.88477,82.9375 99.363281,72.246094 93.841797,61.554688 79.541016,61.541016 65.240234,61.527344 50.939453,61.513672 Z m 0.152344,0.261719 c 14.196615,0.01367 28.393229,0.02734 42.589844,0.04102 5.43099,10.516276 10.861979,21.032552 16.292969,31.548828 C 84.465495,93.309896 58.956381,93.254557 33.447266,93.199219 39.328776,82.72461 45.210287,72.25 51.091797,61.775391 Z"
--  let d5 = constDyn $ "d" =: "m 103.13867,75.431641 c -6.936197,12.172526 -13.872395,24.345053 -20.808592,36.517579 14.009114,-0.0794 28.018232,-0.15885 42.027342,-0.23828 -7.07199,-12.093642 -14.14599,-24.186109 -21.21875,-36.279299 z m 0.002,0.511718 c 6.92318,11.83789 13.84636,23.675781 20.76954,35.513671 -13.71289,0.0775 -27.425784,0.15495 -41.138676,0.23242 6.789712,-11.915364 13.579424,-23.830727 20.369136,-35.746091 z" 
  let d6 = constDyn $ "d" =: "m 71.648438,74.958984 c -4.969401,2.839844 -9.938803,5.679688 -14.908204,8.519532 2.857422,0 5.714844,0 8.572266,0 -0.612193,7.566963 -1.554734,15.435602 -1.960938,22.814454 5.569011,0 11.138021,0 16.707032,0 -0.692057,-7.604818 -1.384115,-15.209636 -2.076172,-22.814454 2.858073,0 5.716146,0 8.574219,0 -4.969401,-2.839844 -9.938802,-5.679688 -14.908203,-8.519532 z m 0,0.238282 c 4.709635,2.691406 9.419271,5.382812 14.128906,8.074218 -2.673828,0 -5.347657,0 -8.021485,0 0.692057,7.604165 1.384115,15.208331 2.076172,22.812496 -5.455729,0 -10.911458,0 -16.367187,0 0.691406,-7.604165 1.382812,-15.208331 2.074218,-22.812496 -2.673177,0 -5.346354,0 -8.019531,0 4.709636,-2.691406 9.419271,-5.382812 14.128907,-8.074218 z"

  elDynAttrNS' (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d1 $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d2 $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d3 $ return ()
    --elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d4 $ return ()
    --elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d5 $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "path" d6 $ return () 
    return ()
  return ()


-- Paths for PyramidIcon
-- "m 32.853227,94.470163 c 25.869494,-0.02747 51.738989,-0.05493 77.608483,-0.0824 8.09255,13.930477 16.18511,27.860947 24.27766,41.791417 -41.814363,0 -83.628727,0 -125.4430904,0 C 17.148595,122.27617 25.000911,108.37317 32.853227,94.470163 Z"

-- "m 110.5332,94.263672 c -25.917818,0.069 -51.93054,-0.02851 -77.789059,0.144531 -7.886719,13.964847 -15.773438,27.929687 -23.6601566,41.894527 41.9570316,0 83.9140636,0 125.8710956,0 -8.14063,-14.01302 -16.28125,-28.02604 -24.42188,-42.039058 z m -0.14258,0.248047 c 8.04427,13.847661 16.08855,27.695311 24.13282,41.542971 -41.671225,0 -83.34245,0 -125.0136744,0 7.8053384,-13.82031 15.6106774,-27.64063 23.4160154,-41.46094 25.821613,-0.02734 51.643226,-0.05469 77.464839,-0.08203 z"

-- "m 51.015257,61.644439 c 14.248963,0.01353 28.497926,0.02707 42.746889,0.0406 5.476255,10.603887 10.952514,21.207773 16.428764,31.81166 -25.656043,-0.05575 -51.312086,-0.1115 -76.968129,-0.16725 5.930825,-10.56167 11.861651,-21.12334 17.792476,-31.68501 z"

-- "M 50.939453,61.513672 C 44.956953,72.160567 38.980265,82.810804 33,93.458984 c 25.802083,0.05664 51.604167,0.113281 77.40625,0.169922 C 104.88477,82.9375 99.363281,72.246094 93.841797,61.554688 79.541016,61.541016 65.240234,61.527344 50.939453,61.513672 Z m 0.152344,0.261719 c 14.196615,0.01367 28.393229,0.02734 42.589844,0.04102 5.43099,10.516276 10.861979,21.032552 16.292969,31.548828 C 84.465495,93.309896 58.956381,93.254557 33.447266,93.199219 39.328776,82.72461 45.210287,72.25 51.091797,61.775391 Z"

-- "m 63.351889,106.18865 c 5.530767,0 11.061534,0 16.592301,0 -0.691346,-7.604804 -1.382691,-15.209609 -2.074037,-22.814413 2.765383,0 5.530767,0 8.29615,0 -4.839421,-2.765384 -9.678842,-5.530767 -14.518263,-8.296151 -4.839421,2.765383 -9.678843,5.530767 -14.518264,8.29615 2.765384,0 5.530767,10e-7 8.296151,10e-7 -0.691346,7.604804 -1.382692,15.209609 -2.074038,22.814413 z"

-- "m 71.648438,74.958984 c -4.969401,2.839844 -9.938803,5.679688 -14.908204,8.519532 2.857422,0 5.714844,0 8.572266,0 -0.612193,7.566963 -1.554734,15.435602 -1.960938,22.814454 5.569011,0 11.138021,0 16.707032,0 -0.692057,-7.604818 -1.384115,-15.209636 -2.076172,-22.814454 2.858073,0 5.716146,0 8.574219,0 -4.969401,-2.839844 -9.938802,-5.679688 -14.908203,-8.519532 z m 0,0.238282 c 4.709635,2.691406 9.419271,5.382812 14.128906,8.074218 -2.673828,0 -5.347657,0 -8.021485,0 0.692057,7.604165 1.384115,15.208331 2.076172,22.812496 -5.455729,0 -10.911458,0 -16.367187,0 0.691406,-7.604165 1.382812,-15.208331 2.074218,-22.812496 -2.673177,0 -5.346354,0 -8.019531,0 4.709636,-2.691406 9.419271,-5.382812 14.128907,-8.074218 z"

-------

-- <path d="M2084.6,5004c-171.9-19.5-437.6-93.8-633-173.9c-367.3-152.4-789.3-517.8-1006.2-867.5c-635-1027.7-369.3-2348.5,613.5-3051.8c160.2-113.3,427.9-246.2,625.2-306.7c338-105.5,230.5-101.6,3321.4-101.6c2680.6,0,2883.8,2,3044,35.2c345.8,70.3,674.1,213,933.9,406.4c158.3,117.2,414.2,381,519.7,535.3c461.1,679.9,523.6,1557.2,162.2,2282C9317.5,4453,8653.3,4918,7877.6,5005.9C7660.7,5029.4,2299.5,5027.4,2084.6,5004z M7899.1,4632.7c740.5-103.5,1338.3-623.3,1565-1359.8c48.8-158.2,52.8-197.3,52.8-513.8c0-316.5-3.9-355.6-54.7-515.8C9264.8,1592.7,8764.6,1108.1,8112,928.4l-195.4-54.7H5005.5H2094.4l-173.9,44.9c-189.5,46.9-480.6,175.8-631.1,279.4c-134.8,91.8-367.3,320.4-465,457.2c-127,177.8-240.3,420.1-297,631.1c-70.3,261.8-74.2,652.6-9.8,908.5c185.6,742.4,785.4,1305.1,1524,1430.2C2240.9,4658.1,7664.6,4666,7899.1,4632.7z"/>

-- <svg version="1.1" width="100%" height="100%" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
-- 	                viewBox="0 0 512 512" xml:space="preserve">

--     <path style="filter:opacity(100%)" stroke-width="1px" stroke="black" d="M442.032,192.56c-6.281-6.595-16.721-6.849-23.317-0.57l-8.089,7.703c-1.728-0.656-3.563-1.042-5.448-1.089
-- 	c-4.381-0.109-8.609,1.528-11.776,4.544l-13.577,12.931c-4.748,4.521-6.19,11.192-4.296,17.036l-19.077,18.167L328.06,59.809
-- 	c-0.755-5.087-3.838-9.531-8.342-12.018l-82.82-45.736c-4.961-2.74-10.984-2.74-15.945,0l-82.82,45.737
-- 	c-4.503,2.487-7.586,6.931-8.342,12.018L65.598,492.733c-0.704,4.744,0.694,9.738,3.826,13.37
-- 	c3.132,3.632,7.691,5.898,12.487,5.898h294.03c4.796,0,9.355-2.266,12.487-5.898c3.134-3.632,4.53-8.535,3.826-13.281
-- 	L362.371,291.24l35.906-34.215c1.881,0.714,3.865,1.078,5.855,1.078c4.086,0,8.178-1.514,11.371-4.554l13.579-12.932
-- 	c3.169-3.017,5.008-7.17,5.114-11.542c0.046-1.883-0.25-3.736-0.82-5.494l8.089-7.704
-- 	C448.058,209.595,448.313,199.156,442.032,192.56z M333.152,319.021l7.258,48.948h-58.656L333.152,319.021z M228.926,35.332
-- 	l46.871,25.884h-93.741L228.926,35.332z M158.037,94.2h141.777l27.489,184.91l-49.315,47.031V174.642
-- 	c0-9.108-7.526-16.672-16.634-16.672h-64.858c-9.108,0-16.361,7.564-16.361,16.672v193.327h-62.693L158.037,94.2z
-- 	M245.005,190.953v177.015H213.12V190.953H245.005z M101.029,479.016l11.523-78.063H345.3l11.522,78.063H101.029z"/>
--     </svg>



boolOpacity:: Bool -> Map Text Text
boolOpacity False = "style" =: "filter: opacity(50%)"
boolOpacity True = "style" =: "filter: opacity(100%)"

  -- <svg viewBox="-2 -2 28 28" width="100%" height="100%" preserveAspectRatio="xMidYMid meet" xmlns="http://www.w3.org/2000/svg" fill-rule="evenodd" clip-rule="evenodd">
  --   <path style="filter:opacity(50%)" d="M24 20h-21.888l2.885 2.247-.665.753-4.475-3.503 4.478-3.497.665.753-2.882 2.247h20.882v-11h1v12zm-2.118-16l-2.882-2.247.665-.753 4.478 3.497-4.475 3.503-.665-.753 2.885-2.247h-20.888v11.145h-1v-12.145h21.882z"/>
  --   <circle cx="12" cy="12" r="11.5" fill="transparent" stroke="black"></circle>
  --   <line x1="0" y1="12" x2="24" y2="12" stroke="black" transform="rotate(135,12, 12)"></line>
  --   </svg>