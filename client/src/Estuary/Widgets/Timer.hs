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

visualiseTimerWidget:: MonadWidget t m => Dynamic t Timer -> W t m (Variable t Timer)
visualiseTimerWidget delta = mdo
  v <- variable delta $ localEdits'
  initialValue <- sample $ current delta
  let initialWidget = selectVisualiser initialValue
  let remoteOrLocalEdits = leftmost [updated delta, localEdits']
  let updatedWidgets = fmap selectVisualiser remoteOrLocalEdits -- gets an event t timer
  localEdits <- widgetHold initialWidget updatedWidgets
 -- localEdits <- widgetHold initialWidget $ traceEventWith show updatedWidgets -- m (Dynamic t (Event t TimeVi)) 
  let localEdits' = switchDyn localEdits -- Event t TimeVision
  return v

timerTracer:: MonadWidget t m => Rational ->  W t m ()
timerTracer segments = do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  beatPosition <- currentBeat -- :: Event t Rational
  beat <- holdDyn 0 beatPosition
  beat' <- holdUniqDyn beat
--  beat' <- traceDynamicWith (\x -> "beat of cyclicTracer: " ++ show (realToFrac x :: Double)) beat
  -- timer beat' currentTempo
  --visualiseText beat' $ constDyn "mu"
  timer beat' currentTempo
  return ()

-- this for playFunc and resetFunc
-- performEvent :: Event t (Performable m a) -> m (Event t a)
-- localChanges <- performEvent $ fmap (liftIO . stopWatchToNextState) y
selectVisualiser :: MonadWidget t m => Timer -> W t m (Event t Timer)
selectVisualiser (Finite 0 xs mode loop measure) = divClass "timer-Visualiser" $ do
  c <- context
  let currentTempo = fmap (tempo . ensemble . ensembleC) c
  widgetBuildTime <- liftIO $ getCurrentTime
  beatPos <- currentBeat -- Event t Rational  :: MonadWidget t m => W t m (Event t Rational)
  beat <- holdDyn 0 beatPos

  timerTracer 10
  topRowContainer <- divClass "flex-container-row" $ do
    fstRowWrap <- divClass "flex-item-row" $ do
      firstRowEvent <- divClass "flex-container-col" $ do
        resetItem <- clickableDiv "flex-item-col" blank -- :: Event t ()
        let resetEvent = resetFunc <$ resetItem -- Event t (Timer -> Timer)
        flip <- divClass "flex-item-col" blank -- :: Event t ()
        -- here flipto controlers with flipableWidget
        return resetEvent
      return firstRowEvent
    sndRowWrap <- divClass "flex-item-row" $ do 
      secondRowEvent <- divClass "flex-container-col" $ do
        playPauseItem <- clickableDiv "flex-item-col" blank -- Event t ()

        let beatAtClick = tag (current beat) playPauseItem -- Event t Rational
        let utcCurrentBeat = attachWith countToTime (current $ currentTempo) beatAtClick -- Event t UTC

--        playUTC <- sample $ hold widgetBuildTime utcCurrentBeat
        
--        let playPauseEvent = (playPauseFunc playUTC) <$ playPauseItem -- Event t (Timer -> Timer)
        visualisationItem <- clickableDiv "flex-item-col" blank
        let visualisationEvent = resetFunc <$ visualisationItem
        let rightEvents = leftmost [visualisationEvent] -- open path for playPause
        return rightEvents
      return secondRowEvent
    let polyptychEvent = fmap (\x -> x $ Finite 0 xs mode loop measure) $ leftmost [fstRowWrap,sndRowWrap]
    return polyptychEvent
  return topRowContainer



-- I suspect that Falling needs a starting time as Holding has...
-- data CurrentMode = Falling' UTCTime | Halted | Holding' UTCTime Rational

resetFunc:: Timer -> Timer
resetFunc (Finite n xs (Holding' pauseTime countTime) loop m) = Finite n xs (Holding' pauseTime 0) loop m
resetFunc (Finite n xs (Falling' fallStartMark) loop m) = Finite n xs (Falling' fallStartMark) loop m
              --  where newFallingTime = getCurrentUTC somehow... -- change this!
resetFunc (Finite n xs Halted loop m) = Finite n xs Halted loop m

-- flipFunc:: Bool -> Bool   --- ???? the flipping to controller functionality is not part of the definition :O
-- flipFunc True = False
-- flipFunc False = True

-- CurrentMode = Falling' UTCTime | Halted | Holding' UTCTime Rational

playPauseFunc:: UTCTime -> Timer -> Timer
playPauseFunc playTime (Finite n xs Halted loop m) = Finite n xs (Falling' playTime) loop m



-- playFunc (Finite n xs (Falling' fallStartMark) loop m) = Finite n xs (Holding' fallStartMark 30) loop m 
-- playFunc (Finite n xs Halted loop m) = Finite n xs Halted loop m 

visualiserFunc:: Timer -> Timer
visualiserFunc (Finite n xs mode loop m) = Finite ((n+1)`mod`numberOfVis) xs mode loop m

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

visualiseSandClock :: MonadWidget t m => Dynamic t Rational -> Dynamic t Text -> W t m ()
visualiseSandClock countdown tag = do
  -- dynamic stuff
  let yFall = countToFallY 50 0 <$> countdown
  let heightFall = countToFallH 50 <$> countdown
  let yHold = countToHoldY 0 100 <$> countdown
  let heightHold = countToHoldH 0 <$> countdown

  let class' = constDyn $ "class" =: "timer"
  let width = constDyn $ "width" =: "100%"
  let height = constDyn $ "height" =: "100%"
  let vB = constDyn $  "viewBox" =: "0 0 100 100"
  let par = constDyn $ "preserveAspectRatio" =: "xMaxYMid meet" 
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
    | len < = 18 = "font-size" =: "0.75em"
    | len <= 24 = "font-size" =: "0.5em"
    | len <= 30 = "font-size" =: "0.25em"
    |otherwise = "font-size" =: "0.125em"

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
  tick <- tickLossy 0.06666666666666667 widgetBuildTime
  pure $ attachWith timeToCount (current currentTempo) $ fmap _tickInfo_lastUTC tick


timer:: MonadWidget t m => Dynamic t Rational -> Dynamic t Tempo -> W t m ()
timer beat tempo = mdo 
  tempDiv <- divClass "temporaryDiv" $ do
    -- get the tick from inside the widget
    let textos = constDyn "intro = 20, the lovely repetition = 30, outro = 10"
    (valTxBx,_) <- textWithLockWidget 2 (constDyn False) textos -- Dyn t Text
    boton <- button "test" -- Event ()
    -- terrible loop mechanism, this will change once the definition is concieved
    tru <- button "loop"
    fals <- button "once"

    let si = tag (constant True)  tru -- Event t Bool
    let no = tag (constant False) fals -- Event t Bool
    stateOfLoop <- holdDyn False $ leftmost [si,no]

    let beatAtBEvent = tag (current $ beat) boton -- Event t Rational
    lastBEventDyn <- holdDyn 0 beatAtBEvent -- Dynamic t Rational

    let txPressed = tag (current $ valTxBx) boton
    targetDyn <- holdDyn [] $ fmap parseForm txPressed -- Dyn t [(Text,Rational)]
    let countDyn = fmap (\x -> snd x) <$> targetDyn -- Dyn t [Rational]

    let countFromBEvent = (\x y -> x - y) <$> beat <*> lastBEventDyn

    let countFromBEventLooped = loopBool <$> stateOfLoop <*> countDyn <*> countFromBEvent

    let inSecsBeat = countToTime <$> tempo <*> beat
    let inSecsLastBEventDyn = countToTime <$> tempo <*> lastBEventDyn

    let countdown = multiTimer 0 <$> countDyn <*> countFromBEventLooped
    let countdownP = multiTimerPercent 0 <$> countDyn <*> countFromBEventLooped 
    let label = genLabel 0 <$> targetDyn <*> countFromBEventLooped

    let countFromBEventInSecs = diffUTCTime <$> inSecsBeat <*> inSecsLastBEventDyn
    return ((countdownP,label))

  traceDynamicWith (\x -> "count: " ++ show (realToFrac x :: Double)) $ fst tempDiv

--  visualiseText (fst tempDiv) $ snd tempDiv
  visualiseSandClock (fst tempDiv) $ snd tempDiv
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