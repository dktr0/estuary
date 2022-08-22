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

selectVisualiser :: MonadWidget t m => Timer -> W t m (Event t Timer)
selectVisualiser timer = divClass "tempo-visualiser" $ do
    x <- do 
        x <- divClass "flex-container-for-timeVision" $ do
            testPanel <- clickableDiv "flex-item-for-timeVision" blank -- :: Event t ()
            let testEvent = timerNextState <$ testPanel -- Event t (Timer -> Timer)
            
            let panelEvent = fmap (\x -> x $ Finite 0 [10,30,20] Halted True Cycles) $ leftmost [testEvent]
            return panelEvent
        return x
    return x

timerNextState:: Timer -> Timer
timerNextState (Finite n w x y z) = (Finite (n+1) w x y z)


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
  let label = genLabel 0 <$> targetDyn <*> countFromBEventLooped

  let countFromBEventInSecs = diffUTCTime <$> inSecsBeat <*> inSecsLastBEventDyn 

--  dynText $ fmap (\x -> showt (realToFrac x ::Double)) beat -- this shows beats from booting estuary
  divClass "." $ do 
    dynText label
    dynText $ fmap (\x -> showt $ (realToFrac x :: Double)) countdown
    text "| is the countdown(s) (in beats) |"
    return ()
  divClass "." $ do 
    dynText $ fmap (\x -> showt $ (realToFrac x ::Double)) countFromBEventInSecs 
    text "| is the count up from button pressed in seconds |"
    return ()
  divClass "." $ do 
    dynText $ fmap (\x -> showt $ (realToFrac x ::Double)) countFromBEvent 
    text "| is the count up from button pressed in beats |"
    return ()

-- this generates only whole numbers (less precise, more economic??)
loopBool':: Bool -> [Rational] -> Rational -> Rational
loopBool' True xs b = realToFrac (mod (floor b) $ floor $ sum xs) :: Rational
loopBool' False _ b = b

-- this generates all possible rationals (more expensive in terms of computation, more accurate??)
loopBool:: Bool -> [Rational] -> Rational -> Rational
loopBool True xs b = ((b / (sum xs)) - (realToFrac (floor (b / (sum xs))) :: Rational)) * (sum xs)
loopBool False _ b = b

parseForm:: T.Text -> [(T.Text,Rational)]
parseForm tx = 
    let x = Prelude.map (\x -> (fst x,T.drop 1 $ snd x)) $ Prelude.map (T.breakOn $ T.pack "=") $ T.split (==',') $ T.strip tx
        label = Prelude.map fst x
        durs' = Prelude.map snd x
        durs = Prelude.map fromIntegral $ Data.Maybe.mapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) durs'  
    in Prelude.zip label durs


multiTimer:: Rational -> [Rational] -> Rational -> Rational
multiTimer startPoint x  b
  | (x==[]) = 0
  | otherwise = if (Prelude.head ts) > b then (Prelude.head ts) - b else multiTimer (Prelude.head ts) (Prelude.tail x) b
      where ts = Prelude.tail $ Prelude.scanl (+) startPoint x 

genLabel:: Rational -> [(Text,Rational)] -> Rational -> T.Text
genLabel startPoint x b 
  | (x==[]) = pack ""
  | otherwise = 
      let ts = Prelude.tail $ Prelude.scanl (+) startPoint $ Prelude.map snd x 
      in if (Prelude.head ts) > b then Prelude.fst $ Prelude.head x else genLabel (Prelude.head ts) (Prelude.tail x) b