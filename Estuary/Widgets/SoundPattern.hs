{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.SoundPattern where

import Control.Monad
import Data.Map
import Reflex
import Reflex.Dom
import Estuary.Reflex.Utility
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Widgets.Sound




multiTextWidget'::MonadWidget t m => m (Dynamic t (SoundPattern, Event t ChildSignal))
multiTextWidget' = el "div" $ mdo
  let initialMap = empty::Map Int Sound
  addButton <- button' "Add" (1=:Insert (Sound Nothing))
  (values,events) <- container initialMap addButton (0=:Flash<$ never) textWidget
  values'<- forDyn values (SoundPattern . elems) -- Dyn SoundPattern
  forDyn values' (\k-> (k,(DeleteMe<$never)))


multiTextWidget::MonadWidget t m => m (Dynamic t (SoundPattern, Event t ChildSignal))
multiTextWidget = el "div" $ do
  (a,b)<-textWidget (Sound Nothing) never >>=splitDyn
  (c,d)<-textWidget (Sound Nothing) never >>=splitDyn
  (e,f)<-textWidget (Sound Nothing) never >>=splitDyn
  values <- combineDyn (\s1 s2 ->[s1,s2]) a c
  values'<- combineDyn (\pat s3-> SoundPattern $ pat++[s3]) values e
  let eve = DeleteMe <$ never
  forDyn values' (\k -> (k,eve))





trivialSoundPattern :: MonadWidget t m => m (Dynamic t (SoundPattern,()))
trivialSoundPattern = el "div" $ do
  x <- button' "bd cp" $ SoundPattern (Prelude.map simpleSound ["bd","cp"])
  y <- button' "arpy*4" $ SoundPattern (Prelude.map simpleSound ["arpy","arpy","arpy","arpy"])
  z <- button' "~ arp" $ SoundPattern [silentSound,simpleSound "arp"]
  pattern <- holdDyn (SoundPattern []) $ leftmost [x,y,z]
  display pattern
  mapDyn (\a -> (a,())) pattern
