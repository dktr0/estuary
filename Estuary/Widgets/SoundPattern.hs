{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.SoundPattern where

import Control.Monad
import Data.Map
import Data.List (intersperse)
import Reflex
import Reflex.Dom
import Estuary.Reflex.Utility
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Widgets.Generic
import Estuary.Widgets.Sound


multiTextWidget'::MonadWidget t m => m (Dynamic t (SoundPattern, Event t GenericSignal))
multiTextWidget' = el "div" $ mdo
  let initialMap = empty::Map Int Sound
  addButton <- button' "Add" (1=:Insert (Sound Nothing))
  (values,events) <- container initialMap addButton (0=:Flash<$ never) textWidget
  values'<- forDyn values (SoundPattern . elems) -- Dyn SoundPattern
  forDyn values' (\k-> (k,(DeleteMe<$never)))


multiTextWidget::MonadWidget t m => m (Dynamic t (SoundPattern, Event t GenericSignal))
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


soundPatternContainer :: MonadWidget t m => SoundPattern -> Event t () -> m (Dynamic t (SoundPattern,Event t GenericSignal))
soundPatternContainer (SoundPattern initialValues) _ = el "div" $ mdo -- not responding to input events for now...
  let initialList = intersperse' (Right ()) $ (Prelude.map (Left) initialValues)
  let initialList' = zip ([0..]::[Int]) initialList
  let initialMap = fromList initialList'
  let defNew = simpleSound "cp"
  let cEvents = mergeWith union [deleteMap,makeSimpleMap]
  (values,events) <- eitherContainer' initialMap cEvents never never trivialSound plusButton
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys
  let deleteMap = fmap (fromList) deleteList
  let makeSimpleKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSimpleList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left defNew))])) makeSimpleKeys
  let makeSimpleMap = fmap (fromList) makeSimpleList
  mapDyn ((\x -> (x,never))  . SoundPattern . elems) values
  where
    plusButton _ _ = pingButton "+"
    intersperse' x [] = [x]
    intersperse' x xs = [x] ++ (intersperse x xs) ++ [x]
