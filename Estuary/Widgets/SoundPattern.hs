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


multiTextWidget::MonadWidget t m => m (Dynamic t (SoundPattern, Event t GenericSignal))
multiTextWidget = el "div" $ mdo
  let initialMap = empty::Map Int Sound
  addButton <- button' "Add" (1=:Insert (Sound Nothing))
  let constructionEvents = mergeWith union [addButton, deleteEvents]
  (values,events) <- container initialMap constructionEvents never textWidget
  values'<- forDyn values (SoundPattern . elems) -- Dyn SoundPattern
  let deleteEvents = fmap (Data.Map.map (\val->Delete)) events
  returnVal <- forDyn values' (\k-> (k,(DeleteMe<$never)))
  display values'
  return returnVal


soundPatternContainer :: MonadWidget t m => SoundPattern -> Event t () -> m (Dynamic t (SoundPattern,Event t GenericSignal))
soundPatternContainer (SoundPattern initialValues) _ = el "div" $ mdo -- not responding to input events for now...
  let initialList = intersperse' (Right ()) $ (Prelude.map (Left) initialValues)
  let initialList' = zip ([0..]::[Int]) initialList
  let initialMap = fromList initialList'
  let defNew = simpleSound "cp"
  let cEvents = mergeWith union [deleteMap,makeSimpleMap]
  (values,events) <- eitherContainer' initialMap cEvents never never errorMessageWidget plusButton
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
