{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.SpecificPattern
where

import Reflex
import Reflex.Dom
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse)


-- container :: (Ord k, Num k, Show k, Eq v, Show v, MonadWidget t m)
--    => Map k v                                -- a map of initial values
--    -> Event t (Map k (Construction v))       -- construction events (replace/insert/delete)
--    -> Event t (Map k w)                      -- signaling events to be delivered to child widgets
--    -> (v -> Event t w -> m (Dynamic t (v,Event t x)))                -- function to make a widget given initial value and signaling event
--    -> m ( (Dynamic t (Map k v)) , Event t (Map k x) )

--data SpecificPattern = S (GeneralPattern SampleName) | N (GeneralPattern Int) | Sound (GeneralPattern Sample) | Pan (GeneralPattern Double) deriving (Eq)


simpleSWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t ()))
simpleSWidget (S genPat) _ = el "div" $ mdo
  let initialMap = (0::Int)=:(Right ())
  addButton <- button' "add" (fromList $ zip [0::Int,1,2] [Insert $ Right (),Insert $ Left Blank,Insert $ Right ()]) -- liftM ((singleton 0 (Insert Blank)) <$) $ button "Add"
  let cEvents = leftmost [addButton, makeSimpleMap, deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never textWidget miscButton -- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys
  let deleteMap = fmap (fromList) deleteList
  let makeSimpleKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSimpleList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSimpleKeys
  let makeSimpleMap = fmap (fromList) makeSimpleList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (S $ Group x Once))
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    miscButton _ _ = pingButton "+"
    intersperse' x [] = [x]
    intersperse' x xs = [x] ++ (intersperse x xs) ++ [x]

  --
  -- eitherContainer' :: (Ord k, Num k, Show k, Eq v, Eq a, MonadWidget t m)
  --    => Map k (Either v a)                               -- a map of initial values
  --    -> Event t (Map k (Construction (Either v a)))       -- construction events (replace/insert/delete)
  --    -> Event t (Map k w)                                -- signaling events to be delivered to child widgets of type v
  --    -> Event t (Map k b)                                -- signaling events to be delivered to child widgets of type a
  --    -> (v -> Event t w -> m (Dynamic t (v,Event t e)))  -- function to build widgets for type v (returning events of type x)
  --    -> (a -> Event t b -> m (Dynamic t (a,Event t e)))  -- function to build widgets for type a (returning events of type c)
  --    -> m ( (Dynamic t (Map k v)) , Event t (Map k e) )

-- soundPatternContainer :: MonadWidget t m => SoundPattern -> Event t () -> m (Dynamic t (SoundPattern,Event t GenericSignal))
-- soundPatternContainer (SoundPattern initialValues) _ = el "div" $ mdo -- not responding to input events for now...
--   let initialList = intersperse' (Right ()) $ (Prelude.map (Left) initialValues)
--   let initialList' = zip ([0..]::[Int]) initialList
--   let initialMap = fromList initialList'
--   let defNew = simpleSound "cp"
--   let cEvents = mergeWith union [deleteMap,makeSimpleMap]
--   (values,events) <- eitherContainer' initialMap cEvents never never errorMessageWidget plusButton
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys
--   let deleteMap = fmap (fromList) deleteList
--   let makeSimpleKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSimpleList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left defNew))])) makeSimpleKeys
--   let makeSimpleMap = fmap (fromList) makeSimpleList
--   mapDyn ((\x -> (x,never))  . SoundPattern . elems) values
--   where
--     plusButton _ _ = pingButton "+"
--     intersperse' x [] = [x]
--     intersperse' x xs = [x] ++ (intersperse x xs) ++ [x]



textWidget::MonadWidget t m => GeneralPattern String-> Event t () -> m (Dynamic t (GeneralPattern String,Event t GenericSignal))
textWidget iVal _ = do
  text "Sample:"
  textField <-textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:50px;") & textInputConfig_initialValue .~ (show iVal)
  let inputVal = _textInput_value textField
  plusButton <- button "^" >>=count
  minusButton <- button "v">>=count
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\str rep-> if str=="" ||str=="~" then Blank else Atom str rep) inputVal repeats'
  forDyn genPat (\k-> (k,never))
