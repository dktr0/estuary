{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.GeneralPattern
where

import Reflex
import Reflex.Dom
import Estuary.Reflex.Container
import qualified Estuary.Widgets.SpecificPattern as S
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)


-- widget with a slider returning a single Atom with range [minVal,maxVal] and stepsize specified
doubleSliderWidget::MonadWidget t m => (Double,Double) -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
doubleSliderWidget (minVal,maxVal) stepsize (Atom iEnd Once) _ = elAttr "table" tableAttrs $ mdo
  slider <- el "tr" $ elAttr "td" (Data.Map.union ("colspan"=:"3") ("style"=:"text-align:left")) $ do
      let attrs = constDyn $ fromList $ zip ["min","max","step","style"] [show minVal,show maxVal, show stepsize,"width:100px"]
      rangeInput <- textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs & textInputConfig_setValue .~ sliderUpdateVal & textInputConfig_initialValue .~ (show iEnd)
      let rangeVal = _textInput_value rangeInput
      mapDyn (\x-> maybe 0.5 id (readMaybe x::Maybe Double)) rangeVal
  (begEv,endEv,delEv) <- el "tr" $ do
    begPlus <- tdButtonAttrs "<" (-0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
    endPlus <- tdButtonAttrs ">" (0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border: 1pt solid black"
    return (begPlus,endPlus,deleteButton)
  let buttons = leftmost [endEv,begEv]
  let sliderValBeh = current slider
  let sliderAndButtonVal = attachWith (\a b -> max 0 $ min 1 $ a+b) sliderValBeh buttons
  let sliderUpdateVal = fmap show sliderAndButtonVal
  mapDyn (\x-> (Atom x Once,delEv)) slider
  where tableAttrs=("style"=:"margin:5px;display:inline-table;background-color:lightgreen;width:10%;border-spacing:5px;border: 2pt solid black")


-- uses clickListWidget to 
sampleNameContainer::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
sampleNameContainer (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- clickListWidget (fromList $ zip [(1::Int)..] ["~","bd","cp","bassfoo","moog", "arpy"]) (Atom iSamp iReps) repeatsEv >>= splitDyn
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border 1pt solid black") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repInitial <- holdDyn iReps $ updated repeats'
  let repeatsEv = updated repInitial
  mapDyn (\x->(x,deleteEvent)) sample
  where tableAttrs=("style"=:"display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border: 3pt solid black")
genPatButtonWidget'' _ e = genPatButtonWidget'' (Atom "~" Once) e


-- A clickable td element. Each click cycles to the next element in the map. Updated with a RepOrDiv event.
-- rep/div values get shown on the button too.
clickListWidget::(MonadWidget t m, Show a, Eq a) => Map Int a ->  GeneralPattern a -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern a, Event t GenericSignal))
clickListWidget cycleMap (Atom iVal iReps) updatedReps = mdo
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVal) $ elems cycleMap
  sampleButton <- tdButtonAttrs' showVal (iVal) $ "style"=:"width:60px;text-align:center;background-color:lightblue"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length cycleMap)
  str'' <- mapDyn (\x-> maybe iVal id $ Data.Map.lookup x cycleMap) num
  let str' = updated str''
  str <- holdDyn (iVal) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x r) str reps
  showVal <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample

-- returns atom of a character
vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget (Atom iVowel _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:("width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:1pt solid black")) $ mdo
  let vowMap = fromList $ zip [0::Int,1..] ['X','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems vowMap
  vowelButton <- tdButtonAttrs' showVowel iVowel $ "style"=:"width:50px;text-align:center;background-color:lightblue;border:1pt solid black"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 50px; text-align:center;background-color:lightblue;border:1pt solid black"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length vowMap)
  char'' <- mapDyn (\x-> maybe ('X') id $ Data.Map.lookup x vowMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Once) char
  showVowel <- mapDyn show vowel
  mapDyn (\x->(x,deleteButton)) vowel

