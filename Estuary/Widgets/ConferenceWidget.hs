{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.ConferenceWidget
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

--------------------------------------
--              Config 1:           --
--------------------------------------
eldadWidget:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
eldadWidget iChain _ = elAttr "table" tableAttrs $ do
  sPattern<- elAttr "tr" ("style"=:"background-color:lightgrey") $ do
    elAttr "b" ("style"=:"font-size:100%;margin:5px") $ text "S"
    (s,_)<- sContainerWidget (Estuary.Tidal.Types.Sound Blank) never >>= splitDyn
    return s
  endPattern <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
    elAttr "b" ("style"=:"font-size:100%;margin:5px") $ text "End"
    (pat,_) <- doubleContainerWidget (End $ Atom 0.5 Once) never >>= splitDyn
    return pat
  vowelPattern <- elAttr "tr" ("style"=:"background-color:wheat") $ do
    elAttr "b" ("style"=:"font-size:100%;margin:5px;") $ text "Vow"
    (pat,_) <- charContainerWidget (Vowel $ Atom '~' Once) never >>= splitDyn
    return pat
  patChain' <- combineDyn (\s e-> PatternChain' (TransformedPattern [NoTransformer] s) Merge (PatternChain $ TransformedPattern [NoTransformer] e)) endPattern vowelPattern
  patChain <- combineDyn (\chain pat-> PatternChain' (TransformedPattern [NoTransformer] pat) Merge chain) patChain' sPattern
  mapDyn (\x-> (x,never)) patChain
  where
    tableAttrs=("style"=:"display:inline-table")

-- S Pattern
genPatButtonWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
genPatButtonWidget (Atom iSamp iReps) _ = elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- sButtonWidget (Atom iSamp iReps) repeatsEv >>= splitDyn
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repInitial <- holdDyn iReps $ updated repeats'
  let repeatsEv = updated repInitial
  mapDyn (\x->(x,deleteEvent)) sample
  where tableAttrs=("style"=:("margin:5px;display:inline-table;background-color:lightgreen;width:10%;padding:6px;border-spacing:5px;border:"++widgetBorder))
genPatButtonWidget _ e = genPatButtonWidget (Atom "~" Once) e

sContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never genPatButtonWidget (pingButton''' "+" ("style"=:"background-color:lightblue"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (S $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

-- End Pattern
doubleContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
doubleContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (pingButton''' "+" ("style"=:"background-color:lightblue"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (endWidget,Atom (1) Once, End)

endWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
endWidget (Atom iEnd Once) _ = elAttr "table" tableAttrs $ mdo
  slider <- el "tr" $ elAttr "td" (Data.Map.union ("colspan"=:"3") ("style"=:"text-align:left")) $ do
      let attrs = constDyn $ fromList $ zip ["min","max","step","style"] ["0","1","0.05","width:100px"]
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

-- Vowel Pattern
charContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (pingButton''' "+" ("style"=:"text-align:center;width:20px;background-color:lightblue"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    tableAttrs=("style"=:"display:inline-table;border-spacing:5px;")
    (widgetBuilder,defaultGeneralPat, patType) = (vowelButtonWidget, Atom '~' Once, Vowel)

vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget (Atom iVowel _) _ = elAttr "table" ("style"=:"border-spacing:5px;display:inline-table;background-color:lightgreen;border:3pt solid black") $ mdo
  let sampleMap = fromList $ zip [0::Int,1..] ['~','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems sampleMap
  vowelButton <- tdButtonAttrs' (showVowel) (iVowel) $ "style"=:"width:80px;text-align:center;background-color:lightblue"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 80px; text-align:center;background-color:lightblue"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  char'' <- mapDyn (\x-> maybe ('~') id $ Data.Map.lookup x sampleMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Once) char
  showVowel <- mapDyn show vowel
  mapDyn (\x->(x,deleteButton)) vowel

----------------------------------------
--              Config 2              --
----------------------------------------

eldadWidget':: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
eldadWidget' iChain _ = do
  sPattern<- elAttr "div" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "b" ("style"=:"font-size:100%;margin:5px") $ text "S"
    (s,_)<- sContainerWidget' (Estuary.Tidal.Types.S Blank) never >>= splitDyn
    return s
  endPattern <- elAttr "div" ("style"=:"background-color:Lightyellow") $ do
    elAttr "b" ("style"=:"font-size:100%;margin:5px") $ text "End"
    (pat,_) <- doubleContainerWidget' (End $ Atom 0.5 Once) never >>= splitDyn
    return pat
  vowelPattern <- elAttr "div" ("style"=:"background-color:wheat") $ do
    elAttr "b" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
    (pat,_) <- charContainerWidget' (Vowel $ Atom '~' Once) never >>= splitDyn
    return pat
  patChain' <- combineDyn (\s e-> PatternChain' (TransformedPattern [NoTransformer] s) Merge (PatternChain $ TransformedPattern [NoTransformer] e)) endPattern vowelPattern
  patChain <- combineDyn (\chain pat-> PatternChain' (TransformedPattern [NoTransformer] pat) Merge chain) patChain' sPattern
  mapDyn (\x-> (x,never)) patChain

-- S Pattern
sContainerWidget'::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sContainerWidget' (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never genPatButtonWidget' (pingButton''' "+" ("style"=:"background-color:lightblue"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (S $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

genPatButtonWidget'::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
genPatButtonWidget' (Atom iSamp iReps) _ = elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- sButtonWidget (Atom iSamp iReps) repeatsEv >>= splitDyn
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repInitial <- holdDyn iReps $ updated repeats'
  let repeatsEv = updated repInitial
  mapDyn (\x->(x,deleteEvent)) sample
  where tableAttrs=("style"=:"margin:5px;display:inline-table;background-color:lightgreen;width:10%;padding:6px;border-spacing:5px;border: 3pt solid black")
genPatButtonWidget' _ e = genPatButtonWidget' (Atom "~" Once) e

-- End Pattern
doubleContainerWidget':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
doubleContainerWidget' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (pingButton''' "+" ("style"=:"background-color:lightblue"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (endWidget', Atom (1) Once, End)

endWidget'::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
endWidget' (Atom iEnd Once) _ = elAttr "table" tableAttrs $ mdo
  slider <- el "tr" $ elAttr "td" (Data.Map.union ("colspan"=:"3") ("style"=:"text-align:left")) $ do
      let attrs = constDyn $ fromList $ zip ["min","max","step","style"] ["0","1","0.05","width:100px"]
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


-- Vowel Pattern

charContainerWidget':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton "+" (Ping) ("style"=:"text-align:center;display:inline-table;max-width:20%;background-color:lightblue;height:50%"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    tableAttrs=("style"=:"text-align:center;width:90%;display:inline-table;border-spacing:5px;")
    (widgetBuilder,defaultGeneralPat, patType) = (vowelButtonWidget', Atom '~' Once, Vowel)

vowelButtonWidget'::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget' (Atom iVowel _) _ = elAttr "table" ("style"=:"border-spacing:5px;display:inline-table;background-color:lightgreen;border:3pt solid black") $ mdo
  let sampleMap = fromList $ zip [0::Int,1..] ['~','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems sampleMap
  vowelButton <- tdButtonAttrs' (showVowel) (iVowel) $ "style"=:"width:80px;text-align:center;background-color:lightblue"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 80px; text-align:center;background-color:lightblue"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  char'' <- mapDyn (\x-> maybe ('~') id $ Data.Map.lookup x sampleMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Once) char
  showVowel <- mapDyn show vowel
  mapDyn (\x->(x,deleteButton)) vowel


----------------------------------------
--              Config 3              --
----------------------------------------


eldadWidget'':: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
eldadWidget'' iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
  s<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "S"
    (pat,_)<- sContainerWidget'' (Estuary.Tidal.Types.S Blank) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  end <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
    (pat,_) <- doubleContainerWidget'' (End $ Atom 0.5 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "tr" ("style"=:"background-color:wheat") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
    (pat,_) <- charContainerWidget'' (Vowel $ Atom '~' Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  patChain' <- combineDyn (\e v-> PatternChain' e Merge (PatternChain v)) end vowel
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
--  patChain' <- combineDyn (\e v->case toPatternChain e of EmptyPatternChain-> toPatternChain v; otherwise->PatternChain' e Merge $ toPatternChain v) end vowel
  --patChain <- combineDyn (\chain pat-> PatternChain' pat Merge chain ) patChain' s
  mapDyn (\x-> (x,never)) patChain
  where
    toPatternChain (TransformedPattern _ (End  (Group [] _))) = EmptyPatternChain
    toPatternChain (TransformedPattern _ (Vowel (Group [] _))) = EmptyPatternChain
    toPatternChain x = PatternChain x

--
-- eldadWidget''':: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
-- eldadWidget''' iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
--   sPattern<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
--     elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "S"
--     (pat,_)<- sContainerWidget'' (Estuary.Tidal.Types.S Blank) never >>= splitDyn
--     pat' <- forDyn pat (\x-> if x == (Estuary.Tidal.Types.S $ Group [] Once) then EmptyPatternChain else PatternChain $ TransformedPattern [NoTransformer] $ x)
--     return pat'
--   endPattern <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
--     elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
--     (pat,_) <- doubleContainerWidget'' (End $ Atom 0.5 Once) never >>= splitDyn
--     pat' <- forDyn pat (\x-> if x == (End $ Group [] Once) then EmptyPatternChain else PatternChain $ TransformedPattern [NoTransformer] x)
--     return pat'
--   vowelPattern <- elAttr "tr" ("style"=:"background-color:wheat") $ do
--     elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
--     (pat,_) <- charContainerWidget'' (Vowel $ Atom '~' Once) never >>= splitDyn
--     pat' <- forDyn pat (\x-> if x == (Vowel $ Group [] Once) then EmptyPatternChain else PatternChain $ TransformedPattern [NoTransformer] $ x )
--     return pat'
--   patChain' <- combineDyn (\e v-> if v == EmptyPatternChain then e else PatternChain'' e Merge v) endPattern vowelPattern
--   patChain <- combineDyn (\chain pat-> if chain == EmptyPatternChain then pat else PatternChain'' pat Merge chain) patChain' sPattern
--   mapDyn (\x-> (x,never)) patChain



-- S Pattern
sContainerWidget''::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sContainerWidget'' (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never genPatButtonWidget'' (tdPingButton' "+" ("style"=:"text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:50%;vertical-align:middle"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (S $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

genPatButtonWidget''::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
genPatButtonWidget'' (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- sButtonWidget (Atom iSamp iReps) repeatsEv >>= splitDyn
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

-- End Pattern
doubleContainerWidget'':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
doubleContainerWidget'' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:"text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:50%;vertical-align:middle"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> patType $ Group x Once)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (endWidget'', Atom (1) Once, End)

-- swap with endWidget'' in doubleContainerWidget for use.
endWidget2''::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
endWidget2'' (Atom iEnd Once) _ = elAttr "td" tableAttrs $ mdo
  slider <- el "tr" $ elAttr "td" (Data.Map.union ("colspan"=:"3") ("style"=:"text-align:left")) $ do
      let attrs = constDyn $ fromList $ zip ["min","max","step","style"] ["0","1","0.05","width:100px"]
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
  where tableAttrs=("style"=:"margin:5px;text-align:center;display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border: 2pt solid black")


-- Vowel Pattern

charContainerWidget'':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget'' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:"text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:50%;vertical-align:middle"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    tableAttrs=("style"=:"text-align:center;display:inline-table;border-spacing:5px;")
    (widgetBuilder,defaultGeneralPat, patType) = (vowelButtonWidget'', Atom '~' Once, Vowel)

vowelButtonWidget''::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget'' (Atom iVowel _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:"width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:3pt solid black") $ mdo
  let sampleMap = fromList $ zip [0::Int,1..] ['~','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems sampleMap
  vowelButton <- tdButtonAttrs' (showVowel) (iVowel) $ "style"=:"width:50px;text-align:center;background-color:lightblue;border:1pt solid black"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 50px; text-align:center;background-color:lightblue;border:1pt solid black"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  char'' <- mapDyn (\x-> maybe ('~') id $ Data.Map.lookup x sampleMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Once) char
  showVowel <- mapDyn show vowel
  mapDyn (\x->(x,deleteButton)) vowel


-- Experimental:
endWidget''::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
endWidget'' (Atom iEnd Once) _ = elAttr "td" ("style"=:"text=align:center;margin:10px") $ mdo
  (returnVal,attrs) <- elDynAttr "td" attrs $ do
    (begEv,endEv,delEv) <- el "tr" $ do
      begPlus <- tdButtonAttrs "<" (-0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
      endPlus <- tdButtonAttrs ">" (0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
      deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border: 1pt solid black"
      return (begPlus,endPlus,deleteButton)
    let buttons = leftmost [endEv,begEv]
    endVal' <- foldDyn (\a b-> min 1 $ max 0 $ a+b) iEnd buttons
    endVal <- mapDyn (\x-> (fromInteger $ round $ x*100)/100) endVal'
    endGradient <- forDyn endVal makeStyleString
    tableAttrs <- forDyn endGradient (\x->"style"=:("text-align:center;display:inline-table;width:100pt;border-spacing:5px;border: 2pt solid black;"++x))
    val <- mapDyn (\x-> (Atom x Once,delEv)) endVal
    return $ (val, tableAttrs)
  return returnVal



------------------------
--      conf 4        --
------------------------



eldadWidget''':: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
eldadWidget''' iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
  s<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "S"
    (pat,_)<- sContainerWidget''' (Blank) never >>= splitDyn
    forDyn pat (\x-> TransformedPattern [NoTransformer] $ S x)
  end <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
    (pat,_) <- endContainerWidget''' (End $ Atom 0.5 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "tr" ("style"=:"background-color:wheat") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
    (pat,_) <- charContainerWidget''' (Vowel $ Atom '~' Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  up <- elAttr "tr" ("style"=:"background-color:lightcyan") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Up"
    (pat,_) <- upContainerWidget''' (Up $ Atom 0 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
  patChain' <- combineDyn (\e p-> PatternChain' e Merge p) end patChain''
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
  mapDyn (\x-> (x,never)) patChain
  where
    toPatternChain (TransformedPattern _ (End  (Group [] _))) = EmptyPatternChain
    toPatternChain (TransformedPattern _ (Vowel (Group [] _))) = EmptyPatternChain
    toPatternChain x = PatternChain x



--
-- widget iBuilder newBuilder iVal iUpdateEvent = mdo
--   val <- widgetHold (iBuilder iVal iUpdateEvent) (fmap (\x-> if x == MakeGroup then newBuilder iVal iUpdateEvent else iBuilder iVal iUpdateEvent) $ switchPromptlyDyn upEv)
--   let val' = joinDyn val
--   upEv <- forDyn val' (\(returnVal, updateEv)->updateEv)
--   return val'
--

f ::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
f (Atom x r) e = groupableWidget (Atom x r) e
f (Blank) e = groupableWidget (Blank) e
f (Group xs r) e = sContainerWidget''' (Group xs r) e
f (Layers xs r) e = sContainerWidget''' (Layers xs r) e


widget::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
widget iVal updateEvent = mdo
  --let ev = fmap (\x->if x ==MakeGroup then sContainerWidget''' (S Blank) updateEvent else )
  val <- widgetHold (f (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> f (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'

  -- get events (GenericSignal) out of val
  -- filter for RebuildMe
  -- when RebuildMe, constructionEvents = an event wrapping f (and the newest value of the child widget)

  -- val <- widgetHold (groupableWidget (Blank) updateEvent) (fmap (\x-> sContainerWidget''' x updateEvent) upEv)
  -- let val' = joinDyn val
  -- val <- forDyn val' (\(returnVal, updateEv)->returnVal)
  -- ev <- forDyn val' (\(returnVal, updateEv)->updateEv)
  --
  -- let upEv = tagDyn val updateEvent
  -- return val'



-- S Pattern
sContainerWidget'''::(MonadWidget t m) => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName,Event t GenericSignal))
sContainerWidget''' (Atom iVal iReps) _ =  elAttr "td" ("style"=:"border: 1pt solid red") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  -- (values,events) <- eitherContainer' initialMap cEvents never  never genPatButtonWidget''' (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  --widget:: MonadWidget t m => a -> Event t b -> (a -> Event t b -> m (Dynamic t (a, Event t c))) -> (a -> Event t b -> m (Dynamic t (a, Event t c))) -> Event t b -> m (Dynamic t (a, Event t c))
  (values,events) <- eitherContainer' initialMap cEvents never never widget (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
sContainerWidget''' (Group xs iReps) _ =  elAttr "td" ("style"=:"border: 1pt solid red") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  -- (values,events) <- eitherContainer' initialMap cEvents never  never genPatButtonWidget''' (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  --widget:: MonadWidget t m => a -> Event t b -> (a -> Event t b -> m (Dynamic t (a, Event t c))) -> (a -> Event t b -> m (Dynamic t (a, Event t c))) -> Event t b -> m (Dynamic t (a, Event t c))
  (values,events) <- eitherContainer' initialMap cEvents never never widget (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
sContainerWidget''' (Layers xs r) e = elAttr "td" ("style"=:"border: 2pt solid blue") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  -- (values,events) <- eitherContainer' initialMap cEvents never  never genPatButtonWidget''' (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  --widget:: MonadWidget t m => a -> Event t b -> (a -> Event t b -> m (Dynamic t (a, Event t c))) -> (a -> Event t b -> m (Dynamic t (a, Event t c))) -> Event t b -> m (Dynamic t (a, Event t c))
  (values,events) <- eitherContainer' initialMap cEvents never never widget (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Layers x Once))
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
sContainerWidget''' _ e = sContainerWidget''' (Atom "~" Once) e


genPatButtonWidget'''::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
genPatButtonWidget''' (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty) $ do
    (sampName,_) <- el "td" $ sButtonWidget''' (Atom iSamp iReps) never >>= splitDyn
    elAttr "td" ("style"=:"width:15%") $ forDyn repsHold show >>= dynText
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
    return $ (sampName,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ fromList $ zip ["style","colspan"] ["text-align:center; background-color:lightblue;border: 1pt solid black","2"]
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repsHold <- holdDyn iReps $ updated repeats'
  sample' <- combineDyn Atom sample repsHold
  mapDyn (\x->(x,deleteEvent)) sample'
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
genPatButtonWidget''' _ e = genPatButtonWidget''' (Atom "~" Once) e

sButtonWidget'''::MonadWidget t m =>  GeneralPattern SampleName -> Event t RepOrDiv -> m (Dynamic t (SampleName, Event t GenericSignal))
sButtonWidget''' (Atom iSamp iReps) updatedReps = mdo
  let sampleMap = fromList $ zip [0::Int,1..] $ concat [[("Rest","~")],fmap (\x->("Bass",x)) ["bd", "bassfoo", "less"], fmap (\x->("Glitch",x)) ["glitch", "click", "glitch2", "casio", "hardcore", "house", "cosmic"], fmap (\x->("Crashes",x)) ["cc"], fmap (\x->("Long Sounds",x)) ["sheffield", "ade", "padlong", "tacscan"], fmap (\x->("Humans and Animals",x)) ["h", "baa", "crow", "numbers", "alphabet", "hmm"]]
  let initialIndex = maybe 0 id $ Data.List.findIndex (==iSamp) $ fmap snd $ elems sampleMap
  dd <- dropdownOpts initialIndex sampleMap def
  let val = _dropdown_value dd
  val' <- forDyn val (\x -> maybe "~" snd $ Data.Map.lookup x sampleMap)
  forDyn val' (\x-> (x,never))


-- End Pattern
endContainerWidget''':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
endContainerWidget''' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> patType $ Group x Once)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (endWidget''', Atom (1) Once, End)

endWidget'''::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
endWidget''' (Atom iEnd Once) _ = elAttr "td" ("style"=:"text-align:center;margin:10px") $ mdo
  (returnVal,attrs) <- elDynAttr "td" attrs $ do
    (begEv,endEv,delEv) <- el "tr" $ do
      begPlus <- tdButtonAttrs "<" (-0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
      endPlus <- tdButtonAttrs ">" (0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
      deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border: 1pt solid black"
      return (begPlus,endPlus,deleteButton)
    let buttons = leftmost [endEv,begEv]
    endVal' <- foldDyn (\a b-> min 1 $ max 0 $ a+b) iEnd buttons
    endVal <- mapDyn (\x-> (fromInteger $ round $ x*100)/100) endVal'
    endGradient <- forDyn endVal makeStyleString
    tableAttrs <- forDyn endGradient (\x->"style"=:("text-align:center;display:inline-table;width:100pt;border-spacing:5px;border:"++widgetBorder++";"++x))
    val <- mapDyn (\x-> (Atom x Once,delEv)) endVal
    return $ (val, tableAttrs)
  return returnVal

upContainerWidget''':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
upContainerWidget''' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> patType $ Group x Once)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (upWidget''', Atom 0 Once, Up)

upWidget'''::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
upWidget''' (Atom iUpVal _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  upCount <- elAttr "tr" (empty)$ do
    elAttr "td" ("style"=:"text-align:center")$ dynText upValShow
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border 1pt solid black") >>= count
    return upButton
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  upVal <- combineDyn (\a b ->  (a)-(b)+(round iUpVal)::Int ) upCount downCount
  upValShow <- forDyn upVal show
  --repsHold <- holdDyn iUpVal $ updated repeats
  mapDyn (\x->(Atom (fromIntegral x) Once,deleteEvent)) upVal
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
upWidget''' _ e = upWidget''' (Atom 0 Once) e

-- Vowel Pattern
charContainerWidget''':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget''' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    tableAttrs=("style"=:"text-align:center;display:inline-table;border-spacing:5px;")
    (widgetBuilder,defaultGeneralPat, patType) = (vowelButtonWidget''', Atom '~' Once, Vowel)

vowelButtonWidget'''::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget''' (Atom iVowel _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:("width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:"++widgetBorder)) $ mdo
  let sampleMap = fromList $ zip [0::Int,1..] ['~','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems sampleMap
  vowelButton <- tdButtonAttrs' (showVowel) (iVowel) $ "style"=:"width:50px;text-align:center;background-color:lightblue;border:1pt solid black"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 50px; text-align:center;background-color:lightblue;border:1pt solid black"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  char'' <- mapDyn (\x-> maybe ('~') id $ Data.Map.lookup x sampleMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Once) char
  showVowel <- mapDyn show vowel
  vowel' <- mapDyn (\x -> if x =='~' then Atom 't' Once else Atom x Once) char
  mapDyn (\x->(x,deleteButton)) vowel'



  -- (selectElement,_) <- elAttr' "select" (empty) $ do
  --   elAttr "optgroup" ("label"=:"Rests") $ do
  --     elAttr "option" ("value"=:"~") $ text "~"
  --     elAttr "option" ("value"=:"a") $ text "a"
  -- selectEv <- wrapDomEvent (_el_element selectElement) (onEventName Click) (mouseXY) -- Event t Int
  -- let selectEv' = fmap show selectEv -- ev string
  -- val <- holdDyn ("nothing") selectEv'
  -- forDyn val (\x-> (Atom x Once, never))


------------------------
--     Universal      --
------------------------
sButtonWidget::MonadWidget t m =>  GeneralPattern SampleName -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
sButtonWidget (Atom iSamp iReps) updatedReps = mdo
  let sampleMap = fromList $ zip [(0::Int)..] ["~","bd","sn","cp","hh"]  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iSamp) $ elems sampleMap
  sampleButton <- tdButtonAttrs' (showSample) (iSamp) $ "style"=:"width:60px;text-align:center;background-color:lightblue"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  str'' <- mapDyn (\x-> maybe ("~") id $ Data.Map.lookup x sampleMap) num
  let str' = updated str''
  str <- holdDyn (iSamp) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x r) str reps
  showSample <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample

-- Height of plus td plus buttons
plusHeight = "30px"
widgetBorder = "2pt solid black"

makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100








  --
  -- eldadWidget''':: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
  -- eldadWidget''' iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
  --   sPattern<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
  --     elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "S"
  --     (pat,_)<- sContainerWidget'' (Estuary.Tidal.Types.S Blank) never >>= splitDyn
  --     pat' <- forDyn pat (\x-> if x == (Estuary.Tidal.Types.S $ Group [] Once) then EmptyPatternChain else PatternChain $ TransformedPattern [NoTransformer] $ x)
  --     return pat'
  --   endPattern <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
  --     elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
  --     (pat,_) <- doubleContainerWidget'' (End $ Atom 0.5 Once) never >>= splitDyn
  --     pat' <- forDyn pat (\x-> if x == (End $ Group [] Once) then EmptyPatternChain else PatternChain $ TransformedPattern [NoTransformer] x)
  --     return pat'
  --   vowelPattern <- elAttr "tr" ("style"=:"background-color:wheat") $ do
  --     elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
  --     (pat,_) <- charContainerWidget'' (Vowel $ Atom '~' Once) never >>= splitDyn
  --     pat' <- forDyn pat (\x-> if x == (Vowel $ Group [] Once) then EmptyPatternChain else PatternChain $ TransformedPattern [NoTransformer] $ x )
  --     return pat'
  --   patChain' <- combineDyn (\e v-> if v == EmptyPatternChain then e else PatternChain'' e Merge v) endPattern vowelPattern
  --   patChain <- combineDyn (\chain pat-> if chain == EmptyPatternChain then pat else PatternChain'' pat Merge chain) patChain' sPattern
  --   mapDyn (\x-> (x,never)) patChain

  --
  -- genPatButtonWidget'''::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
  -- genPatButtonWidget''' (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  --   (sample,upCount) <- elAttr "tr" (empty) $ do
  --     sampName <- el "tr" $ do
  --       (sampName,_) <- el "td" $ sButtonWidget''' (Atom iSamp iReps) never >>= splitDyn
  --       elAttr "td" ("style"=:"width:30px") $ forDyn repsHold show >>= dynText
  --       return sampName
  --     upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border 1pt solid black") >>= count
  --     return $ (sampName,upButton)
  --   (deleteEvent,downCount) <- el "tr" $ do
  --     deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
  --     downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
  --     return $ (deleteButton, downButton)
  --   downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  --   repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  --   repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  --   repsHold <- holdDyn iReps $ updated repeats'
  --   sample' <- combineDyn Atom sample repsHold
  --   mapDyn (\x->(x,deleteEvent)) sample'
  --   where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
  -- genPatButtonWidget''' _ e = genPatButtonWidget''' (Atom "~" Once) e

-- turn me into a group, or tear my group apart

groupWidget:: MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m ( Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
groupWidget iPat _ = elAttr "td" ("style"=:"border: 1pt solid red") $ mdo
  let initialMap = fromList $ zip [0::Int,1] [Left Blank,Right ()]
  deleteEvent <- button' "-" (DeleteMe)
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never groupableWidget (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  forDyn values' (\x-> (Group x Once, deleteEvent))


groupableWidget::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
groupableWidget (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty) $ do
    (sampName,_) <- el "td" $ sButtonWidget''' (Atom iSamp iReps) never >>= splitDyn
    elAttr "td" ("style"=:"width:15%") $ forDyn repsHold show >>= dynText
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
    return $ (sampName,upButton)
  (layerEvent, groupEvent,deleteEvent,downCount) <- el "tr" $ do
    (layerEvent,groupEvent, deleteEvent) <- el "td" $ do
      groupButton <- tdButtonAttrs "[]" (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
      layerButton <- tdButtonAttrs "[,,]" (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
      deleteButton <- tdButtonAttrs "-" (DeleteMe) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
      return $ (layerButton, groupButton, deleteButton)
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
    return $ (layerEvent,groupEvent,deleteEvent, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repsHold <- holdDyn iReps $ updated repeats'
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  sample' <- combineDyn Atom sample repsHold
  sample'' <- combineDyn (\s tog-> if tog then Group [s] Once else s) sample' groupToggle
  sample''' <- combineDyn (\s tog-> case s of (Atom a x) -> if tog then Layers [s] Once else s; otherwise-> s) sample'' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  let e = leftmost [rebuildEvent, deleteEvent]
  mapDyn (\x->(x,e)) sample'''
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
groupableWidget _ e = groupableWidget (Atom "~" Once) e

-- when make group is called - change the value from an atom to a group (or whatever is appropriate) so the pattern matching works in 'f'

-- groupableWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
-- groupableWidget (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
--   (sample,upCount) <- elAttr "tr" (empty) $ do
--     (sampName,_) <- el "td" $ sButtonWidget''' (Atom iSamp iReps) never >>= splitDyn
--     elAttr "td" ("style"=:"width:15%") $ forDyn repsHold show >>= dynText
--     upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
--     return $ (sampName,upButton)
--   (groupEvent,deleteEvent,downCount) <- el "tr" $ do
--     (groupEvent, deleteEvent) <- el "td" $ do
--       groupButton <- tdButtonAttrs "Make Group" (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--       deleteButton <- tdButtonAttrs " - " (DeleteMe) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--       return $ (groupButton,deleteButton)
--     downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
--     return $ (groupEvent,deleteEvent, downButton)
--   isGroup <- toggle False groupEvent
--   downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
--   repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
--   repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
--   repsHold <- holdDyn iReps $ updated repeats'
--   sample' <- combineDyn Atom sample repsHold
--   let e = leftmost [groupEvent, deleteEvent]
--   mapDyn (\x->(x,e)) sample'
--   rval <- combineDyn (\isG rVal -> if isG then groupWidget Blank never else constDyn rVal) isGroup returnVal
--   rval <- forDyn (\x-> fmap (\isG-> if isG then groupWidget Blank never else constDyn x) $ current isGroup) returnVal
--   join rval
--   where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
-- groupableWidget _ e = groupableWidget (Atom "~" Once) e


-- groupableWidget'::MonadWidget t m => Event t GenericSignal -> GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
-- groupableWidget' groupEvent (Atom iSamp iReps) event = do
--   a <- toggle False groupEvent
--   b <- forDyn a (\x-> if x then groupWidget (Atom iSamp iReps) event else nongroup (Atom iSamp iReps) never) -- Dynamic t (m Dynamic t (genpat,event gen))
--   return $ joinDyn b
--   where
--     nongroup (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
--       (sample,upCount) <- elAttr "tr" (empty) $ do
--         (sampName,_) <- el "td" $ sButtonWidget''' (Atom iSamp iReps) never >>= splitDyn
--         elAttr "td" ("style"=:"width:15%") $ forDyn repsHold show >>= dynText
--         upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
--         return $ (sampName,upButton)
--       (groupEvent,deleteEvent,downCount) <- el "tr" $ do
--         (groupEvent, deleteEvent) <- el "td" $ do
--           groupButton <- tdButtonAttrs "Make Group" (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--           deleteButton <- tdButtonAttrs " - " (DeleteMe) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--           return $ (groupButton,deleteButton)
--         downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
--         return $ (groupEvent,deleteEvent, downButton)
--       -- isGroup <- toggle False groupEvent
--       downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
--       repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
--       repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
--       repsHold <- holdDyn iReps $ updated repeats'
--       sample' <- combineDyn Atom sample repsHold
--       let e = leftmost [groupEvent, deleteEvent]
--       mapDyn (\x->(x,e)) sample'
--     --  rval <- combineDyn (\isG rVal -> if isG then groupWidget Blank never else constDyn rVal) isGroup returnVal
--       -- rval <- forDyn (\x-> fmap (\isG-> if isG then groupWidget Blank never else constDyn x) $ current isGroup) returnVal
--       --join rval
--     tableAttrs = ("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
-- -- groupableWidget' _ _ e = groupableWidget' never (Atom "~" Once) e
-- iclcFixedStruct:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
-- iclcFixedStruct iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
--   s<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
--     elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "S"
--     (pat,_)<- groupableSContainer (Blank) never >>= splitDyn
--     forDyn pat (\x-> TransformedPattern [NoTransformer] $ S x)
--   end <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
--     elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
--     (pat,_) <- endContainerWidget''' (End $ Atom 0.5 Once) never >>= splitDyn
--     forDyn pat (TransformedPattern [NoTransformer])
--   vowel <- elAttr "tr" ("style"=:"background-color:wheat") $ do
--     elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
--     (pat,_) <- charContainerWidget''' (Vowel $ Atom '~' Once) never >>= splitDyn
--     forDyn pat (TransformedPattern [NoTransformer])
--   up <- elAttr "tr" ("style"=:"background-color:lightcyan") $ do
--     elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Up"
--     (pat,_) <- upContainerWidget''' (Up $ Atom 0 Once) never >>= splitDyn
--     forDyn pat (TransformedPattern [NoTransformer])
--   patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
--   patChain' <- combineDyn (\e p-> PatternChain' e Merge p) end patChain''
--   patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
--   mapDyn (\x-> (x,never)) patChain
--   where
--     toPatternChain (TransformedPattern _ (End  (Group [] _))) = EmptyPatternChain
--     toPatternChain (TransformedPattern _ (Vowel (Group [] _))) = EmptyPatternChain
--     toPatternChain x = PatternChain x
--
-- -- f ::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
-- -- f (Atom x r) e = groupableWidget (Atom x r) e
-- -- f (Blank) e = groupableWidget (Blank) e
-- -- f (Group xs r) e = sContainerWidget''' (Group xs r) e
-- -- f (Layers xs r) e = sContainerWidget''' (Layers xs r) e
--
-- groupOrAtom::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
-- groupOrAtom iVal updateEvent = mdo
--   val <- widgetHold (f (iVal) never) makeGroupEvent'' -- m dyn dyn
--   let val' = joinDyn val
--   widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
--   makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
--   makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> f (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
--   let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
--   return val'
--
-- -- S Pattern
-- groupableSContainer::(MonadWidget t m) => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName,Event t GenericSignal))
-- groupableSContainer (Atom iVal iReps) _ =  elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
--   delButton <- button' " - " DeleteMe
--   let initialMap = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
--   let cEvents = mergeWith (union) [makeSMap,deleteMap]
--   (values,events) <- eitherContainer' initialMap cEvents never never groupOrAtom (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
--   let deleteMap = fmap (fromList) deleteList
--   let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
--   let makeSMap = fmap (fromList) makeSList
--   values' <- forDyn values (elems)
--   returnVal <- forDyn values' (\x-> (Group x Once))
--   returnVal'<-forDyn returnVal (\x->(x,delButton))
--   return returnVal'
-- groupableSContainer (Group xs iReps) _ =  elAttr "td" ("style"=:"border: 1pt solid red") $ mdo
--   delButton <- button' " - " DeleteMe
--   let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--   let cEvents = mergeWith (union) [makeSMap,deleteMap]
--   (values,events) <- eitherContainer' initialMap cEvents never never widget (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
--   let deleteMap = fmap (fromList) deleteList
--   let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
--   let makeSMap = fmap (fromList) makeSList
--   values' <- forDyn values (elems)
--   returnVal <- forDyn values' (\x-> (Group x Once))
--   returnVal'<-forDyn returnVal (\x->(x,delButton))
--   return returnVal'
-- groupableSContainer (Layers xs r) e = elAttr "td" ("style"=:"border: 2pt solid blue") $ mdo
--   delButton <- button' " - " DeleteMe
--   let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--   let cEvents = mergeWith (union) [makeSMap,deleteMap]
--   (values,events) <- eitherContainer' initialMap cEvents never never widget (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
--   let deleteMap = fmap (fromList) deleteList
--   let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
--   let makeSMap = fmap (fromList) makeSList
--   values' <- forDyn values (elems)
--   returnVal <- forDyn values' (\x-> (Layers x Once))
--   returnVal'<-forDyn returnVal (\x->(x,delButton))
--   return returnVal'
-- groupableSContainer _ e = sContainerWidget''' (Atom "~" Once) e
--
-- -- End Pattern
-- endContainerWidget'''':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
-- endContainerWidget'''' a _ = mdo
--   let initialMap = (0::Int)=:(Right ())
--   let cEvents = mergeWith (union) [makeSMap,deleteMap]
--   (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
--   let deleteMap = fmap (fromList) deleteList
--   let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
--   let makeSMap = fmap (fromList) makeSList
--   values' <- forDyn values (elems)
--   returnVal <- forDyn values' (\x-> patType $ Group x Once)
--   returnVal'<-forDyn returnVal (\x->(x,never))
--   return returnVal'
--   where
--     (widgetBuilder,defaultGeneralPat, patType) = (endWidget''', Atom (1) Once, End)
--
-- endWidget''''::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
-- endWidget'''' (Atom iEnd Once) _ = elAttr "td" ("style"=:"text-align:center;margin:10px") $ mdo
--   (returnVal,attrs) <- elDynAttr "td" attrs $ do
--     (begEv,endEv,delEv) <- el "tr" $ do
--       begPlus <- tdButtonAttrs "<" (-0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
--       endPlus <- tdButtonAttrs ">" (0.05) ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black")
--       deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border: 1pt solid black"
--       return (begPlus,endPlus,deleteButton)
--     let buttons = leftmost [endEv,begEv]
--     endVal' <- foldDyn (\a b-> min 1 $ max 0 $ a+b) iEnd buttons
--     endVal <- mapDyn (\x-> (fromInteger $ round $ x*100)/100) endVal'
--     endGradient <- forDyn endVal makeStyleString
--     tableAttrs <- forDyn endGradient (\x->"style"=:("text-align:center;display:inline-table;width:100pt;border-spacing:5px;border:"++widgetBorder++";"++x))
--     val <- mapDyn (\x-> (Atom x Once,delEv)) endVal
--     return $ (val, tableAttrs)
--   return returnVal
--
-- upContainerWidget'''':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
-- upContainerWidget'''' a _ = mdo
--   let initialMap = (0::Int)=:(Right ())
--   let cEvents = mergeWith (union) [makeSMap,deleteMap]
--   (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
--   let deleteMap = fmap (fromList) deleteList
--   let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
--   let makeSMap = fmap (fromList) makeSList
--   values' <- forDyn values (elems)
--   returnVal <- forDyn values' (\x-> patType $ Group x Once)
--   returnVal'<-forDyn returnVal (\x->(x,never))
--   return returnVal'
--   where
--     (widgetBuilder,defaultGeneralPat, patType) = (upWidget''', Atom 0 Once, Up)
--
-- upWidget''''::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
-- upWidget'''' (Atom iUpVal _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
--   upCount <- elAttr "tr" (empty)$ do
--     elAttr "td" ("style"=:"text-align:center")$ dynText upValShow
--     upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border 1pt solid black") >>= count
--     return upButton
--   (deleteEvent,downCount) <- el "tr" $ do
--     deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
--     downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
--     return $ (deleteButton, downButton)
--   upVal <- combineDyn (\a b ->  (a)-(b)+(round iUpVal)::Int ) upCount downCount
--   upValShow <- forDyn upVal show
--   --repsHold <- holdDyn iUpVal $ updated repeats
--   mapDyn (\x->(Atom (fromIntegral x) Once,deleteEvent)) upVal
--   where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
-- upWidget'''' _ e = upWidget''' (Atom 0 Once) e
--
-- -- Vowel Pattern
-- charContainerWidget'''':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
-- charContainerWidget'''' a _ = mdo
--   let initialMap = (0::Int)=:(Right ())
--   let cEvents = mergeWith (union) [makeSMap,deleteMap]
--   (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButton'"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
--   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
--   let deleteMap = fmap (fromList) deleteList
--   let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
--   let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
--   let makeSMap = fmap (fromList) makeSList
--   values' <- forDyn values (elems)
--   returnVal <- forDyn values' (\x-> (patType $ Group x Once))
--   returnVal'<-forDyn returnVal (\x->(x,never))
--   return returnVal'
--   where
--     tableAttrs=("style"=:"text-align:center;display:inline-table;border-spacing:5px;")
--     (widgetBuilder,defaultGeneralPat, patType) = (vowelButtonWidget''', Atom '~' Once, Vowel)
--
-- vowelButtonWidget''''::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
-- vowelButtonWidget'''' (Atom iVowel _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:("width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:"++widgetBorder)) $ mdo
--   let sampleMap = fromList $ zip [0::Int,1..] ['~','a','e','i','o','u']  -- Map Int (String,String)
--   let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems sampleMap
--   vowelButton <- tdButtonAttrs' (showVowel) (iVowel) $ "style"=:"width:50px;text-align:center;background-color:lightblue;border:1pt solid black"
--   deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 50px; text-align:center;background-color:lightblue;border:1pt solid black"
--   num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
--   char'' <- mapDyn (\x-> maybe ('~') id $ Data.Map.lookup x sampleMap) num
--   let char' = updated char''
--   char <- holdDyn (iVowel) char'
--   vowel <- mapDyn (\x -> Atom x Once) char
--   showVowel <- mapDyn show vowel
--   vowel' <- mapDyn (\x -> if x =='~' then Atom 't' Once else Atom x Once) char
--   mapDyn (\x->(x,deleteButton)) vowel'
--
--
-- groupableSWidget::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
-- groupableSWidget (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
--   (sample,upCount) <- elAttr "tr" (empty) $ do
--     sampleName <- textInput $ def & textInputConfig_initialValue .~ (iSamp) >>= _textInput_value
--     let sampleName = _textInput_value sampleName'
--     elAttr "td" ("style"=:"width:15%") $ forDyn repsHold show >>= dynText
--     upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
--     return $ (sampleName,upButton)
--   (layerEvent, groupEvent,deleteEvent,downCount) <- el "tr" $ do
--     (layerEvent,groupEvent, deleteEvent) <- el "td" $ do
--       groupButton <- tdButtonAttrs "[]" (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--       layerButton <- tdButtonAttrs "[,,]" (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--       deleteButton <- tdButtonAttrs "-" (DeleteMe) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
--       return $ (layerButton, groupButton, deleteButton)
--     downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
--     return $ (layerEvent,groupEvent,deleteEvent, downButton)
--   downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
--   repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
--   repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
--   repsHold <- holdDyn iReps $ updated repeats'
--   groupToggle <- toggle False groupEvent
--   layerToggle <- toggle False layerEvent
--   sample' <- combineDyn Atom sample repsHold
--   sample'' <- combineDyn (\s tog-> if tog then Group [s] Once else s) sample' groupToggle
--   sample''' <- combineDyn (\s tog-> case s of (Atom a x) -> if tog then Layers [s] Once else s; otherwise-> s) sample'' layerToggle
--   let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
--   let e = leftmost [rebuildEvent, deleteEvent]
--   mapDyn (\x->(x,e)) sample'''
--   where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
-- groupableSWidget _ e = groupableWidget (Atom "~" Once) e
