{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.GeneralPattern
where

import Reflex
import Reflex.Dom
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)






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

-- countStepWidget (see ICOAH 'up' Widget for example)
-- step is the amount each click of the up/down arrows modify the value by
--   ---------------
--   --  0.0   ▲  --
--   --   -    ▼  --
--   ---------------
countStepWidget::MonadWidget t m => Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
countStepWidget step (Atom iUpVal _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  upCount <- elAttr "tr" (empty)$ do
    elAttr "td" ("style"=:"text-align:center")$ dynText upValShow
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border 1pt solid black") >>= count
    return upButton
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  upVal <- combineDyn (\a b ->  (a*step)-(b*step)+(iUpVal)) upCount downCount
  upValShow <- forDyn upVal show
  --repsHold <- holdDyn iUpVal $ updated repeats
  mapDyn (\x->(Atom x Once,deleteEvent)) upVal
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:2pt solid black"))
countStepWidget step _ e = countStepWidget step (Atom 0 Once) e


charWidget::MonadWidget t m => GeneralPattern Char-> Event t () -> m (Dynamic t (GeneralPattern Char,Event t GenericSignal))
charWidget (Atom iVal reps) _ = do
  textField <-textInput $ def & textInputConfig_attributes .~ (constDyn $ fromList $ zip ["style", " maxlength"] ["width:40px", "1"]) & textInputConfig_initialValue .~ [iVal]
  let inputVal = _textInput_value textField
  inputChar <- mapDyn (\c-> if length c ==1 then c!!0 else  '~') inputVal
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\char rep-> if char=='~' then Blank else Atom char rep) inputChar repeats'
  forDyn genPat (\k-> (k,deleteButton))


crushWidget::MonadWidget t m => GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
crushWidget iVal _ = do
  text "0"
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range","0","16","1","width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text "16"
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Int) then Atom (read x::Int) Once else Atom 16 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))

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


-- < and > buttons, background color fill illustrateds value
-- see iclc fixed, end widget
--  -----------------
--  --  <   >   -  --
--  -----------------
faderButtonWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
faderButtonWidget (Atom iEnd Once) _ = elAttr "td" ("style"=:"text-align:center;margin:10px") $ mdo
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
    tableAttrs <- forDyn endGradient (\x->"style"=:("text-align:center;display:inline-table;width:100pt;border-spacing:5px;border:2pt solid black;"++x))
    val <- mapDyn (\x-> (Atom x Once,delEv)) endVal
    return $ (val, tableAttrs)
  return returnVal


--  Can interchange for groupableStringWidget (which is used in the ICLC widgets for S patterns)
--  -----------------
--  -  [____] *2  ▲ -
--  -  [] [,]  -  ▼ -
--  -----------------
groupableSWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
groupableSWidget (Atom iSamp iReps) _ = elAttr "div" ("style"=:"text-align:center;display:inline-block") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty) $ do
    sampleName <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:50px;") & textInputConfig_initialValue .~ (iSamp)
    let sampleName' = _textInput_value sampleName
    elAttr "td" ("style"=:"width:15%") $ forDyn repsHold show >>= dynText
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
    return $ (_textInput_value sampleName,upButton)
  (layerEvent, groupEvent,deleteEvent,downCount) <- el "tr" $ do
    (layerEvent,groupEvent) <- el "td" $ do
      groupButton <- tdButtonAttrs " [] " (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
      layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
      return $ (layerButton, groupButton)
    deleteEvent <- tdButtonAttrs " - " (DeleteMe) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
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
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) sample'''
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:2pt solid black"))
groupableSWidget _ e = groupableSWidget (Atom "~" Once) e


intWidget::MonadWidget t m => GeneralPattern Int-> Event t () -> m (Dynamic t (GeneralPattern Int,Event t GenericSignal))
intWidget iVal _ = do
  let attrs = def & textInputConfig_attributes .~ constDyn ("style"=:"width:20px;") & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
  textField <-textInput attrs
  let inputVal = _textInput_value textField
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\str rep-> if isJust (readMaybe str::Maybe Int) then Atom (read str::Int) rep else Atom 0 rep ) inputVal repeats'
  forDyn genPat (\k-> (k,deleteButton))

-- uses clickListWidget as a base widget, intersperses with + buttons
sampleNameWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
sampleNameWidget (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
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
sampleNameWidget _ e = sampleNameWidget (Atom "~" Once) e



----------------------
--  GenPat Double   --
----------------------
-- The following 3 widget functions compose of a container which interpserses aGLDoubleWidgets
-- (something that returns an Atom, Group or Layer (aGL)) with + buttons. The widgets can be 
-- recursively constructed: each individual widget can be turned into a container itself (as a group or layer).

doubleContainer::(MonadWidget t m) => Double -> Double -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
doubleContainer vMin vMax step (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (doubleAGL vMin vMax step) (tdPingButtonAttrs "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Layers x Once)
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
doubleContainer vMin vMax step (Group xs iReps) _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (doubleAGL vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  forDyn returnVal (\x->(x,delButton))
doubleContainer vMin vMax step (Atom iVal iReps) _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (doubleAGL vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal::Double) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  forDyn returnVal (\x->(x,delButton))
doubleContainer vMin vMax step _ e = doubleContainer vMin vMax step (Atom (vMin+(vMax-vMin)/2) Once) e
  where
    makeIMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    makeIMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    makeIMap _ = makeIMap (Atom 0 Once)

aGLDoubleWidget::(MonadWidget t m) => Double -> Double -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
aGLDoubleWidget vMin vMax step (Atom iVal _) _ = elAttr "div" ("style"=:"text-align:center;display:inline-block") $ elAttr "table" tableAttrs $ mdo
  (genPat,deleteEvent) <- elAttr "tr" (empty) $ do
    genPat <- elAttr "td" ("style"=:"width:45px") $ do
      let attrs = fromList $ zip ["style","step","min","max"] ["width:40px;",show step, show vMin, show vMax]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      inVal' <- mapDyn (\str-> if isJust (readMaybe str::Maybe Double) then (read str::Double) else (vMax-vMin)/2+vMin) inVal
      mapDyn (\x->Atom (max vMin $ min vMax x) Once) inVal'
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "style"=:"width:45px;text-align:center; background-color:lightblue;border: 1pt solid black"
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) genPat''
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:2pt solid black"))
aGLDoubleWidget vMin vMax step _ e = aGLDoubleWidget vMin vMax step (Atom 0 Once) e

doubleAGL::(MonadWidget t m) => Double -> Double -> Double-> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
doubleAGL vMin vMax step iVal updateEvent = mdo
  val <- widgetHold (function vMin vMax step (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> function vMin vMax step (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    function vMin vMax step (Atom x r) e = aGLDoubleWidget vMin (vMax) step (Atom x r) e
    function vMin vMax step (Blank) e = aGLDoubleWidget vMin vMax step (Blank) e
    function vMin vMax step (Group xs r) e = doubleContainer vMin vMax step (Group xs r) e
    function vMin vMax step (Layers xs r) e = doubleContainer vMin vMax step (Layers xs r) e




----------------------
--  GenPat Ints     --
----------------------
-- The following 3 widget functions compose of a container which interpserses aGLStringWidgets
-- (something that returns an Atom, Group or Layer (aGL)) with + buttons. The widgets can be 
-- recursively constructed: each individual widget can be turned into a container itself (as a group or layer).

intContainer::(MonadWidget t m) => Int -> Int -> Int -> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
intContainer vMin vMax step (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (intAGL vMin vMax step) (tdPingButtonAttrs "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (0::Int) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Layers x Once)
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
intContainer vMin vMax step (Group xs iReps) _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (intAGL vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (0::Int) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  forDyn returnVal (\x->(x,delButton))
intContainer vMin vMax step (Atom iVal iReps) _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (intAGL vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (0::Int) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  forDyn returnVal (\x->(x,delButton))
intContainer vMin vMax step _ e = intContainer vMin vMax step (Atom 0 Once) e
  where
    makeIMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    makeIMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    makeIMap _ = makeIMap (Atom 0 Once)

aGLIntWidget::(MonadWidget t m) => Int -> Int -> Int -> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
aGLIntWidget vMin vMax step (Atom iVal _) _ = elAttr "div" ("style"=:"text-align:center;display:inline-block") $ elAttr "table" tableAttrs $ mdo
  (genPat,deleteEvent) <- elAttr "tr" (empty) $ do
    genPat <- elAttr "td" ("style"=:"width:45px") $ do
      let attrs = fromList $ zip ["style","step","min","max"] ["width:40px;",show step, show vMin, show vMax]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      inVal' <- mapDyn (\str-> if isJust (readMaybe str::Maybe Int) then (read str::Int) else 0) inVal
      mapDyn (\x->Atom (max vMin $ min vMax x) Once) inVal'
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "style"=:"width:45px;text-align:center; background-color:lightblue;border: 1pt solid black"
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) genPat''
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:2pt solid black"))
aGLIntWidget vMin vMax step _ e = aGLIntWidget vMin vMax step (Atom 0 Once) e

intAGL::(MonadWidget t m) => Int -> Int -> Int-> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
intAGL vMin vMax step iVal updateEvent = mdo
  val <- widgetHold (function vMin vMax step (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> function vMin vMax step (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    function vMin vMax step (Atom x r) e = aGLIntWidget vMin (vMax) step (Atom x r) e
    function vMin vMax step (Blank) e = aGLIntWidget vMin vMax step (Blank) e
    function vMin vMax step (Group xs r) e = intContainer vMin vMax step (Group xs r) e
    function vMin vMax step (Layers xs r) e = intContainer vMin vMax step (Layers xs r) e


----------------------
--  GenPat Strings  --
----------------------
-- The following 3 widget functions compose of a container which interpserses aGLStringWidgets
-- (something that returns an Atom, Group or Layer (aGL)) with + buttons. The widgets can be 
-- recursively constructed: each individual widget can be turned into a container itself (as a group or layer).

-- String container
stringContainer::(MonadWidget t m) => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
stringContainer (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (stringAGL) (tdPingButtonAttrs "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom ("~"::String) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Layers x Once)
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
stringContainer (Group xs iReps) _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (stringAGL) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom ("~"::String) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  forDyn returnVal (\x->(x,delButton))
stringContainer (Atom iVal iReps) _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (stringAGL) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom ("~"::String) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  forDyn returnVal (\x->(x,delButton))
stringContainer _ e = stringContainer (Atom ("~") Once) e
  where
    makeIMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    makeIMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    makeIMap _ = makeIMap (Atom 0 Once)

-- Individual string widgets (able to turn into a container themselves by signaling their container in the returned event)
aGLStringWidget::(MonadWidget t m) => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
aGLStringWidget (Atom iVal iReps) _ = elAttr "div" ("style"=:"display:inline-block;") $ elAttr "table" tableAttrs $ mdo
  genPat <- el "tr" $ do
    val <- el "td" $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:60px") & textInputConfig_initialValue .~ (iVal)
      let val = _textInput_value inputField
      return val
    reps <- repDivWidget iReps
    combineDyn (\x y -> Atom x y) val reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "style"=:"width:45px;text-align:center; background-color:lightblue;border: 1pt solid black"
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x-> (x,leftmost [rebuildEvent, deleteEvent])) genPat''
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:130pt;border-spacing:5px;border:2pt solid black"))
aGLStringWidget _ e = aGLStringWidget (Atom "~" Once) e

-- Initially represents an Atom - string widget, catches signals to be turned into a Group or Layer
stringAGL::(MonadWidget t m) =>  GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
stringAGL iVal updateEvent = mdo
  val <- widgetHold (builder (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> builder (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    builder (Atom x r) e = aGLStringWidget (Atom x r) e
    builder (Blank) e = aGLStringWidget (Blank) e
    builder (Group xs r) e = stringContainer (Group xs r) e
    builder (Layers xs r) e = stringContainer (Layers xs r) e


repDivWidget::MonadWidget t m => RepOrDiv -> m (Dynamic t RepOrDiv)
repDivWidget (Rep iVal) = elAttr "table" ("style"=:"height:20px")$ mdo
  val <- el "tr" $ do
    repOrDiv <- el "td" $ mdo
      text "*"
      rep <- checkbox True $ def & checkboxConfig_setValue .~ (fmap not divideEv)
      let repEv = _checkbox_change rep
      el "br" blank
      text "/"
      divide <- checkbox False $ def & checkboxConfig_setValue .~ (fmap not repEv)
      let divideEv = _checkbox_change divide
      let val = _checkbox_value rep
      return val
    num <- el "td" $ do
      let attrs = fromList $ zip ["style","step","min"] ["width:35px;","1", "0"]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      forDyn inVal (\x-> maybe 1 id $ ((readMaybe x)::Maybe Int))
    combineDyn (\rd val -> if rd then Rep val else Div val) repOrDiv num
  return val
repDivWidget (Div iVal) = elAttr "table" ("style"=:"height:20px")$ mdo
  val <- el "tr" $ do
    repOrDiv <- el "td" $ mdo
      text "*"
      rep <- checkbox False $ def & checkboxConfig_setValue .~ (fmap not divideEv)
      let repEv = _checkbox_change rep
      el "br" blank
      text "/"
      divide <- checkbox True $ def & checkboxConfig_setValue .~ (fmap not repEv)
      let divideEv = _checkbox_change divide
      let val = _checkbox_value rep
      return val
    num <- el "td" $ do
      let attrs = fromList $ zip ["style","step","min"] ["width:35px;","1", "0"]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      forDyn inVal (\x-> maybe 1 id $ ((readMaybe x)::Maybe Int))
    combineDyn (\rd val -> if rd then Rep val else Div val) repOrDiv num
  return val
repDivWidget _ = repDivWidget (Rep 1)



-- Eldad's Widgets:

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
  where tableAttrs=("style"=:"margin:5px;display:inline-table;background-color:lightgreen;width:10%;padding:6px;border-spacing:5px;border: 3pt solid black")
genPatButtonWidget _ e = genPatButtonWidget (Atom "~" Once) e


sampleTextWidget::MonadWidget t m => GeneralPattern Sample-> Event t () -> m (Dynamic t (GeneralPattern Sample,Event t GenericSignal))
sampleTextWidget (Atom (Sample s) r) _ = do
  let (iName,iN) = s
  text "S"
  sampleField <-textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:50px;") & textInputConfig_initialValue .~ (iName)
  let sampleName = _textInput_value sampleField
  text "N"
  nField <-textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:20px;") & textInputConfig_initialValue .~ (show iN) & textInputConfig_inputType .~"number"
  let nVal' = _textInput_value nField
  nVal <- forDyn nVal' (\x-> if isJust (readMaybe x::Maybe Int) then read x::Int else 0)
  sample <- combineDyn (\a b -> Sample (a,b)) sampleName nVal
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\samp rep-> Atom samp rep) sample repeats'
  forDyn genPat (\k-> (k,deleteButton))

sButtonWidget::MonadWidget t m =>  GeneralPattern SampleName -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
sButtonWidget (Atom iSamp iReps) updatedReps = mdo
  let sampleMap = fromList $ zip [(0::Int)..] ["~","bd","sn","cp","hh"]  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iSamp) $ elems sampleMap
  sampleButton <- tdButtonAttrs' (showSample) (iSamp) $ "style"=:"width:60%;text-align:center;background-color:lightblue"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length sampleMap)
  str'' <- mapDyn (\x-> maybe ("~") id $ Data.Map.lookup x sampleMap) num
  let str' = updated str''
  str <- holdDyn (iSamp) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x r) str reps
  showSample <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample

--Slider w/ a range and stepsize
sliderWidget::MonadWidget t m => (Double,Double)-> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
sliderWidget (minVal,maxVal) stepsize iVal _ = do
  text $ show minVal
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range",show minVal,show maxVal,show stepsize,"width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text $ show maxVal
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Double) then Atom (read x::Double) Once else Atom 0.5 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))

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


vowelButtonWidget'::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget' (Atom iVowel _) _ = elAttr "table" ("style"=:"border-spacing:5px;display:inline-table;background-color:lightgreen;border:3pt solid black") $ mdo
  let sampleMap = fromList $ zip [0::Int,1,2,3,4] ['~','a','e','i','o','u']  -- Map Int (String,String)
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

-- For faderButtonWidget - gradient string used to show the 
makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100