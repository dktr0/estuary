{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.IclcWidgets
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


-- Height of plus td plus buttons
plusHeight = "30px"
widgetBorder = "2pt solid black"

makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100

------------------------------
--        Fixed Struct      --
------------------------------

iclcFixedStruct:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
iclcFixedStruct iChain _ = elAttr "div" (empty) $ do
  s<- elAttr "div" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "Sound"
    (pat,_)<- soundContainer (Blank) never >>= splitDyn
    forDyn pat (\x-> TransformedPattern [NoTransformer] $ S x)
  end <- elAttr "div" ("style"=:"background-color:Lightyellow") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "End"
    (pat,_) <- endContainerWidget (End $ Atom 0.5 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "div" ("style"=:"background-color:wheat") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "Vowel"
    (pat,_) <- charContainerWidget (Vowel $ Atom '~' Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  up <- elAttr "tr" ("style"=:"background-color:lightcyan;display:inline-block") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "Up"
    -- (pat,_) <- doubleGroupOrAtom (-244) (244) 1 (Atom 0 Once) never >>= splitDyn
    (pat,_) <- specificDoubleContainer (Up $ Atom 0 Once) never >>= splitDyn
    --(pat, _) <- groupableDoubleWidget (1) (4) 1 (Atom 3 Once) never >>= splitDyn
    forDyn pat (\x -> TransformedPattern [NoTransformer] x)
  patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
  patChain' <- combineDyn (\e p-> PatternChain' e Merge p) end patChain''
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
  mapDyn (\x-> (x,never)) patChain
  where
    toPatternChain (TransformedPattern _ (End  (Group [] _))) = EmptyPatternChain
    toPatternChain (TransformedPattern _ (Vowel (Group [] _))) = EmptyPatternChain
    toPatternChain x = PatternChain x



-- f ::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))

sContainer (S iVal) e = do
  a<-soundContainer iVal e
  forDyn a (\(x,y)->(S x, y))

-- S Pattern
soundContainer::(MonadWidget t m) => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName,Event t GenericSignal))
soundContainer (Atom iVal iReps) _ =  elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never never sGroupOrAtom (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- (clickableDivAttrs' "+" (Ping) ("style"=:("text-align:center;display:inline-block;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
soundContainer (Group xs iReps) _ =  elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never never sGroupOrAtom (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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
soundContainer (Layers xs r) e = elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never never sGroupOrAtom (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
soundContainer _ e = soundContainer (Atom "~" Once) e

sGroupOrAtom::MonadWidget t m => GeneralPattern SampleName -> Event t GenericSignal -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
sGroupOrAtom iVal updateEvent = mdo
  val <- widgetHold (f (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> f (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    f (Atom x r) e = groupableSWidget (Atom x r) e
    f (Blank) e = groupableSWidget (Blank) e
    f (Group xs r) e = soundContainer (Group xs r) e
    f (Layers xs r) e = soundContainer (Layers xs r) e

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
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
groupableSWidget _ e = groupableSWidget (Atom "~" Once) e

-- Up widget
upGroupOrAtom::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
upGroupOrAtom iVal updateEvent = mdo
  val <- widgetHold (f (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> f (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    f (Atom x r) e = groupableUpWidget (Atom x r) e
    f (Blank) e = groupableUpWidget (Blank) e
    f (Group xs r) e = upContainer (Group xs r) e
    f (Layers xs r) e = upContainer (Layers xs r) e

upContainer:: MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
upContainer (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never upGroupOrAtom (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (0::Double) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Layers x Once)
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
upContainer iVal _ = elAttr "div" ("style"=:"border: 1pt solid red;display:inline-block;") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = makeIMap iVal
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never upGroupOrAtom (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (0::Double) Once))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> Group x Once)
  returnVal'<-forDyn returnVal (\x->(x,delButton))
  return returnVal'
  where
    makeIMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    makeIMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    makeIMap _ = makeIMap (Atom 0 Once)


groupableUpWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
groupableUpWidget (Atom iUpVal _) _ = elAttr "div" ("style"=:"text-align:center;display:inline-block") $ elAttr "table" tableAttrs $ mdo
  (upVal,deleteEvent) <- elAttr "tr" (empty) $ do
    genPat <- elAttr "td" ("style"=:"width:45px") $ do
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:40px;") & textInputConfig_initialValue .~ (show iUpVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      mapDyn (\str-> if isJust (readMaybe str::Maybe Double) then Atom (read str::Double) Once else Atom 0 Once) inVal
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "style"=:"width:45px;text-align:center; background-color:lightblue;border: 1pt solid black"
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ fromList $ zip ["style"] ["text-align:center; background-color:lightblue;border: 1pt solid black"]
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  --upVal' <- mapDyn (\x-> Atom (fromIntegral x) Once) upVal
  upVal'' <- combineDyn (\u tog-> if tog then Group [u] Once else u) upVal groupToggle
  upVal'''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) upVal'' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) upVal'''
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
groupableUpWidget _ e = groupableUpWidget (Atom 0 Once) e


-- Vowel Pattern
charContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget a _ = mdo
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
    (widgetBuilder,defaultGeneralPat, patType) = (vowelButtonWidget, Atom '~' Once, Vowel)

vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget (Atom iVowel _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:("width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:"++widgetBorder)) $ mdo
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


-- End Pattern
endContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
endContainerWidget a _ = mdo
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
    (widgetBuilder,defaultGeneralPat, patType) = (endWidget, Atom (1) Once, End)

endWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
endWidget (Atom iEnd Once) _ = elAttr "td" ("style"=:"text-align:center;margin:10px") $ mdo
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



































specificDoubleContainer::MonadWidget t m=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
specificDoubleContainer iVal e = do
  genPat <- doubleContainer vMin vMax step iGroup e -- m (dyn gen eve)
  mapDyn (\(x,ev)->(constructor x, ev)) genPat
  where
    (constructor, iGroup,vMin,vMax,step) = case iVal of
      Accelerate x -> (Accelerate, x, -500,500,1)
      Bandq x -> (Bandq, x, 0, 22000, 10)
      Begin x -> (Begin, x, 0, 1, 0.05)
      Delay x -> (Delay, x, 0, 1, 0.05)
      Delayfeedback x -> (Delayfeedback, x, 0, 1, 0.05)
      Delaytime x -> (Delaytime, x, 0, 1, 0.05)
      End x -> (End,x, 0, 1, 0.05)
      Gain x -> (Gain, x,  0, 1, 0.05)
      Hresonance x -> (Hresonance, x, 0, 1, 0.05)
      Pan x -> (Pan, x, 0, 1, 0.05)
      Resonance x -> (Resonance, x, 0, 1, 0.05)
      Shape x -> (Shape, x, 0, 1, 0.05)
      Speed x -> (Speed, x, -999, 999, 0.5)
      Up x -> (Up, x, -132,132, 1)


doubleContainer::(MonadWidget t m) => Double -> Double -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
doubleContainer vMin vMax step (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (doubleGroupOrAtom vMin vMax step) (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never (doubleGroupOrAtom vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never (doubleGroupOrAtom vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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

groupableDoubleWidget::(MonadWidget t m) => Double -> Double -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
groupableDoubleWidget vMin vMax step (Atom iVal _) _ = elAttr "div" ("style"=:"text-align:center;display:inline-block") $ elAttr "table" tableAttrs $ mdo
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
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
groupablDoubleWidget vMin vMax step _ e = groupablDoubleWidget vMin vMax step (Atom 0 Once) e

doubleGroupOrAtom::(MonadWidget t m) => Double -> Double -> Double-> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
doubleGroupOrAtom vMin vMax step iVal updateEvent = mdo
  val <- widgetHold (function vMin vMax step (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> function vMin vMax step (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    function vMin vMax step (Atom x r) e = groupableDoubleWidget vMin (vMax) step (Atom x r) e
    function vMin vMax step (Blank) e = groupableDoubleWidget vMin vMax step (Blank) e
    function vMin vMax step (Group xs r) e = doubleContainer vMin vMax step (Group xs r) e
    function vMin vMax step (Layers xs r) e = doubleContainer vMin vMax step (Layers xs r) e


------------------------------------
--            Strings....         --
------------------------------------



specificStringContainer::MonadWidget t m=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
specificStringContainer iVal e = do
  genPat <- stringContainer iGroup e -- m (dyn gen eve)
  mapDyn (\(x,ev)->(constructor x, ev)) genPat
  where
    (constructor, iGroup) = case iVal of
      (S _) -> (S , Atom "~" Once)


stringContainer::(MonadWidget t m) => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
stringContainer (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (stringGroupOrAtom) (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never (stringGroupOrAtom) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never (stringGroupOrAtom) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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

groupableStringWidget::(MonadWidget t m) => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
groupableStringWidget (Atom iVal iReps) _ = elAttr "div" ("style"=:"display:inline-block;") $ elAttr "table" tableAttrs $ mdo
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
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:130pt;border-spacing:5px;border:"++widgetBorder))

groupableStringWidget _ e = groupableStringWidget (Atom "~" Once) e

stringGroupOrAtom::(MonadWidget t m) =>  GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
stringGroupOrAtom iVal updateEvent = mdo
  val <- widgetHold (builder (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> builder (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    builder (Atom x r) e = groupableStringWidget (Atom x r) e
    builder (Blank) e = groupableStringWidget (Blank) e
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










----------------Ints.....


specificIntContainer::MonadWidget t m=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
specificIntContainer iVal e = do
  genPat <- intContainer vMin vMax step iGroup e -- m (dyn gen eve)
  mapDyn (\(x,ev)->(constructor x, ev)) genPat
  where
    (constructor, iGroup,vMin,vMax,step) = case iVal of
      Bandf x -> (Bandf, x, 0, 25000, 10)
      Coarse x -> (Coarse, x, 0, 24, 1)
      Crush x -> (Crush, x, 0, 16, 1)
      Estuary.Tidal.Types.Cut x -> (Estuary.Tidal.Types.Cut, x, -50, 50, 1)
      Cutoff x -> (Cutoff, x, 0, 25000, 10)
      Hcutoff x -> (Cutoff, x, 0, 25000, 10)
      Loop x -> (Loop, x, 0, 1024,1)
      N x -> (N, x, 0, 50, 1)


intContainer::(MonadWidget t m) => Int -> Int -> Int -> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
intContainer vMin vMax step (Layers xs iReps) _ =  elAttr "div" ("style"=:"border: 2pt solid blue;display:inline-block") $ mdo
  delButton <- button' " - " DeleteMe
  let initialMap = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never (intGroupOrAtom vMin vMax step) (tdPingButton' "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never (intGroupOrAtom vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never (intGroupOrAtom vMin vMax step) (pingButton''' "+" ("style"=:"background-color:lightblue;display:inline-block"))-- values:dyn Map k GeneralPattern,
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

groupableIntWidget::(MonadWidget t m) => Int -> Int -> Int -> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
groupableIntWidget vMin vMax step (Atom iVal _) _ = elAttr "div" ("style"=:"text-align:center;display:inline-block") $ elAttr "table" tableAttrs $ mdo
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
  where tableAttrs=("style"=:("display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border:"++widgetBorder))
groupablIntWidget vMin vMax step _ e = groupablIntWidget vMin vMax step (Atom 0 Once) e

intGroupOrAtom::(MonadWidget t m) => Int -> Int -> Int-> GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t GenericSignal))
intGroupOrAtom vMin vMax step iVal updateEvent = mdo
  val <- widgetHold (function vMin vMax step (iVal) never) makeGroupEvent'' -- m dyn dyn
  let val' = joinDyn val
  widgetEvents <- forDyn val' (\(x,y)->y) -- Dyn Ev.
  makeGroupEvent <- forDyn widgetEvents (\x->ffilter (==RebuildMe) x) -- Dyn Event t RebuildMe
  makeGroupEvent' <- combineDyn (\v e-> fmap (\x-> function vMin vMax step (fst v) never) e) val' makeGroupEvent -- dyn Event m(...)
  let makeGroupEvent'' = switchPromptlyDyn makeGroupEvent' -- Event m(...)
  return val'
  where
    function vMin vMax step (Atom x r) e = groupableIntWidget vMin (vMax) step (Atom x r) e
    function vMin vMax step (Blank) e = groupableIntWidget vMin vMax step (Blank) e
    function vMin vMax step (Group xs r) e = intContainer vMin vMax step (Group xs r) e
    function vMin vMax step (Layers xs r) e = intContainer vMin vMax step (Layers xs r) e
