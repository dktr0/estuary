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
import qualified Estuary.Widgets.IclcWidgets as I
import qualified Estuary.Widgets.GeneralPattern as Gen


icoahWidget:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
icoahWidget iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
  s<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "S"
    (pat,_)<- Gen.sampleNameContainer (Blank) never >>= splitDyn
    forDyn pat (\x-> TransformedPattern [NoTransformer] $ S x)
  end <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
    (pat,_) <- I.specificDoubleContainer (End $ Atom 0.5 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "tr" ("style"=:"background-color:wheat") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
    (pat,_) <- charContainerWidget (Vowel $ Atom '~' Once) never >>= splitDyn
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

-- S Pattern
sContainerWidget ::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never Gen.sampleNameContainer (tdPingButtonAttrs  "+" ("style"=:"text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:50%;vertical-align:middle"))-- values:dyn Map k GeneralPattern,
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

genPatButtonWidget'''::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t GenericSignal))
genPatButtonWidget''' (Atom iSamp iReps) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  (sample,upCount) <- elAttr "tr" (empty) $ do
    let sampleMap = fromList $ zip [0::Int,1..] $ concat [[("Rest","~")],fmap (\x->("Bass",x)) ["bd", "bassfoo", "less"], fmap (\x->("Glitch",x)) ["glitch", "click", "glitch2", "casio", "hardcore", "house", "cosmic"], fmap (\x->("Crashes",x)) ["cc"], fmap (\x->("Long Sounds",x)) ["sheffield", "ade", "padlong", "tacscan"], fmap (\x->("Humans and Animals",x)) ["h", "baa", "crow", "numbers", "alphabet", "hmm"]]
    sampName <- el "td" $ sDropDownOptsWidget iSamp sampleMap
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


-- Creates a dropdown menu with headings
-- sampleMap values are tuples. The first is the 'subheading' that the second is placed under
sDropDownOptsWidget::MonadWidget t m => String -> Map Int (String,String) -> m (Dynamic t String)
sDropDownOptsWidget iSamp sampleMap = mdo
  let initialIndex = maybe 0 id $ Data.List.findIndex (==iSamp) $ fmap snd $ elems sampleMap
  dd <- dropdownOpts initialIndex sampleMap def
  let val = _dropdown_value dd
  forDyn val (\x -> maybe "~" snd $ Data.Map.lookup x sampleMap)


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
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
charContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:"++plusHeight++";vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
    (widgetBuilder,defaultGeneralPat, patType) = (Gen.vowelButtonWidget, Atom 'X' Once, Vowel)


------------------------
--     Universal      --
------------------------

-- Height of plus td plus buttons
plusHeight = "30px"
widgetBorder = "2pt solid black"

makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100
