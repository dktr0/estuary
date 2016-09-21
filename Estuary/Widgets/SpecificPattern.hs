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
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)



panSampleWidget::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (PatternChain,Event t GenericSignal))
panSampleWidget i e = do
  samplePattern <- sampleContainerWidget i e >>= mapDyn (\(a,b)->a)
  panPattern <- panContainerWidget i e >>= mapDyn (\(a,b)->a)-- Pan (GeneralPattern Double)
  crushPattern <- crushContainerWidget i e >>= mapDyn (\(a,b)->a)
  transSample<- forDyn samplePattern (TransformedPattern [NoTransformer])
  transPan <- forDyn panPattern (TransformedPattern [NoTransformer])
  transCrush <- forDyn crushPattern (TransformedPattern [NoTransformer])
  patternChain <-combineDyn (\pan crush-> (PatternChain' pan Add (PatternChain crush))) transPan transCrush -- Dynamic t PatternChain
  patternChain' <- combineDyn (\sample chain-> PatternChain' sample Add chain) transSample patternChain
  forDyn patternChain' (show) >>=display
  forDyn patternChain' (\k -> (k,never))

stringContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
stringContainerWidget a _ = mdo
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
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      S _ -> (textWidget, Blank, S)

charContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget a _ = mdo
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
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (charWidget, Atom 'c' Once, Unit)
      Vowel _ -> (vowelButtonWidget, Atom '~' Once, Vowel)


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
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Accelerate _ -> (sliderWidget (-500,500) 0.01 , Atom (1) Once, Accelerate) --making an assumption about accelerate values being [-500,500]
      Bandq _ -> (sliderWidget (0,400) 1, Atom (100) Once, Bandq) -- !!@@making an uneducated assupmtion about Bandq values....
      Begin _ -> (sliderWidget (0,1) 0.01, Atom (0) Once, Begin)
      Delay _ -> (sliderWidget (0,1) 0.01, Atom (0) Once, Delay)
      Delayfeedback _ -> (sliderWidget (0,1) 0.01, Atom (0) Once, Delayfeedback)
      Delaytime _ -> (sliderWidget (0,1) 0.01, Atom (0.5) Once, Delaytime)
      End _ -> (endWidget, Atom (1) Once, End)
      Gain _ -> (sliderWidget (0,4) 0.01, Atom (1) Once, Gain)
      Hresonance _ -> (sliderWidget (0,1) 0.01, Atom (0.5) Once, Hresonance) -- @ is 0.5 an appropriate starting value?
      Pan _ -> (doubleWidget, Atom (0.5) Once, Pan)
      Resonance _ -> (sliderWidget (0,1) 0.01, Atom (0.5) Once, Resonance) -- @ is 0.5 an appropriate starting value?
      Shape _ -> (sliderWidget (0,1) 0.01, Atom (0.5) Once, Shape)
      Speed _ -> (sliderWidget (-500,500) 0.01, Atom 1 Once, Speed)
      Up _ -> (sliderWidget (-36,36) 0.5, Atom 0 Once, Up)

intContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
intContainerWidget a _ = mdo
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
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Bandf _ -> (intWidget, Atom (440) Once, Bandf)
      Coarse _ -> (intWidget, Atom (0) Once, Coarse)
      Crush _ -> (crushWidget, Atom (16) Once, Crush)
      Estuary.Tidal.Types.Cut _ -> (intWidget, Atom 1 Once, Estuary.Tidal.Types.Cut)
      Cutoff _ -> (intWidget, Atom (440) Once, Cutoff)
      Hcutoff _ -> (intWidget, Atom (440) Once, Hcutoff)
      Loop _ -> (intWidget, Atom 0 Once, Loop)
      N _ -> (intWidget, Atom (0) Once, N)


crushContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
crushContainerWidget iVal _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never crushWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left (Atom 16 Once)))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Crush $ Group x Once))
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'


sampleContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sampleContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never sampleWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert $ Left $ Atom (Sample ("~",0)) Once)])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Sound $ Group x Once))
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

panContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
panContainerWidget iVal _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never doubleWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left (Atom 0.5 Once)))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Pan $ Group x Once))
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

nContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
nContainerWidget iVal _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never intWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (N $ Group x Once))
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

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


--miscSelectButton:: MonadWidget t m => GeneralPattern a -> Event t () ->  m (Dynamic t ((),Event t GenericSignal))
--miscSelectButton

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


-- sliders from 0-1 range
doubleWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t GenericSignal))
doubleWidget iVal _ = do
  text "0"
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range","0","1","0.01","width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text "1"
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Double) then Atom (read x::Double) Once else Atom 0.5 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))

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

textWidget::MonadWidget t m => GeneralPattern String-> Event t () -> m (Dynamic t (GeneralPattern String,Event t GenericSignal))
textWidget iVal _ = do
  text "s"
  textField <-textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:40px;") & textInputConfig_initialValue .~ (show iVal)
  let inputVal = _textInput_value textField
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\str rep-> if str=="" ||str=="~" then Blank else Atom str rep) inputVal repeats'
  forDyn genPat (\k-> (k,deleteButton))

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

groupWidget::MonadWidget t m => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
groupWidget iVal _= mdo
  text " ["
  textField <-textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:50px;") & textInputConfig_initialValue .~ (show iVal)
  let inputVal = _textInput_value textField
  text "]"
  display repeats'
  text "  "
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  samples <- forDyn inputVal (words) -- dyn [str]
  samples' <- forDyn samples (Prelude.map (\x->if x=="~" then Blank else Atom x Once)) -- dyn [general Pat]
  genPat <- combineDyn (\list rep-> if list == [] then Group [Blank] Once else Group list rep) samples' repeats'
  forDyn genPat (\k-> (k,deleteButton))

groupWidget'::MonadWidget t m => GeneralPattern String -> Event t () -> m (Dynamic t (GeneralPattern String, Event t GenericSignal))
groupWidget' iVal _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never textWidget (pingButton'' "+")-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (Group x Once))
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

sampleWidget::MonadWidget t m => GeneralPattern Sample-> Event t () -> m (Dynamic t (GeneralPattern Sample,Event t GenericSignal))
sampleWidget (Atom (Sample s) r) _ = do
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

charContainerWidget':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget' a _ = mdo
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
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (charWidget, Atom 'c' Once, Unit)
      Vowel _ -> (vowelButtonWidget', Atom '~' Once, Vowel)

charContainerWidget'':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget'' a _ = mdo
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
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (charWidget, Atom 'c' Once, Unit)
      Vowel _ -> (vowelButtonWidget, Atom '~' Once, Vowel)


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

vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t GenericSignal))
vowelButtonWidget (Atom iVowel _) _ = elAttr "td" ("style"=:"width:10%") $ elAttr "table" ("style"=:"border-spacing:10px;display:inline-table;background-color:lightgreen;border:3pt solid black") $ mdo
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
