{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.SpecificPattern
where

import Reflex
import Reflex.Dom
import Estuary.Reflex.Container
import qualified Estuary.Widgets.GeneralPattern as G
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)


-- see ICOAH vowel widget for example (using vowelButtonWidget)
-- Vowel Pattern
charContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (pingButton''' "+" ("style"=:"background-color:lightblue")) -- (tdPingButtonAttrs"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (G.charWidget, Atom 'c' Once, Unit)
      Vowel _ -> (G.vowelButtonWidget, Atom 'X' Once, Vowel)


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
      Unit _ -> (G.charWidget, Atom 'c' Once, Unit)
      Vowel _ -> (G.vowelButtonWidget', Atom '~' Once, Vowel)

charContainerWidget'':: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
charContainerWidget'' a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("style"=:"text-align:center;display:inline-table;max-width:20%;background-color:lightblue;height:50%"))
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
    tableAttrs=("style"=:"text-align:center;width:90%;display:inline-block;border-spacing:5px;")
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (G.charWidget, Atom 'c' Once, Unit)
      Vowel _ -> (G.vowelButtonWidget, Atom 'X' Once, Vowel)



crushContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
crushContainerWidget iVal _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never G.crushWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
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
      Accelerate _ -> (G.sliderWidget (-500,500) 0.01 , Atom (1) Once, Accelerate) --making an assumption about accelerate values being [-500,500]
      Bandq _ -> (G.sliderWidget (0,400) 1, Atom (100) Once, Bandq) -- !!@@making an uneducated assupmtion about Bandq values....
      Begin _ -> (G.sliderWidget (0,1) 0.01, Atom (0) Once, Begin)
      Delay _ -> (G.sliderWidget (0,1) 0.01, Atom (0) Once, Delay)
      Delayfeedback _ -> (G.sliderWidget (0,1) 0.01, Atom (0) Once, Delayfeedback)
      Delaytime _ -> (G.sliderWidget (0,1) 0.01, Atom (0.5) Once, Delaytime)
      End _ -> (G.endWidget, Atom (1) Once, End)
      Gain _ -> (G.sliderWidget (0,4) 0.01, Atom (1) Once, Gain)
      Hresonance _ -> (G.sliderWidget (0,1) 0.01, Atom (0.5) Once, Hresonance) -- @ is 0.5 an appropriate starting value?
      Pan _ -> (G.sliderWidget (0,1) 0.01, Atom (0.5) Once, Pan)
      Resonance _ -> (G.sliderWidget (0,1) 0.01, Atom (0.5) Once, Resonance) -- @ is 0.5 an appropriate starting value?
      Shape _ -> (G.sliderWidget (0,1) 0.01, Atom (0.5) Once, Shape)
      Speed _ -> (G.sliderWidget (-500,500) 0.01, Atom 1 Once, Speed)
      Up _ -> (G.sliderWidget (-36,36) 0.5, Atom 0 Once, Up)


-- End Pattern container
-- intersperses G.faderButtonWidget with + buttons
-- see iclc fixed end widget
endContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
endContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
    (widgetBuilder,defaultGeneralPat, patType) = (G.faderButtonWidget, Atom (1) Once, End)


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
      Bandf _ -> (G.intWidget, Atom (440) Once, Bandf)
      Coarse _ -> (G.intWidget, Atom (0) Once, Coarse)
      Crush _ -> (G.crushWidget, Atom (16) Once, Crush)
      Estuary.Tidal.Types.Cut _ -> (G.intWidget, Atom 1 Once, Estuary.Tidal.Types.Cut)
      Cutoff _ -> (G.intWidget, Atom (440) Once, Cutoff)
      Hcutoff _ -> (G.intWidget, Atom (440) Once, Hcutoff)
      Loop _ -> (G.intWidget, Atom 0 Once, Loop)
      N _ -> (G.intWidget, Atom (0) Once, N)


sampleContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sampleContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never G.sampleTextWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
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

-- Sample container widget using the click-list button
-- doesn't support groups or lists.
-- Used in ICOAH widget
sampleContainerWidget' ::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
sampleContainerWidget' (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never G.sampleNameWidget (tdPingButtonAttrs  "+" ("style"=:"text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:50%;vertical-align:middle"))-- values:dyn Map k GeneralPattern,
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

nContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t GenericSignal))
nContainerWidget iVal _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never G.intWidget (pingButton''' "+" ("style"=:"background-color:lightblue")) -- values:dyn Map k GeneralPattern,
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
  (values,events) <- eitherContainer' initialMap cEvents never  never G.genPatButtonWidget (pingButton''' "+" ("style"=:"background-color:lightblue"))-- values:dyn Map k GeneralPattern,
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

-- Calls G.doubleContainer with the appropriate stepsize, min, and max range values
specificDoubleContainer::MonadWidget t m=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
specificDoubleContainer iVal e = do
  genPat <- G.doubleContainer vMin vMax step iGroup e -- m (dyn gen eve)
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


-- Calls G.intContainer with the appropriate stepsize, min, and max range values
specificIntContainer::MonadWidget t m=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
specificIntContainer iVal e = do
  genPat <- G.intContainer vMin vMax step iGroup e -- m (dyn gen eve)
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

-- Points to appropriate General Pattern Container given the initial value
-- To be adapted/expanded for further tidal patterns that take strings
specificStringContainer::MonadWidget t m=> SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
specificStringContainer iVal e = do
  genPat <- G.stringContainer iGroup e -- m (dyn gen eve)
  mapDyn (\(x,ev)->(constructor x, ev)) genPat
  where
    (constructor, iGroup) = case iVal of
      (S _) -> (S , Atom "~" Once)


-- Intersperses countStepWidgets with + buttons (see ICOAH up widget)
upContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t GenericSignal))
upContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
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
    (widgetBuilder,defaultGeneralPat, patType) = (G.countStepWidget 1, Atom 0 Once, Up)


