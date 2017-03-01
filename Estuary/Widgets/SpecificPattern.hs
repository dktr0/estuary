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
charContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t ()))
charContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("class"=:"addButton")) -- (tdPingButtonAttrs"+" ("style"=:("text-align:center;display:inline-table;max-width:30px;background-color:lightblue;height:30px;vertical-align:middle")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (patType $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (G.charWidget', Atom 'c' Once, Unit)
      Vowel _ -> (G.vowelButtonWidget, Atom 'X' Once, Vowel)


-- End Pattern container
-- intersperses G.faderButtonWidget with + buttons
-- see iclc fixed end widget (used in PatternChain.hs)
endContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (EditSignal a)))
endContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("class"=:("addButton")))
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> patType $ Group x Once)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (G.faderButtonWidget, Atom (1) Once, End)


intContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t ()))
intContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("class"=:"addButton"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
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


-- Sample container widget using the click-list button
-- doesn't support groups or lists.
-- Used in ICOAH widget (in PatternChain.hs)
sampleContainerWidget ::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t ()))
sampleContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never G.sButtonContainer (tdPingButtonAttrs  "+" ("class"=:"addButton"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (S $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

sContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t ()))
sContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never G.sButtonContainer (pingButton''' "+" ("class"=:"addButton"))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left Blank))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> (S $ Group x Once))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'

--specificContainer::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (EditSignal (GeneralPattern a))))
--specificContainer (Accelerate x) e = G.generalContainer (G.aGLDoubleWidget (-500) 500 1) x e >>= mapDyn (\(x,ev)->(Accelerate x,ev))
--specificContainer (Bandq x) e = G.generalContainer  (G.aGLDoubleWidget 0 22000 10) x e >>= mapDyn (\(x,ev)->(Bandq x,ev))
--specificContainer (Begin x) e = G.generalContainer  (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Begin x,ev))
--specificContainer (Delay x) e = G.generalContainer    (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Delay x,ev))
--specificContainer (Delayfeedback x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Delayfeedback x,ev))
--specificContainer (Delaytime x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Delaytime x,ev))
--specificContainer (End x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(End x,ev))
--specificContainer (Gain x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Gain x,ev))
--specificContainer (Hresonance x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Hresonance x,ev))
--specificContainer (Pan x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Pan x,ev))
--specificContainer (Resonance x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Resonance x,ev))
--specificContainer (Shape x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x e >>= mapDyn (\(x,ev)->(Shape x,ev))
--specificContainer (Speed x) e = G.generalContainer (G.aGLDoubleWidget (-999) 999 0.5) x e >>= mapDyn (\(x,ev)->(Speed x,ev))
--specificContainer (Up x) e = G.generalContainer (G.aGLDoubleWidget (-132) 132 1) x e >>= mapDyn (\(x,ev)->(Up x,ev))
--specificContainer (Bandf x) e = G.generalContainer (G.aGLIntWidget 0 25000 10) x e >>= mapDyn (\(x,ev)->(Bandf x,ev))
--specificContainer (Coarse x) e = G.generalContainer (G.aGLIntWidget 0 24 1) x e >>= mapDyn (\(x,ev)->(Coarse x,ev))
--specificContainer (Crush x) e = G.generalContainer (G.aGLIntWidget 0 16 1) x e >>= mapDyn (\(x,ev)->(Crush x,ev))
--specificContainer (Estuary.Tidal.Types.Cut x) e = G.generalContainer (G.aGLIntWidget (-50) 50 1) x e >>= mapDyn (\(x,ev)->(Estuary.Tidal.Types.Cut x,ev))
--specificContainer (Cutoff x) e = G.generalContainer (G.aGLIntWidget 0 25000 10) x e >>= mapDyn (\(x,ev)->(Cutoff x,ev))
--specificContainer (Hcutoff x) e = G.generalContainer (G.aGLIntWidget 0 25000 10) x e >>= mapDyn (\(x,ev)->(Hcutoff x,ev))
--specificContainer (Loop x) e = G.generalContainer (G.aGLIntWidget 0 1024 1) x e >>= mapDyn (\(x,ev)->(Loop x,ev))
--specificContainer (N x) e = G.generalContainer (G.aGLIntWidget 0 50 1) x e >>= mapDyn (\(x,ev)->(N x,ev))
----specificContainer (S x) e = G.generalContainer G.aGLStringWidget x e >>= mapDyn (\(x,ev)->(S x,ev))

----genCont-> dyn (genpat a, EditSignal (Genpat b))
--specificContainer (S x) e = G.generalContainer (G.aGLWidget G.popupSampleWidget) x e >>= mapDyn (\(x,ev)->(S x,ev))
----specificContainer (Sample x) e = G.generalContainer (G.aGLIntWidget G.popupSampleWidget) (Atom (Sample ("bd",0)) Once) never
---- @fix vowels...
--specificContainer (Vowel x) e = G.generalContainer G.charWidget x e >>= mapDyn (\(x,ev)->(Vowel x,ev))
--specificContainer (Unit x) e = G.generalContainer G.charWidget x e >>= mapDyn (\(x,ev)->(Unit x,ev))


specificContainer::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t ()))
specificContainer (Accelerate x) e = G.generalContainer (G.aGLDoubleWidget (-500) 500 1) x never >>= mapDyn (\(x,ev)->(Accelerate x,(() <$) ev))
specificContainer (Bandq x) e = G.generalContainer  (G.aGLDoubleWidget 0 22000 10) x never >>= mapDyn (\(x,ev)->(Bandq x,(() <$) ev))
specificContainer (Begin x) e = G.generalContainer  (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Begin x,(() <$) ev))
specificContainer (Delay x) e = G.generalContainer    (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Delay x,(() <$) ev))
specificContainer (Delayfeedback x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Delayfeedback x,(() <$) ev))
specificContainer (Delaytime x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Delaytime x,(() <$) ev))
specificContainer (End x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(End x,(() <$) ev))
specificContainer (Gain x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Gain x,(() <$) ev))
specificContainer (Hresonance x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Hresonance x,(() <$) ev))
specificContainer (Pan x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Pan x,(() <$) ev))
specificContainer (Resonance x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Resonance x,(() <$) ev))
specificContainer (Shape x) e = G.generalContainer (G.aGLDoubleWidget 0 1 0.05) x never >>= mapDyn (\(x,ev)->(Shape x,(() <$) ev))
specificContainer (Speed x) e = G.generalContainer (G.aGLDoubleWidget (-999) 999 0.5) x never >>= mapDyn (\(x,ev)->(Speed x,(() <$) ev))
specificContainer (Up x) e = G.generalContainer (G.aGLDoubleWidget (-132) 132 1) x never >>= mapDyn (\(x,ev)->(Up x,(() <$) ev))
specificContainer (Bandf x) e = G.generalContainer (G.aGLIntWidget 0 25000 10) x never >>= mapDyn (\(x,ev)->(Bandf x,(() <$) ev))
specificContainer (Coarse x) e = G.generalContainer (G.aGLIntWidget 0 24 1) x never >>= mapDyn (\(x,ev)->(Coarse x,(() <$) ev))
specificContainer (Crush x) e = G.generalContainer (G.aGLIntWidget 0 16 1) x never >>= mapDyn (\(x,ev)->(Crush x,(() <$) ev))
specificContainer (Estuary.Tidal.Types.Cut x) e = G.generalContainer (G.aGLIntWidget (-50) 50 1) x never >>= mapDyn (\(x,ev)->(Estuary.Tidal.Types.Cut x,(() <$) ev))
specificContainer (Cutoff x) e = G.generalContainer (G.aGLIntWidget 0 25000 10) x never >>= mapDyn (\(x,ev)->(Cutoff x,(() <$) ev))
specificContainer (Hcutoff x) e = G.generalContainer (G.aGLIntWidget 0 25000 10) x never >>= mapDyn (\(x,ev)->(Hcutoff x,(() <$) ev))
specificContainer (Loop x) e = G.generalContainer (G.aGLIntWidget 0 1024 1) x never >>= mapDyn (\(x,ev)->(Loop x,(() <$) ev))
specificContainer (N x) e = G.generalContainer (G.aGLIntWidget 0 50 1) x never >>= mapDyn (\(x,ev)->(N x,(() <$) ev))
--specificContainer (S x) e = G.generalContainer G.aGLStringWidget x e >>= mapDyn (\(x,ev)->(S x,ev))

--genCont-> dyn (genpat a, EditSignal (Genpat b))
specificContainer (S x) e = G.generalContainerLive G.popupSampleWidget x never >>= mapDyn (\(x,ev)->(S x,(() <$) ev))
--specificContainer (S x) e = mdo
--  v <- G.generalContainer (G.popupSampleWidget liveness) x never
--  ev <- mapDyn snd v
--  let livenessEv = ffilter (\x->case x of MakeL4->True; MakeL3 -> True; otherwise -> False) $ switchPromptlyDyn ev
--  -- livenessEv
--  liveness <- holdDyn L4 $ fmap (\x->case x of MakeL4 -> L4; otherwise->L3) livenessEv
--  mapDyn (\(x,ev)->(S x,(() <$) ev)) v

--specificContainer (Sample x) e = G.generalContainer (G.aGLIntWidget G.popupSampleWidget) (Atom (Sample ("bd",0)) Once) never
specificContainer (Vowel x) e = G.generalContainer G.charWidget x never >>= mapDyn (\(x,ev)->(Vowel x,(() <$) ev))
specificContainer (Unit x) e = G.generalContainer G.charWidget x never >>= mapDyn (\(x,ev)->(Unit x,(() <$) ev))



-- Intersperses countStepWidgets with + buttons (see ICOAH up widget in PatternChain.hs)
upContainerWidget:: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (EditSignal a)))
upContainerWidget a _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events) <- eitherContainer' initialMap cEvents never  never widgetBuilder (tdPingButtonAttrs "+" ("class"=:("addButton")))-- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ defaultGeneralPat))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  values' <- forDyn values (elems)
  returnVal <- forDyn values' (\x-> patType $ Group x Once)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (G.countStepWidget 1, Atom 0 Once, Up)
