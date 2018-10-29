{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.SpecificPattern where

import Reflex
import Reflex.Dom
import Data.Map
import Control.Monad
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)

import Estuary.Types.Hint
import Estuary.Types.Live
import Estuary.Widgets.Generic
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Container
import qualified Estuary.Widgets.GeneralPattern as G
import Estuary.Tidal.Types
import Estuary.Reflex.Utility

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
  returnVal <- forDyn values' (\x-> (patType $ Group (Live (x,Once) L4) Inert))
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Unit _ -> (G.charWidget', Atom 'c' Inert Once, Unit)
      Vowel _ -> (G.vowelButtonWidget, Atom 'X' Inert Once, Vowel)


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
  returnVal <- forDyn values' (\x-> patType $ Group (Live (x,Once) L4) Inert)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (G.faderButtonWidget, Atom (1) Inert Once, End)


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
  returnVal <- forDyn values' (\x-> patType $ Group (Live (x,Once) L4) Inert)
  display returnVal
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = case a of
      Bandf _ -> (G.intWidget, Atom (440) Inert Once, Bandf)
      Coarse _ -> (G.intWidget, Atom (0) Inert Once, Coarse)
      Crush _ -> (G.crushWidget, Atom (16) Inert Once, Crush)
      Estuary.Tidal.Types.Cut _ -> (G.intWidget, Atom 1 Inert Once, Estuary.Tidal.Types.Cut)
      Cutoff _ -> (G.intWidget, Atom (440) Inert Once, Cutoff)
      Hcutoff _ -> (G.intWidget, Atom (440) Inert Once, Hcutoff)
      Loop _ -> (G.intWidget, Atom 0 Inert Once, Loop)
      N _ -> (G.intWidget, Atom (0) Inert Once, N)


-- Sample container widget using the click-list button
-- doesn't support groups or lists.
-- Used in ICOAH widget (in PatternChain.hs)
sampleContainerWidget ::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t (),Event t Hint))
sampleContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events,hints) <- eitherContainer''' initialMap cEvents never never G.sButtonContainer tdPingButton'' -- values:dyn Map k GeneralPattern,
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert $ Left $ Blank Inert )])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  mapDyn ((\x -> ((S $ Group (Live (x,Once) L4) Inert),never,hints)) .  elems) values
  where
    tdPingButton' = tdPingButtonAttrs  "+" ("class"=:"addButton")
    tdPingButton'' x e = tdPingButton' x e >>= mapDyn (\(a,b) -> (a,b,never))

sContainerWidget::(MonadWidget t m) => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern,Event t ()))
sContainerWidget (S genPat) _ = mdo
  let initialMap = (0::Int)=:(Right ())
  let cEvents = mergeWith (union) [makeSMap,deleteMap]
  (values,events,hints) <- eitherContainer''' initialMap cEvents never never G.sButtonContainer tdPingButton''
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events --Event [keys]
  let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys -- Evnt []
  let deleteMap = fmap (fromList) deleteList
  let makeSKeys = fmap (keys . Data.Map.filter (isChangeValue)) events
  let makeSList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Blank Inert))])) makeSKeys
  let makeSMap = fmap (fromList) makeSList
  mapDyn ((\x -> ( S $ Group (Live (x,Once) L4) Inert,never)) . elems) values
  where
    tdPingButton' = pingButton''' "+" ("class"=:"addButton")
    tdPingButton'' x e = tdPingButton' x e >>= mapDyn (\(a,b) -> (a,b,never))

specificContainer::MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (), Event t Hint))
specificContainer (Accelerate x) e = G.generalContainerLive' (G.popupDoubleWidget 0 (-500) 500 1) x never >>= mapDyn (\(x,ev,h)->(Accelerate x,(() <$) ev,h))
specificContainer (Bandq x) e = G.generalContainerLive'  (G.popupDoubleWidget 1 0 22000 10) x never >>= mapDyn (\(x,ev,h)->(Bandq x,(() <$) ev,h))
specificContainer (Begin x) e = G.generalContainerLive'  (G.popupDoubleWidget 0 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Begin x,(() <$) ev,h))
specificContainer (Delay x) e = G.generalContainerLive'    (G.popupDoubleWidget 0.5 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Delay x,(() <$) ev,h))
specificContainer (Delayfeedback x) e = G.generalContainerLive' (G.popupDoubleWidget 0.2 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Delayfeedback x,(() <$) ev,h))
specificContainer (Delaytime x) e = G.generalContainerLive' (G.popupDoubleWidget 0.5 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Delaytime x,(() <$) ev,h))
specificContainer (End x) e = G.generalContainerLive' (G.popupDoubleWidget 1 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(End x,(() <$) ev,h))
specificContainer (Gain x) e = G.generalContainerLive' (G.popupDoubleWidget 1 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Gain x,(() <$) ev,h))
specificContainer (Hresonance x) e = G.generalContainerLive' (G.popupDoubleWidget 0 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Hresonance x,(() <$) ev,h))
specificContainer (Pan x) e = G.generalContainerLive' (G.popupDoubleWidget 0.5 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Pan x,(() <$) ev,h))
specificContainer (Resonance x) e = G.generalContainerLive' (G.popupDoubleWidget 0 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Resonance x,(() <$) ev,h))
specificContainer (Shape x) e = G.generalContainerLive' (G.popupDoubleWidget 0 0 1 0.05) x never >>= mapDyn (\(x,ev,h)->(Shape x,(() <$) ev,h))
-- specificContainer (Speed x) e = G.generalContainerLive' (G.popupDoubleWidget 1 (-999) 999 0.5) x never >>= mapDyn (\(x,ev,h)->(Speed x,(() <$) ev,h))
specificContainer (Speed x) e = G.generalContainerLive' (G.typedAtomWidget 1) x never >>= mapDyn (\(x,ev,hint)->(Speed x,(() <$) ev,hint))

specificContainer (Up x) e = G.generalContainerLive' (G.popupDoubleWidget 0 (-132) 132 1) x never >>= mapDyn (\(x,ev,h)->(Up x,(() <$) ev,h))
specificContainer (Bandf x) e = G.generalContainerLive' (G.popupIntWidget 440 0 25000 10) x never >>= mapDyn (\(x,ev,h)->(Bandf x,(() <$) ev,h))
specificContainer (Coarse x) e = G.generalContainerLive' (G.popupIntWidget 0 0 24 1) x never >>= mapDyn (\(x,ev,h)->(Coarse x,(() <$) ev,h))
specificContainer (Crush x) e = G.generalContainerLive' (G.popupIntWidget 16 0 16 1) x never >>= mapDyn (\(x,ev,h)->(Crush x,(() <$) ev,h))
specificContainer (Estuary.Tidal.Types.Cut x) e = G.generalContainerLive' (G.popupIntWidget 1 (-50) 50 1) x never >>= mapDyn (\(x,ev,h)->(Estuary.Tidal.Types.Cut x,(() <$) ev,h))
specificContainer (Cutoff x) e = G.generalContainerLive' (G.popupIntWidget 20000 0 25000 10) x never >>= mapDyn (\(x,ev,h)->(Cutoff x,(() <$) ev,h))
specificContainer (Hcutoff x) e = G.generalContainerLive' (G.popupIntWidget 0 0 25000 10) x never >>= mapDyn (\(x,ev,h)->(Hcutoff x,(() <$) ev,h))
specificContainer (Loop x) e = G.generalContainerLive' (G.popupIntWidget 0 0 1024 1) x never >>= mapDyn (\(x,ev,h)->(Loop x,(() <$) ev,h))
specificContainer (N x) e = G.generalContainerLive' (G.popupIntWidget 0 0 50 1) x never >>= mapDyn (\(x,ev,h)->(N x,(() <$) ev,h))

specificContainer (S x) e = do
  -- a <- G.generalContainerLive' G.popupSampleWidget x never >>= mapDyn (\(x,ev,hint)->(S x,(() <$) ev,hint))
  G.generalContainerLive' (G.typedAtomWidget "~") x never >>= mapDyn (\(x,ev,hint)->(S x,(() <$) ev,hint))
  --
  -- -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
  --
  -- -- mapDyn (\(x,_,_)-> show x) a >>= dynText
  -- return (constDyn $ S Blank,never,never)
--specificContainer (Sample x) e = G.generalContainer (G.aGLIntWidget G.popupSampleWidget) (Atom (Sample ("bd",0)) Once) never
specificContainer (Vowel x) e = G.generalContainerLive' G.charWidget x never >>= mapDyn (\(x,ev,h)->(Vowel x,(() <$) ev,h))
specificContainer (Unit x) e = G.generalContainerLive' G.charWidget x never >>= mapDyn (\(x,ev,h)->(Unit x,(() <$) ev,h))



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
  returnVal <- forDyn values' (\x-> patType $ Group (Live (x,Once) L4) Inert)
  returnVal'<-forDyn returnVal (\x->(x,never))
  return returnVal'
  where
    (widgetBuilder,defaultGeneralPat, patType) = (G.countStepWidget 1, Atom 0 Inert Once, Up)
