{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.GeneralPattern
where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Container
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Data.Maybe
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse, findIndex, elem, elemIndex)
import Data.Either(partitionEithers)
import GHCJS.DOM.EventM
import Data.Maybe(isJust,listToMaybe,fromMaybe,fromJust)
import Text.Read(readMaybe)
import Control.Applicative (liftA2)

import Estuary.Types.Hint


generalContainerLive' :: (MonadWidget t m, Eq a, Show a) 
  => (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint)))
  -> GeneralPattern a 
  -> Event t (EditSignal (GeneralPattern a)) 
  -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
generalContainerLive' b i e= mdo
  gpat <- resettableWidget (generalContainerLive b) i e rebuildEvent'
  ev <- liftM switchPromptlyDyn $ mapDyn (\(_,x,_)->x) gpat
  let rebuildEvent = ffilter (\x->case x of RebuildMe -> True; otherwise->False) ev
  let rebuildEvent' = attachDynWith (\(v,_,_) ev->v) gpat rebuildEvent
  return gpat



generalContainerLive :: (MonadWidget t m, Eq a, Show a) 
  => (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint)))
  -> GeneralPattern a 
  -> Event t (EditSignal (GeneralPattern a)) 
  -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
generalContainerLive b i _ = elClass "div" (getClass i) $ mdo
  text "["
  let cEvents = mergeWith (union) [insertMap,deleteMap]


  -- m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) )
  --inter <- flippableWidget (eitherContainer4 (initialMap i) cEvents livenessEvMap livenessEvMap (leftBuilder liveness) (rightBuilder liveness)) (return $ (constDyn empty,never,never)) False $ updated splitTog
  (allValues,events,hints) <- eitherContainer4 (initialMap i) cEvents livenessEvMap livenessEvMap (leftBuilder liveness) (rightBuilder liveness)

  --allValues <-  mapDyn (\(a,_,_)->a) inter  -- Dynamic Map (k,Either (GeneralPattern a,() ))
  --events <- liftM switchPromptlyDyn $ mapDyn (\(_,b,_)->b) inter  -- Event (Map k e)
  --hints <- liftM (switchPromptlyDyn) $ mapDyn (\(_,_,c)->c) inter  -- Event []

   -- -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) , Event t (Map k e1) )


--(Dynamic t (Map Int (Either (GeneralPattern a) ())) ,    Event t (Map Int (EditSignal (GeneralPattern a))),   Event t Hint)

  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
  let insertMap = fmap (fromList . concat . (insertList (Blank Inert)) . keys . Data.Map.filter isChangeValue )  events

  values <- mapDyn (fst . Data.Either.partitionEithers . elems) allValues
  childKeys <- mapDyn keys allValues
  let events' = fmap (Data.Map.elems) events -- Event [l]

  let potential = constDyn Inert

  let livenessEv = fmap (\x-> if Data.List.elem MakeL3 x then L3 else L4) $ ffilter (\x-> Data.List.elem MakeL3 x || Data.List.elem MakeL4 x) events' -- If any child reports a liveness change
  let livenessEvMap = attachDynWith (\k v -> fromList $ zip k $ repeat (case v of L3 -> MakeL3; otherwise -> MakeL4)) childKeys livenessEv -- Ev (Map Int MakeL3) -- cycled back to the container to change all children to appropriate liveness
  liveness <- holdDyn L4 livenessEv
  let evalEv = (Eval <$) $ ffilter (\x-> Data.List.elem Eval x) events' -- If any child reports a change 
  let groupLayerEv = (RebuildMe <$) $ ffilter (\x-> Data.List.elem MakeGroup x || Data.List.elem MakeLayer x) events'
  let deleteContainerEv = (DeleteMe <$) $ ffilter (\x -> Data.List.elem DeleteContainer x) events'

  unchangedVal <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) $ tagDyn values $ leftmost [evalEv, fmap (\x-> if x==L3 then MakeL3 else MakeL4) livenessEv]

  oldAndNew <- combineDyn (,) unchangedVal values
  live <- combineDyn (\l (oldVal, newVal) ->if l==L4 then Live (newVal,Once) L4 else Edited (oldVal,Once) (newVal,Once)) liveness oldAndNew

  let splitIndex = coincidence $ fmap (const $ fmap (head . keys . Data.Map.filter (==LayerSplit)) events) $ ffilter (Data.List.elem LayerSplit) events'

  let splitVal = attachDynWith (\l index-> let (vals,rep) = case l of (Edited (old,reps) new) -> (old,reps); (Live (v,reps) _) -> (v,reps); in Layers (Live ([Group (Live (take (div index 2) vals,Once) L4) Inert, Group (Live (reverse $ take ((length vals) - (div index 2)) (reverse vals), Once) L4) Inert],rep) L4) Inert) live splitIndex
  let returnEvents = (leftmost [fmap (\x-> if x==L3 then MakeL3 else MakeL4) livenessEv, deleteContainerEv, (RebuildMe <$) $ ffilter (Data.List.elem LayerSplit) events',groupLayerEv])
  retVal <- combineDyn (\l p-> returnF i l p returnEvents hints) live potential
  regVal <- mapDyn (\(x,_,_)->x) retVal
  --regVal' <- holdDyn (Blank Inert ) splitVal >>= combineDyn (\reg spV -> case spV of (Blank _ ) -> reg; otherwise -> (spV,returnEvents,hints)) regVal
  updatedRegVal <- liftM joinDyn $ holdDyn regVal $ fmap constDyn splitVal

  --let splitBuilder = attachDynWith (\vals split-> do 
  --                                                let vals' = elems vals
  --                                                let leftList = (\x-> if x==[] then [Blank Inert] else x) $ fst . partitionEithers $ take split vals'
  --                                                let rightList = (\x-> if x==[] then [Blank Inert] else x) $ fst . partitionEithers . reverse $ take ((length vals') - split) $ reverse vals'
  --                                                left <- generalContainerLive b (Group (Live (rightList,Once) L4) Inert) never 
  --                                                text ", "
  --                                                right <- generalContainerLive b (Group (Live (rightList,Once) L4) Inert) never
  --                                                combineDyn (\(leftV,leftEv,leftHint) (rightV,rightEv,rightHint) -> (Layers (Live ([leftV,rightV],Once) L4) Inert,leftmost [leftEv,rightEv],leftmost [rightHint,leftHint])) left right
  --                                                ) allValues splitEv



  --returnVal <-liftM joinDyn $ widgetHold (return regVal') splitBuilder
  text "]"
  --mapDyn (\(v,e)->(v,e,hints)) returnVal
  combineDyn (\reg (_,e,h)->(reg,e,h)) updatedRegVal retVal
  where
    initialVal (Atom iV _ _) = iV
    initialVal (Group (Live (iV,_) _) _) = initialVal $ iV!!0
    initialVal (Group (Edited (iV,_) _) _) = initialVal $ iV!!0
    initialVal (Layers (Live (iV,_) _) _) = initialVal $ iV!!0
    initialVal (Layers (Edited (iV,_) _) _) = initialVal $ iV!!0
    getClass (Layers _ _) = "generalPattern-layer"
    getClass (Group _ _) = "generalPattern-group"
    getClass (Atom _ _ _) = "generalPattern-atom"
    initialMap (Layers (Live (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Layers (Edited (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group (Live (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group (Edited (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Atom iVal p iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal p iReps, Right ()]
    leftBuilder live = aGLWidgetLive live b
    rightBuilder live= whitespace live (i) "whiteSpaceAdd" [ChangeValue (Blank Inert),DeleteContainer]
    insertList (Atom iVal p r) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) p r))])
    insertList (Layers (Edited (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Layers (Live (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group (Edited (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group (Live (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Blank p) = Prelude.map (\k -> [(k,Insert (Right ())),  (k+1, Insert (Left $ Blank p))])
    returnF (Layers _ _) x p e h= (Layers x p, e,h)
    returnF (Group _ _) x p e h= (Group x p,e,h)
    returnF (Atom _ _ _) x p e h= (Group x p,e,h)

--generalContainerLive :: (MonadWidget t m, Eq a, Show a) 
--  => (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint)))
--  -> GeneralPattern a 
--  -> Event t (EditSignal (GeneralPattern a)) 
--  -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
--generalContainerLive b i _ = elClass "div" (getClass i) $ mdo
--  text (case i of (Group _ _)-> "["; (Layers _ _)-> "["; otherwise -> "")
--  let cEvents = mergeWith (union) [insertMap,deleteMap]


--  -- m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) )
--  inter <- flippableWidget (eitherContainer4 (initialMap i) cEvents livenessEvMap livenessEvMap (leftBuilder liveness) (rightBuilder liveness)) (return $ (constDyn empty,never,never)) False $ updated splitTog
--  allValues <- liftM joinDyn $ mapDyn (\(a,_,_)->a) inter  -- Dynamic Map (k,Either (GeneralPattern a,() ))
--  events <- liftM switchPromptlyDyn $ mapDyn (\(_,b,_)->b) inter  -- Event (Map k e)
--  hints <- liftM (switchPromptlyDyn) $ mapDyn (\(_,_,c)->c) inter  -- Event []

--   -- -> m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) , Event t (Map k e1) )


--  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
--  let insertMap = fmap (fromList . concat . (insertList (Blank Inert)) . keys . Data.Map.filter isChangeValue )  events

--  values <- mapDyn (fst . Data.Either.partitionEithers . elems) allValues
--  childKeys <- mapDyn keys allValues
--  let events' = fmap (Data.Map.elems) events -- Event [l]

--  let potential = constDyn Inert

--  let livenessEv = fmap (\x-> if Data.List.elem MakeL3 x then L3 else L4) $ ffilter (\x-> Data.List.elem MakeL3 x || Data.List.elem MakeL4 x) events' -- If any child reports a liveness change
--  let livenessEvMap = attachDynWith (\k v -> fromList $ zip k $ repeat (case v of L3 -> MakeL3; otherwise -> MakeL4)) childKeys livenessEv -- Ev (Map Int MakeL3) -- cycled back to the container to change all children to appropriate liveness
--  liveness <- holdDyn L4 livenessEv
--  let evalEv = (Eval <$) $ ffilter (\x-> Data.List.elem Eval x) events' -- If any child reports a change 
--  let deleteContainerEv = (DeleteMe <$) $ ffilter (\x -> Data.List.elem DeleteContainer x) events'

--  unchangedVal <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) $ tagDyn values $ leftmost [evalEv, fmap (\x-> if x==L3 then MakeL3 else MakeL4) livenessEv]

--  oldAndNew <- combineDyn (,) unchangedVal values
--  live <- combineDyn (\l (oldVal, newVal) ->if l==L4 then Live (newVal,Once) L4 else Edited (oldVal,Once) (newVal,Once)) liveness oldAndNew

--  let splitEv = coincidence $ fmap (const $ fmap (head . keys . Data.Map.filter (==LayerSplit)) events) $ ffilter (Data.List.elem LayerSplit) events'

--  let splitVal = attachDynWith (\l index-> let (vals,rep) = case l of (Edited (old,reps) new) -> (old,reps); (Live (v,reps) _) -> (v,reps); in Layers (Live ([Group (Live (take (index+1) vals,Once) L4) Inert, Group (Live (reverse $ take ((length vals) - (index+1)) (reverse vals), Once) L4) Inert],rep) L4) Inert) live splitEv
--  let returnEvents = (leftmost [fmap (\x-> if x==L3 then MakeL3 else MakeL4) livenessEv, deleteContainerEv, (RebuildMe <$) $ ffilter (Data.List.elem LayerSplit) events'])
--  regVal <- combineDyn (\l p-> returnF i l p returnEvents hints) live potential
--  regVal' <- holdDyn (Blank Inert ) splitVal >>= combineDyn (\reg spV -> case spV of (Blank _ ) -> reg; otherwise -> (spV,returnEvents,hints)) regVal

--  --let splitBuilder = attachDynWith (\vals split-> do 
--  --                                                  left <- generalContainerLive b (Group (Live ((take (div split 2) vals),Once) L4) Inert) never 
--  --                                                  right <- generalContainerLive b (Group (Live ((reverse $ take ((length vals) - (div split 2)) (reverse vals)),Once) L4) Inert) never                                                    
--  --                                                  combineDyn (\(leftV,leftEv,leftHint) (rightV,rightEv,rightHint) -> (Layers (Live ([leftV,rightV],Once) L4) Inert,leftmost [leftEv,rightEv],leftmost [leftHint,rightHint])) left right
--  --                                                  ) values splitEv

--  let splitBuilder = attachDynWith (\vals split-> do 
--                                                  let vals' = elems vals
--                                                  let leftList = (\x-> if x==[] then [Blank Inert] else x) $ fst . partitionEithers $ take split vals'
--                                                  let rightList = (\x-> if x==[] then [Blank Inert] else x) $ fst . partitionEithers . reverse $ take ((length vals') - split) $ reverse vals'
--                                                  left <- generalContainerLive b (Group (Live (rightList,Once) L4) Inert) never 
--                                                  text ", "
--                                                  right <- generalContainerLive b (Group (Live (rightList,Once) L4) Inert) never
--                                                  combineDyn (\(leftV,leftEv,leftHint) (rightV,rightEv,rightHint) -> (Layers (Live ([leftV,rightV],Once) L4) Inert,leftmost [leftEv,rightEv],leftmost [rightHint,leftHint])) left right
--                                                  ) allValues splitEv

--  splitTog <- toggle False splitBuilder

--  returnVal <-liftM joinDyn $ widgetHold (return regVal') splitBuilder
--  text (case i of (Group _ _)-> "]"; (Layers _ _)-> "]"; otherwise -> "")
--  --mapDyn (\(v,e)->(v,e,hints)) returnVal
--  return returnVal
--  where
--    initialVal (Atom iV _ _) = iV
--    initialVal (Group (Live (iV,_) _) _) = initialVal $ iV!!0
--    initialVal (Group (Edited (iV,_) _) _) = initialVal $ iV!!0
--    initialVal (Layers (Live (iV,_) _) _) = initialVal $ iV!!0
--    initialVal (Layers (Edited (iV,_) _) _) = initialVal $ iV!!0
--    getClass (Layers _ _) = "generalPattern-layer"
--    getClass (Group _ _) = "generalPattern-group"
--    getClass (Atom _ _ _) = "generalPattern-atom"
--    initialMap (Layers (Live (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--    initialMap (Layers (Edited (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--    initialMap (Group (Live (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--    initialMap (Group (Edited (xs,_) _) _) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
--    initialMap (Atom iVal p iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal p iReps, Right ()]
--    leftBuilder live = aGLWidgetLive live b
--    rightBuilder live= whitespace live (i) "whiteSpaceAdd" [ChangeValue (Blank Inert)]
--    insertList (Atom iVal p r) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) p r))])
--    insertList (Layers (Edited (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
--    insertList (Layers (Live (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
--    insertList (Group (Edited (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
--    insertList (Group (Live (xs,_) _) _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
--    insertList (Blank p) = Prelude.map (\k -> [(k,Insert (Right ())),  (k+1, Insert (Left $ Blank p))])
--    returnF (Layers _ _) x p e h= (Layers x p, e,h)
--    returnF (Group _ _) x p e h= (Group x p,e,h)
--    returnF (Atom _ _ _) x p e h= (Group x p,e,h)

aGLWidgetLive::(MonadWidget t m, Eq a, Show a) => Dynamic t Liveness -> (Dynamic t Liveness -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))) -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)), Event t Hint))
aGLWidgetLive liveness builder iVal ev = mdo
  val <- resettableWidget (function) iVal ev rebuildEvent'
  widgetEvents <- liftM switchPromptlyDyn $ mapDyn (\(_,y,_)->y) val
  let rebuildEvent = ffilter (\x->case x of RebuildMe -> True; otherwise->False) widgetEvents
  --holdDyn "no rebuild yet" (("event" <$) rebuildEvent) >>= dynText
  let rebuildEvent' = attachDynWith (\(value,_,_) _ ->value) val rebuildEvent
  return val
  where
    function (Atom x p r) e = builder liveness (Atom x p r) e
    function (Blank p) e = builder liveness (Blank p) e
    function (Group l p) e = generalContainerLive (builder) (Group l p) e
    function (Layers l p) e = generalContainerLive  (builder) (Layers l p) e



--popupSampleWidget :: MonadWidget t m => Dynamic t Liveness -> GeneralPattern String -> Event t (EditSignal (GeneralPattern String)) -> m (Dynamic t (GeneralPattern String, Event t (EditSignal (GeneralPattern String)), Event t Hint))
--popupSampleWidget liveness iVal e = elClass "div" "atomPopup" $ mdo
--  sample <- clickableSpanClass inVal "noClass" ()
--  --repDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
--  repDivEv <- liftM joinDyn $ flippableWidget (return $ constDyn Once) (repDivWidget'' iRepDiv never) iRepDivViewable $ updated repDivToggle

--  (dynPopup, dynHintEv) <- flippableWidget (return (never,never)) popup (case iPotential of Inert->False; otherwise->True) (updated popupToggle) >>= splitDyn 
--  let popupMenu =  switch $ current dynPopup
--  let hintEv = switchPromptlyDyn dynHintEv
--  let closeEvents = (() <$)  $ ffilter (==Nothing)  popupMenu
--  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
--  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu



--  repDivVal'<- holdDyn iRepDiv $ fmap (\x->if x then Rep 1 else Once) $ updated repDivToggle
--  --repDivVal'' <- liftM joinDyn $ holdDyn repDivVal' $ fmap constDyn repDivEv
--  repDivVal'' <- liftM joinDyn $ holdDyn repDivVal' (fmap constDyn $ updated repDivEv)

--  repDivVal <- combineDyn (\tog val -> if tog then val else Once) repDivToggle repDivVal''
--  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
--  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
--  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
--  let sampleChanges = fmap (\x-> maybe (Blank Inert ) (\y-> case y of (ChangeValue z)-> Atom z Inert Once; otherwise -> Blank Inert) x) $ ffilter (\x-> maybe False (isChangeValue) x) popupMenu -- Event t (GeneralPat)
  
--  popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost $ [(() <$) sample,(() <$) closeEvents,(() <$) livenessEv, (() <$) sampleChanges,(() <$) $ updated repDivToggle]
--  inVal <- holdDyn iSamp $ fmap show sampleChanges
--  potential <- liftM updated (mapDyn (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle) >>= holdDyn iPotential

--  holdDyn "nothing yet" (fmap show $ updated repDivVal) >>= dynText

--  atomVal <- combineDyn (\val pot -> Atom val pot) inVal potential >>= combineDyn (\r con ->con r) repDivVal
--  groupVal <- combineDyn (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
--  let groupEvVal = tag (current groupVal) groupEv
--  layerVal <- combineDyn (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
--  let layerEvVal = tag (current layerVal) layerEv
--  genPat <- liftM joinDyn $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
--  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv]
--  mapDyn (\x-> (x,upSig,hintEv)) genPat
--  where
--    popupActions = [MakeRepOrDiv, MakeGroup, MakeLayer, DeleteMe]
--    sampleMap = fromList $ zip [0::Int,1..] $ [("Rest","~"),("Percussion", "bd"),("Percussion", "cp"),("Percussion", "hh"),("Percussion", "sn"),("Percussion","dr"),("Percussion","jazz"),("Bass","jvbass"), ("Bass","wobble"),("Bass","bass1"),("Pitched","arpy"), ("Pitched", "casio"), ("Pitched","latibro")]
--    popup = samplePickerPopup liveness sampleMap popupActions
--    iRepDivViewable = (iRepDiv/=Once)
--    (iSamp,iRepDiv,iPotential) = case iVal of
--                    (Group (Edited (oldV,oldR) (newV,newR)) p) -> (show $ oldV!!0,oldR,p)
--                    (Group (Live (newV,newR) lness) p) -> (show $ newV!!0,newR,p)
--                    (Layers (Edited (oldV,oldR) (newV,newR)) p) -> (show $ oldV!!0,oldR,p)
--                    (Layers (Live (newV,newR) lness) p) -> (show $ newV!!0,newR,p)
--                    (Atom v p r) -> (v,r,p)
--                    otherwise -> ("~",Once,Inert)


popupSampleWidget :: MonadWidget t m => Dynamic t Liveness -> GeneralPattern String -> Event t (EditSignal (GeneralPattern String)) -> m (Dynamic t (GeneralPattern String, Event t (EditSignal (GeneralPattern String)), Event t Hint))
popupSampleWidget liveness iVal e = elClass "div" "atomPopup" $ mdo
  sample <- clickableSpanClass inVal "noClass" ()
  --repDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  repDivVal <- liftM joinDyn $ flippableWidget (return $ constDyn Once) (repDivWidget'' iRepDiv never) iRepDivViewable $ updated repDivToggle

  --let repDivVal = constDyn $ Rep 1

  (dynPopup, dynHintEv) <- flippableWidget (return (never,never)) popup (case iPotential of Inert->False; otherwise->True) (updated popupToggle) >>= splitDyn 

  let popupMenu =  switchPromptlyDyn dynPopup
  let hintEv = switchPromptlyDyn dynHintEv
  let closeEvents = (() <$)  $ ffilter (==Nothing)  popupMenu
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu



  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
  let sampleChanges = fmap (\x-> maybe (Blank Inert ) (\y-> case y of (ChangeValue z)-> Atom z Inert Once; otherwise -> Blank Inert) x) $ ffilter (\x-> maybe False (isChangeValue) x) popupMenu -- Event t (GeneralPat)
  
  --popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost $ [(() <$) sample,(() <$) closeEvents,(() <$) livenessEv, (() <$) sampleChanges] --, (() <$) $ updated repDivToggle]
  --let popupToggle = constDyn False
 
  popupToggle <- toggle (case iPotential of Inert -> False; otherwise-> True) $ leftmost [(() <$) sample, (()<$) popupMenu]

  inVal <- holdDyn iSamp $ fmap show sampleChanges

  potential <- holdDyn iPotential $ fmap (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) (updated popupToggle)  


  rdv <- holdDyn iRepDiv $ fmap (\x-> if x then Rep 4 else Once) (updated repDivToggle)  

  atomVal <- combineDyn (\val pot -> Atom val pot) inVal potential >>= combineDyn (\r con ->con r) repDivVal

  groupVal <- combineDyn (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
  let groupEvVal = tag (current groupVal) groupEv
  layerVal <- combineDyn (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
  let layerEvVal = tag (current layerVal) layerEv
  genPat <- liftM joinDyn $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
  --let genPat = atomVal
  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv] --, (RebuildMe <$) $ updated repDivVal]
  mapDyn (\x-> (x,upSig,hintEv)) genPat
  where
    popupActions = [MakeRepOrDiv, MakeGroup, MakeLayer, DeleteMe]
    sampleMap = fromList $ zip [0::Int,1..] $ [("Rest","~"),("Percussion", "bd"),("Percussion", "cp"),("Percussion", "hh"),("Percussion", "sn"),("Percussion","dr"),("Percussion","jazz"),("Bass","jvbass"), ("Bass","wobble"),("Bass","bass1"),("Pitched","arpy"), ("Pitched", "casio"), ("Pitched","latibro")]
    popup = samplePickerPopup liveness sampleMap popupActions
    iRepDivViewable = (iRepDiv/=Once)
    (iSamp,iRepDiv,iPotential) = case iVal of
                    (Group (Edited (oldV,oldR) (newV,newR)) p) -> (show $ oldV!!0,oldR,p)
                    (Group (Live (newV,newR) lness) p) -> (show $ newV!!0,newR,p)
                    (Layers (Edited (oldV,oldR) (newV,newR)) p) -> (show $ oldV!!0,oldR,p)
                    (Layers (Live (newV,newR) lness) p) -> (show $ newV!!0,newR,p)
                    (Atom v p r) -> (v,r,p)
                    otherwise -> ("~",Once,Inert)


popupIntWidget :: MonadWidget t m => Int -> Int -> Int -> Int -> Dynamic t Liveness -> GeneralPattern Int -> Event t (EditSignal (GeneralPattern Int)) -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal (GeneralPattern Int)), Event t Hint))
popupIntWidget defaultVal minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
  let (iVal,iRepDiv,iPotential) = getIVal iGenPat
  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number" & textInputConfig_attributes .~ (constDyn ("style"=:"width:30px; border: none"))
  let iRepDivViewable = (iRepDiv/=Once)
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) (case iPotential of Inert->False; otherwise->True) $ updated popupToggle
  let popupEv = leftmost $ [ffilter id $ updated $ _textInput_hasFocus textField, closeEvent]  
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= combineDyn (\tog val-> if tog then val else Once) repDivToggle
  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
  popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost [(()<$) popupMenu,(()<$) livenessEv , (()<$) $ ffilter id $ updated $ _textInput_hasFocus textField,(() <$)closeEvent,(() <$) groupEv, (() <$) layerEv]
  inVal <- mapDyn (maybe iVal id . (readMaybe::String -> Maybe Int)) $ _textInput_value textField
  potential <- liftM updated (mapDyn (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle) >>= holdDyn iPotential
  atomVal <- combineDyn (\val pot -> Atom val pot) inVal potential >>= combineDyn (\r con ->con r) repOrDiv
  groupVal <- combineDyn (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
  layerVal <- combineDyn (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
  let groupEvVal = tag (current groupVal) groupEv
  let layerEvVal = tag (current layerVal) layerEv
  genPat <- liftM joinDyn $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv]
  mapDyn (\x-> (x,upSig, never)) genPat
  where 
    attrs = fromList $ zip ["class","step","min","max"] ["atomPopupInput",show step, show minVal, show maxVal]
    popupActions = [MakeRepOrDiv::EditSignal Int, MakeGroup, MakeLayer, DeleteMe]
    popup = basicPopup liveness popupActions
    getIVal gP = case gP of 
      (Group (Live (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Group (Edited (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Atom v p r) -> (v,r,p)
      otherwise -> (defaultVal, Once,Inert)



popupDoubleWidget :: MonadWidget t m => Double -> Double -> Double -> Double -> Dynamic t Liveness -> GeneralPattern Double -> Event t (EditSignal (GeneralPattern Double)) -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal (GeneralPattern Double)), Event t Hint))
popupDoubleWidget defaultVal minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
  let (iVal,iRepDiv,iPotential) = getIVal iGenPat
  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number" & textInputConfig_attributes .~ (constDyn ("style"=:"width:30px; border: none;"))
  let iRepDivViewable = (iRepDiv/=Once)
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) (case iPotential of Inert->False; otherwise->True) $ updated popupToggle
  let popupEv = leftmost $ [ffilter id $ updated $ _textInput_hasFocus textField, closeEvent]  
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= combineDyn (\tog val-> if tog then val else Once) repDivToggle
  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupEv = fmap (\x->case x of (Just MakeGroup)->MakeGroup;otherwise->Close) $ ffilter (\x->case x of (Just MakeGroup)->True;otherwise->False) popupMenu
  let layerEv = fmap (\x->case x of (Just MakeLayer)->MakeLayer;otherwise->Close) $ ffilter (\x->case x of (Just MakeLayer)->True;otherwise->False) popupMenu
  popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost [(()<$) popupMenu,(()<$) livenessEv , (()<$) $ ffilter id $ updated $ _textInput_hasFocus textField,(() <$)closeEvent,(() <$) groupEv, (() <$) layerEv]
  inVal <- mapDyn (maybe iVal id . (readMaybe::String -> Maybe Double)) $ _textInput_value textField
  potential <- liftM updated (mapDyn (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle) >>= holdDyn iPotential
  atomVal <- combineDyn (\val pot -> Atom val pot) inVal potential >>= combineDyn (\r con ->con r) repOrDiv
  groupVal <- combineDyn (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
  layerVal <- combineDyn (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
  let groupEvVal = tag (current groupVal) groupEv
  let layerEvVal = tag (current layerVal) layerEv
  genPat <- liftM joinDyn $ holdDyn atomVal $ leftmost $ fmap (fmap constDyn) [groupEvVal,layerEvVal]
  let upSig = fmap toEditSigGenPat $ leftmost [deleteEv, livenessEv,(RebuildMe <$) groupEv, (RebuildMe <$) layerEv]
  mapDyn (\x-> (x,upSig, never)) genPat
  where 
    attrs = fromList $ zip ["class","step","min","max"] ["atomPopupInput",show step, show minVal, show maxVal]
    popupActions = [MakeRepOrDiv::EditSignal Double, MakeGroup, MakeLayer, DeleteMe]
    popup = basicPopup liveness popupActions
    getIVal gP = case gP of 
      (Group (Live (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Group (Edited (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
      (Atom v p r) -> (v,r,p)
      otherwise -> (defaultVal, Once,Inert)

--popupDoubleWidget :: MonadWidget t m => Double -> Double -> Double -> Double -> Dynamic t Liveness -> GeneralPattern Double -> Event t (EditSignal (GeneralPattern Double)) -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal (GeneralPattern Double)), Event t Hint))
--popupDoubleWidget defaultVal minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
--  let (iVal,iRepDiv,iPotential) = getIVal iGenPat
--  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
--  let iRepDivViewable = (and $ fmap (/=iRepDiv) [Rep 1, Div 1, Once])
--  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
--  inVal <- mapDyn (maybe iVal id . (readMaybe::String -> Maybe Double)) $ _textInput_value textField
--  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) (case iPotential of Inert->False; otherwise->True) $ updated popupDisplayEv
--  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
--  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= combineDyn (\tog val-> if tog then val else Once) repDivToggle
--  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
--  let groupLayerEv = fmap fromJust $ ffilter (\x-> case x of (Just MakeGroup)->True; (Just MakeLayer)->True; otherwise -> False ) popupMenu
--  popupToggle <- toggle (case iPotential of Inert->False; otherwise->True) $ leftmost [(()<$) popupMenu, (()<$) $ ffilter id $ updated $ _textInput_hasFocus textField,(() <$)closeEvent]
--  potential <- liftM updated (mapDyn (\x-> if x then Potentials (fmap toPotential popupActions) else Inert) popupToggle) >>= holdDyn iPotential
--  popupDisplayEv <- toggle (case iPotential of Inert->False; otherwise->True) $ updated potential
  
--  atomVal <- combineDyn (\val pot -> Atom val pot) inVal potential >>= combineDyn (\r con ->con r) repOrDiv
--  groupToggle <- toggle (isGroup iGenPat) $ ffilter (==MakeGroup) groupLayerEv
--  groupVal <- combineDyn (\v l -> if l==L4 then Group  (Live ([v],Once) L4) Inert  else Group (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
--  layerToggle <- toggle (isLayers iGenPat) $ ffilter (==MakeLayer) groupLayerEv
--  layerVal <- combineDyn (\v l -> if l==L4 then Layers (Live ([v],Once) L4) Inert  else Layers (Edited ([v],Once) ([v],Once)) Inert) atomVal liveness
--  genPat <- liftM joinDyn $ combineDyn (\gr lay-> if gr then groupVal else if lay then layerVal else atomVal) groupToggle layerToggle
--  let upEvent = leftmost [fmap (toEditSigGenPat . fromJust) $ ffilter (\x-> or [x==Just MakeL3, x==Just MakeL4, x==Just Eval, x==Just DeleteMe]) popupMenu, (RebuildMe <$) groupLayerEv]
--  mapDyn (\x-> (x,upEvent,never)) genPat
--  where 
--    attrs = fromList $ zip ["class","step","min","max","style"] ["atomPopupInput",show step, show minVal, show maxVal,"width:30px"]
--    popupActions = [MakeRepOrDiv::EditSignal Double, MakeGroup, MakeLayer, DeleteMe]
--    popup = basicPopup liveness popupActions
--    getIVal gP = case gP of 
--      (Group (Live (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
--      (Group (Edited (xs,r) _) p) -> ((\(x,_,_)->x) $ getIVal (xs!!0),r,p)
--      (Atom v p r) -> (v,r,p)
--      otherwise -> (defaultVal, Once,Inert)



------------------------------------------
--             UTILITY GENPAT'S         --
------------------------------------------

--Slider w/ a range and stepsize
sliderWidget::MonadWidget t m => (Double,Double)-> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a), Event t Hint))
sliderWidget (minVal,maxVal) stepsize iGenPat _ = do
  let iVal = getIVal iGenPat
  text $ show minVal
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range",show minVal,show maxVal,show stepsize,"width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text $ show maxVal
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Double) then Atom (read x::Double) Inert Once else Atom ((minVal+maxVal)/2) Inert Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton,never))
  where
    getIVal i = case i of
      (Atom a _ _) -> a
      (Group (Live (xs,_) _) _) -> getIVal $ head xs
      (Group (Edited (xs,_) _) _) -> getIVal $  head xs
      (Layers (Live (xs,_) _) _) -> getIVal $ head xs
      (Layers (Edited (xs,_) _) _) -> getIVal $ head xs
      (Blank _) -> minVal


-- A clickable td element. Each click cycles to the next element in the map. Updated with a RepOrDiv event.
-- rep/div values get shown on the button too.
clickListWidget::(MonadWidget t m, Show a, Eq a) => Map Int a ->  GeneralPattern a -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern a, Event t (EditSignal a)))
clickListWidget cycleMap iGenPat updatedReps = mdo
  let (iVal,iReps) = getIVal iGenPat
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVal) $ elems cycleMap
  sampleButton <- tdButtonAttrs' showVal (iVal) $ "class"=:"clickListtd"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length cycleMap)
  str'' <- mapDyn (\x-> maybe iVal id $ Data.Map.lookup x cycleMap) num
  let str' = updated str''
  str <- holdDyn (iVal) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x Inert r) str reps
  showVal <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample
  where
    getIVal i = case i of
      (Atom a _ r) -> (a,r)
      (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)

repDivWidget::MonadWidget t m => RepOrDiv -> m (Dynamic t RepOrDiv)
repDivWidget (Rep iVal) = elAttr "table" ("class"=:"repDivTable")$ mdo
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
repDivWidget (Div iVal) = elAttr "table" ("class"=:"repDivTable")$ mdo
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


-- countStepWidget (see ICOAH 'up' Widget for example)
-- step is the amount each click of the up/down arrows modify the value by
--   ---------------
--   --  0.0   ▲  --
--   --   -    ▼  --
--   ---------------
countStepWidget::MonadWidget t m => Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
--countStepWidget step (Atom iUpVal _) _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
countStepWidget step iGenPat _ = elAttr "table" ("class"=:"countWidgetTable") $ mdo
  let iUpVal = fst $ getIVal iGenPat
  upCount <- el "tr" $ do
    elAttr "td" ("class"=:"countWidgetTable") $ dynText upValShow
    upButton <- tdButtonAttrs "▲" () ("class"=:"countWidgetTable-upArrowtd") >>= count
    return upButton
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solid black"
    downButton <- tdButtonAttrs "▼" () ("class"=:"countWidgetTable-downArrowtd") >>= count
    return $ (deleteButton, downButton)
  upVal <- combineDyn (\a b ->  (a*step)-(b*step)+(iUpVal)) upCount downCount
  upValShow <- forDyn upVal show
  --repsHold <- holdDyn iUpVal $ updated repeats
  mapDyn (\x->(Atom x Inert Once,deleteEvent)) upVal
  where
    getIVal i = case i of
      (Atom a _ r) -> (a,r)
      (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)

-- widget with a slider returning a single Atom with range [minVal,maxVal] and stepsize specified
doubleSliderWidget::MonadWidget t m => (Double,Double) -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
doubleSliderWidget (minVal,maxVal) stepsize iGenPat _ = elAttr "table" ("class"=:"doubleSliderWidget") $ mdo
  let iVal = fst $ getIVal iGenPat
  slider <- el "tr" $ elAttr "td" (Data.Map.union ("colspan"=:"3") ("style"=:"text-align:left")) $ do
      let attrs = constDyn $ fromList $ zip ["min","max","step","style"] [show minVal,show maxVal, show stepsize,"width:100px"]
      rangeInput <- textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs & textInputConfig_setValue .~ sliderUpdateVal & textInputConfig_initialValue .~ (show iVal)
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
  mapDyn (\x-> (Atom x Inert Once,delEv)) slider
  where
  getIVal i = case i of
    (Atom a _ r) -> (a,r)
    (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)



-- < and > buttons, background color fill illustrateds value
-- see iclc fixed, end widget
--  -----------------
--  --  <   >   -  --
--  -----------------
faderButtonWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
faderButtonWidget iGenPat _ = elAttr "td" ("style"=:"text-align:center;margin:10px") $ mdo
  let iEnd = fst $ getIVal iGenPat
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
    val <- mapDyn (\x-> (Atom x Inert Once,delEv)) endVal
    return $ (val, tableAttrs)
  return returnVal
  where
  getIVal i = case i of
    (Atom a _ r) -> (a,r)
    (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)



charWidget'::MonadWidget t m => GeneralPattern Char-> Event t () -> m (Dynamic t (GeneralPattern Char,Event t (EditSignal a)))
charWidget' iGenPat _ = do
  let (iVal,reps) = getIVal iGenPat
  textField <-textInput $ def & textInputConfig_attributes .~ (constDyn $ fromList $ zip ["style", " maxlength"] ["width:40px", "1"]) & textInputConfig_initialValue .~ [iVal]
  let inputVal = _textInput_value textField
  inputChar <- mapDyn (\c-> if length c ==1 then c!!0 else  '~') inputVal
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\char rep-> if char=='~' then Blank Inert  else Atom char Inert rep) inputChar repeats'
  forDyn genPat (\k-> (k,deleteButton))
  where
  getIVal i = case i of
    (Atom a _ r) -> (a,r)
    (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)


-- used in charContainer, example in Vowel in ICLCStacked widget
charWidget::(MonadWidget t m) => Dynamic t Liveness -> GeneralPattern Char -> Event t (EditSignal (GeneralPattern Char)) -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal (GeneralPattern Char)),Event t Hint))
charWidget _ iGenPat _ = elAttr "table" ("class"=:"aGLStringWidgetTable") $ mdo
  let (iVal,iReps) = getIVal iGenPat
  genPat <- el "tr" $ do
    val <- el "td" $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("class"=:"aGLNumberWidgetTable-textFieldtd") & textInputConfig_initialValue .~ ([iVal])
      let val = _textInput_value inputField
      forDyn val (\x-> maybe (' ') id $ listToMaybe x)
    reps <- repDivWidget iReps
    combineDyn (\x y -> Atom x Inert y) val reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLStringWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLStringWidgetTable-layertd")
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ ("class"=:"aGLStringWidgetTable-deletetd")
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group (Live ([u],Once) L4) Inert else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a Inert x) -> if tog then Layers (Live ([u],Once) L4) Inert else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x-> (x,leftmost [rebuildEvent, deleteEvent],never)) genPat''
  where
  getIVal i = case i of
    (Atom a _ r) -> (a,r)
    (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
    (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)

--charWidget''::MonadWidget t m => GeneralPattern Char-> Event t () -> m (Dynamic t (GeneralPattern Char,Event t (EditSignal a)))
--charWidget'' iGenPat _ = do
--  let (iVal,reps) = getIVal iGenPat
--  textField <-textInput $ def & textInputConfig_attributes .~ (constDyn $ fromList $ zip ["style", " maxlength"] ["width:40px", "1"]) & textInputConfig_initialValue .~ [iVal]
--  let inputVal = _textInput_value textField
--  inputChar <- mapDyn (\c-> if length c ==1 then c!!0 else  '~') inputVal
--  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
--  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
--  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
--  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
--  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
--  genPat <- combineDyn (\char rep-> if char=='~' then Blank Inert  else Atom char Inert rep) inputChar repeats'
--  forDyn genPat (\k-> (k,deleteButton))
--  where
--  getIVal i = case i of
--    (Atom a _ r) -> (a,r)
--    (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
--    (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
--    (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
--    (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)


intWidget::MonadWidget t m => GeneralPattern Int-> Event t () -> m (Dynamic t (GeneralPattern Int,Event t (EditSignal a)))
intWidget iVal _ = do
  let attrs = def & textInputConfig_attributes .~ constDyn ("style"=:"width:20px;") & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
  textField <-textInput attrs
  let inputVal = _textInput_value textField
  plusButton <- buttonDynAttrs "▲" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  minusButton <- buttonDynAttrs "▼" () (constDyn ("style"=:"width:20px;text-align:center")) >>= count
  deleteButton <- buttonDynAttrs "-" (DeleteMe) $ constDyn ("style"=:"width:20px")
  repeats <- combineDyn (\a b ->(a+1-b)) plusButton minusButton
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div (abs (k-2)))
  genPat <- combineDyn (\str rep-> if isJust (readMaybe str::Maybe Int) then Atom (read str::Int) Inert rep else Atom 0 Inert rep ) inputVal repeats'
  forDyn genPat (\k-> (k,deleteButton))


-----------------------------------------------
--       MORE CONTEXT-SPECIFIC WIDGETS...    -- (ie. inteded to be used for 'crush' vs. generally applicable to 'int')
-----------------------------------------------

crushWidget::MonadWidget t m => GeneralPattern Int -> Event t () -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal a)))
crushWidget iVal _ = do
  text "0"
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range","0","16","1","width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text "16"
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Int) then Atom (read x::Int) Inert Once else Atom 16 Inert Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))


---- uses clickListWidget as a base widget, intersperses with + buttons
sampleNameWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a)))
sampleNameWidget iGenPat _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" tableAttrs $ mdo
  let (iSamp,iReps) = getIVal iGenPat
  (sample,upCount) <- elAttr "tr" (empty)$ do
    (samp,_) <- clickListWidget (fromList $ zip [(1::Int)..] ["~","bd","cp","bassfoo","moog", "arpy"]) (Atom iSamp Inert iReps) repeatsEv >>= splitDyn
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue;border: 1pt solid black") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solid black"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount'<-mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repInitial <- holdDyn iReps $ updated repeats'
  let repeatsEv = updated repInitial
  mapDyn (\x->(x,deleteEvent)) sample
  where 
    tableAttrs=("style"=:"display:inline-table;background-color:lightgreen;width:100pt;border-spacing:5px;border: 3pt solid black")
    getIVal i = case i of
      (Atom a _ r) -> (a,r)
      (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
sampleNameWidget _ e = sampleNameWidget (Atom "~" Inert Once) e




---- Eldad's Widgets:
sButtonContainer::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a), Event t Hint))
sButtonContainer iGenPat _ = elAttr "table" tableAttrs $ mdo
  let (iSamp,iReps) = getIVal iGenPat
  (sample,upCount) <- el "tr" $ do
    samp <- sButtonWidget iSamp repeats''
    upButton <- tdButtonAttrs "▲" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (samp,upButton)
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue"
    downButton <- tdButtonAttrs "▼" () ("style"=:"text-align:center;background-color:lightblue") >>= count
    return $ (deleteButton, downButton)
  downCount' <- mapDyn (\x-> case iReps of (Div i) -> x+i-1; Rep i->x-i+1; otherwise -> x) downCount
  repeats <- combineDyn (\a b ->a-b+1) upCount downCount'
  repeats' <- forDyn repeats (\k->if k>0 then Rep k else Div $ abs (k-2))
  repeats'' <- holdDyn iReps $ updated repeats'
  let hints = fmap (SampleHint) $ updated sample
  combineDyn (\x y -> (Atom x Inert y,deleteEvent,hints)) sample repeats''
  where 
    tableAttrs=("style"=:"margin:5px;display:inline-table;background-color:lightgreen;width:10%;padding:6px;border-spacing:5px;border: 3pt solid black")
    getIVal i = case i of
      (Atom a _ r) -> (a,r)
      (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
      (Blank p) -> ("~",Once)
sButtonContainer _ e = sButtonContainer (Atom "~" Inert Once) e

sButtonWidget::MonadWidget t m => SampleName -> Dynamic t RepOrDiv -> m (Dynamic t SampleName)
sButtonWidget iSamp reps = mdo
  let sMap = fromList $ zip [(0::Int)..] ["~","bd","sn","cp","hh","arpy","glitch","tabla"]
  let iNum = maybe (0::Int) id $ Data.List.findIndex (==iSamp) $ elems sMap
  sampleButton <- tdButtonAttrs' (showSample) (iSamp) $ "style"=:"width:60%;text-align:center;background-color:lightblue"
  num <- count sampleButton >>= mapDyn (\x-> (x+iNum) `mod` length sMap)
  sName <- mapDyn (\x-> maybe ("~") id $ Data.Map.lookup x sMap) num
  atom <- combineDyn (\v r ->Atom v Inert r) sName reps
  showSample <- mapDyn (show) atom
  return sName

-- returns atom of a character
vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal a)))
vowelButtonWidget iGenPat _ = elAttr "td" ("style"=:"text-align:center") $ elAttr "table" ("style"=:("width:100px;border-spacing:5px;display:inline-table;background-color:lightgreen;border:1pt solid black")) $ mdo
  let (iVowel,_) = getIVal iGenPat
  let vowMap = fromList $ zip [0::Int,1..] ['X','a','e','i','o','u']  -- Map Int (String,String)
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVowel) $ elems vowMap
  vowelButton <- tdButtonAttrs' showVowel iVowel $ "style"=:"width:50px;text-align:center;background-color:lightblue;border:1pt solid black"
  deleteButton <- tdButtonAttrs (" - ") (DeleteMe) $ "style"=:"width: 50px; text-align:center;background-color:lightblue;border:1pt solid black"
  num <- count vowelButton >>= mapDyn (\x-> (x+initialNum) `mod` length vowMap)
  char'' <- mapDyn (\x-> maybe ('X') id $ Data.Map.lookup x vowMap) num
  let char' = updated char''
  char <- holdDyn (iVowel) char'
  vowel <- mapDyn (\x -> Atom x Inert Once) char
  showVowel <- mapDyn show vowel
  mapDyn (\x->(x,deleteButton)) vowel
  where
      getIVal i = case i of
        (Atom a _ r) -> (a,r)
        (Group (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
        (Group (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
        (Layers (Live (xs,r) _) _) -> (fst $ getIVal $ head xs,r)
        (Layers (Edited (xs,r) _) _) -> (fst $ getIVal $ head xs,r)



-- For faderButtonWidget - gradient string used to show the
makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100
