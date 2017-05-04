{-# LANGUAGE RecursiveDo #-}
module Estuary.Widgets.GeneralPattern
where

import Reflex
import Reflex.Dom
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


generalContainer :: (MonadWidget t m, Eq a, Show a) => (GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)))))-> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))
generalContainer b i _ = elClass "div" (getClass i) $ mdo
  let cEvents = mergeWith (union) [insertMap,deleteMap]
  (allValues,events) <- eitherContainer (initialMap i) cEvents livenessEvMap livenessEvMap (leftBuilder) (rightBuilder liveness)
  values <- mapDyn (fst . Data.Either.partitionEithers . elems) allValues
  childKeys <- mapDyn keys allValues
  let events' = fmap (Data.Map.elems) events -- Event [l]
  let livenessEv = fmap (\x-> if Data.List.elem MakeL3 x then MakeL3 else MakeL4) $ ffilter (\x-> Data.List.elem MakeL3 x || Data.List.elem MakeL4 x) events' -- If any child reports a liveness change
  let livenessEvMap = attachDynWith (\k v -> fromList $ zip k $ repeat v) childKeys livenessEv
  liveness <- holdDyn MakeL4 livenessEv >>= mapDyn (\x-> case x of MakeL4 -> L4; otherwise -> L3)
  let evalEv = (Eval <$) $ ffilter (\x-> Data.List.elem Eval x) events' -- If any child reports a change

  -- When made to L3 or L4, or when Eval is pressed, reset the 'unchanged' value
  unchangedVal <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) $ tagDyn values $ leftmost [evalEv, livenessEv]
  let isEdited = attachWithMaybe (\x y-> if x==y then Nothing else Just y) (current unchangedVal) (updated values)
  isEdited' <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) isEdited
  isEdited'' <- combineDyn (\live updatedVal-> if live==L4 then Just updatedVal else Nothing) liveness isEdited'
  isEdited''' <- combineDyn (\maybeUpdated oldVal-> maybe oldVal id maybeUpdated) isEdited'' unchangedVal
  changes <- holdDyn False $ attachDynWith (==) unchangedVal isEdited

  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
  let insertMap = fmap (fromList . concat . (insertList i) . keys . Data.Map.filter (isChangeValue) )  events
  mapDyn (\x->returnF i x livenessEv) isEdited'''
  where
    initialVal (Atom iV _) = iV
    initialVal (Group iV _) = initialVal $ iV!!0
    initialVal (Layers iV _) = initialVal $ iV!!0
    getClass (Layers _ _) = "generalPattern-layer"
    getClass (Group _ _) = "generalPattern-group"
    getClass (Atom _ _) = "generalPattern-atom"
    initialMap (Layers xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    leftBuilder = aGLWidget b
    rightBuilder live= whitespace live (i) "whiteSpaceAdd" [ChangeValue (i)]
    insertList (Atom iVal _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) Once))])
    insertList (Layers xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    returnF (Layers _ _) x e = (Layers x Once,e)
    returnF (Group _ _) x e = (Group x Once,e)
    returnF (Atom _ _) x e = (Group x Once,e)


aGLWidget::(MonadWidget t m, Eq a, Show a) => (GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))) -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))
aGLWidget builder iVal ev = mdo
  val <- resettableWidget (function) iVal ev rebuildEvent'
  widgetEvents <- forDyn val (\(x,y)->y)
  rebuildEvent <- forDyn widgetEvents (\x-> ffilter (==RebuildMe) x)
  let rebuildEvent' = attachDynWith (\(value,_) _ ->value) val $ switchPromptlyDyn rebuildEvent
  return val
  where
    function (Atom x r) e = builder (Atom x r) e
    function (Blank) e = builder (Blank) e
    function (Group xs r) e = generalContainer builder (Group xs r) e
    function (Layers xs r) e = generalContainer builder (Layers xs r) e


-- When ',' happens, need to clobber the existing eithercontainer  and replace it with two
-- generalContainerLive widgets - one for the left side, one for the right side.

generalContainerLive :: (MonadWidget t m, Eq a, Show a) 
  => (Dynamic t Context -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a)))))
  -> GeneralPattern a 
  -> Event t (EditSignal (GeneralPattern a)) 
  -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))
generalContainerLive b i _ = elClass "div" (getClass i) $ mdo
  text (case i of (Group _ _)-> "["; (Layers _ _)-> "["; otherwise -> "")
  let cEvents = mergeWith (union) [insertMap,deleteMap]

  -- trying to have eithercontainer to be a widgethold,
  --let splitBuilder = attachDynWith (\vals split-> maybe (return $ constDyn (Blank,never)) (\int-> do 
  --                                                                                                  left <- generalContainerLive b (Group (take int vals) Once) never 
  --                                                                                                  right <- generalContainerLive b (Group (reverse $ take ((length vals) - int) (reverse vals)) Once) never
  --                                                                                                  combineDyn (\(leftV,leftEv) (rightV,rightEv) -> (Layers [leftV,rightV] Once,leftmost [leftEv,rightEv])) left right
  --                                                                                                ) split ) values splitEv


--Dynamic t (Dynamic t (Map Int (Either (GeneralPattern a) ())), Event t (Map Int (EditSignal (GeneralPattern a))))

--flippableWidget :: MonadWidget t m => m a -> m a -> Bool -> Event t Bool -> m (Dynamic t a)
  --eithercontainer:: m ( (Dynamic t (Map k (Either v a))) , Event t (Map k e) )
--a = ( dynamic (map k (either v a), event t map)
  --m dynamic (dynamic map, event map)
--m dynamic (

--inter:: m (dynamic t (  dynamic (map k (either v a))  ,  event t map ))

  inter <- flippableWidget (eitherContainer (initialMap i) cEvents livenessEvMap livenessEvMap (leftBuilder liveness) (rightBuilder liveness)) (return $ (constDyn empty,never)) False $ updated splitTog
  allValues <- liftM joinDyn $ mapDyn fst inter 
  events <- liftM switchPromptlyDyn $ mapDyn snd inter 
  --let eventKey = fmap keys events

  

  let deleteMap = fmap (fromList . concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)]) . keys . Data.Map.filter (==DeleteMe)) events
  let insertMap = fmap (fromList . concat . (insertList i) . keys . Data.Map.filter (isChangeValue) )  events

  values <- mapDyn (fst . Data.Either.partitionEithers . elems) allValues
  childKeys <- mapDyn keys allValues
  let events' = fmap (Data.Map.elems) events -- Event [l]


  let livenessEv = fmap (\x-> if Data.List.elem MakeL3 x then MakeL3 else MakeL4) $ ffilter (\x-> Data.List.elem MakeL3 x || Data.List.elem MakeL4 x) events' -- If any child reports a liveness change
  let livenessEvMap = attachDynWith (\k v -> fromList $ zip k $ repeat v) childKeys livenessEv
  liveness <- holdDyn MakeL4 livenessEv >>= mapDyn (\x-> case x of MakeL4 -> L4; otherwise -> L3)
  let evalEv = (Eval <$) $ ffilter (\x-> Data.List.elem Eval x) events' -- If any child reports a change 
  let deleteContainerEv = (DeleteMe <$) $ ffilter (\x -> Data.List.elem DeleteContainer x) events'

  -- When made to L3 or L4, or when Eval is pressed, reset the 'unchanged' value
  unchangedVal <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) $ tagDyn values $ leftmost [evalEv, livenessEv]
  let isEdited = attachWithMaybe (\x y-> if x==y then Nothing else Just y) (current unchangedVal) (updated values)
  isEdited' <- holdDyn (fst . Data.Either.partitionEithers . elems $ initialMap i) isEdited
  isEdited'' <- combineDyn (\live updatedVal-> if live==L4 then Just updatedVal else Nothing) liveness isEdited'
  isEdited''' <- combineDyn (\maybeUpdated oldVal-> maybe oldVal id maybeUpdated) isEdited'' unchangedVal -- Dyn [GenPat]
  
  --let splitEv = fmap (maybe 0 id . elemIndex LayerSplit) $ ffilter (Data.List.elem LayerSplit) events'
  --let splitEv = coincidence $ fmap (const $ fmap (fromJust . elemIndex LayerSplit) events') $ ffilter (Data.List.elem LayerSplit) events'
  
  --let splitEv = coincidence $ fmap (head . keys) $ ffilter (Data.List.elem LayerSplit) $ fmap Data.Map.elems events

  let splitEv = coincidence $ fmap (const $ fmap (head . keys . Data.Map.filter (==LayerSplit)) events) $ ffilter (Data.List.elem LayerSplit) events'
  holdDyn 0 splitEv >>= mapDyn show >>= dynText
  --let splitEv = fmap (head . keys . Data.Map.filter (==LayerSplit)) events
  -- Event [EditSignal]

  --holdDyn "not yet" (fmap (\x->"split thingy : "++ (show x)) (ffilter (Data.List.elem LayerSplit) events') ) >>= dynText

  --[ ]   attachPromptlyDynWith      :: (a -> b ->       c) ->  Dynamic a -> Event b -> Event c
  let splitVal = attachDynWith (\vals index-> Layers [Group (take (index+1) vals) Once, Group (reverse $ take ((length vals) - (index+1)) (reverse vals)) Once] Once) isEdited''' splitEv
  v <- holdDyn Blank splitVal >>= combineDyn (\i e-> case e of Blank -> i; otherwise -> [e]) isEdited'''
  
  --splitVal <- holdDyn Nothing $ fmap (elemIndex LayerSplit) splitEv
  --widgetHold :: m a ->   Event (m a) -> m (Dynamic a)
  --attachDynWith (\vals split-> maybe (return $ constDyn (Blank,never)) (\int-> do 
  --  (leftV,leftEv) <- generalContainerLive b (take split vals) never 
  --  (rightV, rightEv) <- generalContainerLive b (reverse $ take ((length vals) - split) (reverse vals)) never
  --  combineDyn (\(leftV,leftEv) (rightV,rightEv) -> (Layers [leftV,rightv] Once,leftmost [leftEv,rightEv]))
  --    ) split 
  --    ) values splitEv
  --left <- widgetHold (return $ constDyn (Blank,never)) $ 
  --right <- widgetHold (return $ constDyn (Blank,never)) $ 
  --genPat <- combineDyn (\val split-> if isJust split then Layers (maybe val (splitFunc val) split) Once else Group (maybe val (splitFunc val) split) Once) isEdited''' splitVal
  --genPat <- combineDyn (\val split-> maybe (Group val Once) (splitFunc val) split) isEdited''' splitVal

  --trying to have eithercontainer to be a widgethold,
  --let splitBuilder = attachDynWith (\vals split-> maybe (return $ constDyn (Blank,never)) (\int-> do 
  --                                                                                                  left <- generalContainerLive b (Group (take int vals) Once) never 
  --                                                                                                  right <- generalContainerLive b (Group (reverse $ take ((length vals) - int) (reverse vals)) Once) never
  --                                                                                                  combineDyn (\(leftV,leftEv) (rightV,rightEv) -> (Layers [leftV,rightV] Once,leftmost [leftEv,rightEv])) left right
  --                                                                                                ) split ) values splitEv
  -- Problem now is: vals becomes just [] when 

  --text ("split list:  " ++ (show $ take (div split 2) vals))
  --                                                  text ("split list or:  " ++ (show $ take (split-1) vals))

-- Split is caluclating exacly where in the list of + and other widgets to inser the comma, but the 'vals' thing is just concerning the Left widgets

  let splitBuilder = attachDynWith (\vals split-> do 
                                                    let vals' = elems vals
                                                    let leftList = (\x-> if x==[] then [Blank] else x) $ fst . partitionEithers $ take split vals'
                                                    let rightList = (\x-> if x==[] then [Blank] else x) $ fst . partitionEithers . reverse $ take ((length vals') - split) $ reverse vals'
                                                    left <- generalContainerLive b (Group leftList Once) never 
                                                    text ", "
                                                    right <- generalContainerLive b (Group rightList Once) never
                                                    combineDyn (\(leftV,leftEv) (rightV,rightEv) -> (Layers [leftV,rightV] Once,leftmost [leftEv,rightEv])) left right
                                                    ) allValues splitEv

  splitTog <- toggle False splitBuilder
  changes <- holdDyn False $ attachDynWith (==) unchangedVal isEdited
  --mapDyn (\x->returnF i x (leftmost [livenessEv, deleteContainerEv, (RebuildMe <$) $ ffilter (Data.List.elem LayerSplit) events'])) isEdited'''

    

  regVal <-  mapDyn (\x->returnF i x (leftmost [livenessEv, deleteContainerEv, (RebuildMe <$) $ ffilter (Data.List.elem LayerSplit) events'])) v
  returnVal <-liftM joinDyn $ widgetHold (return regVal) splitBuilder
  text (case i of (Group _ _)-> "]"; (Layers _ _)-> "]"; otherwise -> "")
  return returnVal
  --mapDyn (\x-> (x,(leftmost [livenessEv, deleteContainerEv, (RebuildMe <$) $ ffilter (Data.List.elem LayerSplit) events']))) genPat
  where
    initialVal (Atom iV r) = Atom iV r
    initialVal (Group iV _) = initialVal $ iV!!0
    initialVal (Layers iV _) = initialVal $ iV!!0
    initialVal (Blank) = Blank
    getClass (Layers _ _) = "generalPattern-layer"
    getClass (Group _ _) = "generalPattern-group"
    getClass (Atom _ _) = "generalPattern-atom"
    getClass Blank = "generalPattern-blank"
    initialMap (Layers xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Group xs iReps) = fromList $ zip [(0::Int)..] $ [Right ()] ++ (intersperse (Right ()) $ fmap Left xs) ++ [Right ()]
    initialMap (Atom iVal iReps) = fromList $ zip [0::Int,1,2] [Right (),Left $ Atom iVal iReps, Right ()]
    --initialMap (Blank) = 
    leftBuilder live = aGLWidgetLive live b
    rightBuilder live= whitespace live (initialVal i) "whiteSpaceAdd" [ChangeValue (initialVal i),DeleteContainer]
    insertList (Atom iVal _) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ Atom (iVal) Once))])
    insertList (Layers xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    insertList (Group xs iReps) = Prelude.map (\k -> [(k,Insert (Right ())),(k+1,Insert (Left $ xs!!0))])
    returnF (Layers _ _) x e = (Layers x Once,e)
    returnF (Group _ _) x e = (Group x Once,e)
    returnF (Atom _ _) x e = (Group x Once,e)
    splitFunc xs index = Layers [Group (take index xs) Once, Group (reverse $ take ((length xs)-index) (reverse xs)) Once] Once


    --splitFunc (Atom x r) index = Group [Layers [x] Once] Once
    --splitFunc (Group xs r) index = Group [Layers (take index xs) Once, Layers (reverse . take ((length index)-index) (reverse xs))] Once
    --splitFunc (Layers xs r) index = Layers xs r



aGLWidgetLive::(MonadWidget t m, Eq a, Show a) => Dynamic t Context -> (Dynamic t Context -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))) -> GeneralPattern a -> Event t (EditSignal (GeneralPattern a)) -> m (Dynamic t (GeneralPattern a, Event t (EditSignal (GeneralPattern a))))
aGLWidgetLive liveness builder iVal ev = mdo
  -- resettableWidget :: MonadWidget t m => (a -> Event t b -> m (Dynamic t c)) -> a -> Event t b -> Event t a -> m (Dynamic t c)
  val <- resettableWidget (function) iVal ev rebuildEvent'
  widgetEvents <- forDyn val (\(x,y)->y)
  rebuildEvent <- forDyn widgetEvents (\x-> ffilter (==RebuildMe) x)
  let rebuildEvent' = attachDynWith (\(value,_) _ ->value) val $ switchPromptlyDyn rebuildEvent
  return val
  where
    function (Atom x r) e = builder liveness (Atom x r) e
    function (Blank) e = builder liveness (Blank) e
    function (Group xs r) e = generalContainerLive (builder) (Group xs r) e
    function (Layers xs r) e = generalContainerLive  (builder) (Layers xs r) e


popupSampleWidget :: MonadWidget t m => Dynamic t Context -> GeneralPattern String -> Event t (EditSignal (GeneralPattern String)) -> m (Dynamic t (GeneralPattern String, Event t (EditSignal (GeneralPattern String))))
popupSampleWidget liveness iVal e = elAttr "div" (singleton "style" "border: 1px solid black; position: relative; display: inline-block;") $ mdo
  sample <- clickableDivClass'' inVal "noClass" ()
  repDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) popup False (updated popupEvents')
  let closeEvents = (() <$)  $ ffilter (==Nothing)  popupMenu
  let deleteEv = fmap fromJust  $ ffilter (==Just DeleteMe) popupMenu
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repDivVal <- holdDyn iRepDiv repDivEv >>= combineDyn (\tog val -> if tog then val else Once) repDivToggle
  let livenessEv = fmap fromJust $ ffilter (\x-> x==Just MakeL3 || x==Just MakeL4|| x==Just Eval) popupMenu
  let groupLayerEv = fmap fromJust $ ffilter (\x-> case x of (Just MakeGroup)->True; (Just MakeLayer)->True; otherwise -> False ) popupMenu
  let sampleChanges = fmap (\x-> maybe Blank (\y-> case y of (ChangeValue z)-> z; otherwise -> Blank) x) $ ffilter (\x-> maybe False (isChangeValue) x) popupMenu -- Event t (GeneralPat)
  popupEvents' <- toggle False $ leftmost $ [(() <$) sample,(() <$) groupLayerEv,(() <$) closeEvents,(() <$) livenessEv, (() <$) sampleChanges]
  inVal <- holdDyn iSamp $ fmap show sampleChanges
  genPatType <- holdDyn (Atom) $ fmap (\x-> (case x of MakeGroup -> Group; otherwise -> Layers) . (take 1 . repeat . (flip Atom) Once) )  groupLayerEv -- Dyn ()
  genPat <- combineDyn (\constructor v -> constructor v) genPatType inVal 
  genPat' <- combineDyn (\g r -> g r) genPat repDivVal
  let upEvent = leftmost [fmap fromJust $ ffilter (\x-> or [x==Just MakeL3, x==Just MakeL4, x==Just Eval, x==Just DeleteMe]) popupMenu, (RebuildMe <$) groupLayerEv]
  mapDyn (\x-> (x,upEvent)) genPat'
  where
    popupMap = [MakeRepOrDiv, MakeGroup, MakeLayer, DeleteMe]
    sampleMap = fromList $ zip [0::Int,1..] $ [("Rest","~"),("Percussion", "bd"),("Percussion", "cp"),("Percussion", "hh"),("Percussion", "sn"),("Bass","jvbass"), ("Bass","wobble"),("Bass","bass1"),("Pitched","arpy"), ("Pitched", "casio"), ("Pitched","latibro")]
    popup = samplePickerPopup liveness sampleMap popupMap   
    iRepDivViewable = (and $ fmap (/=iRepDiv) [Rep 1, Div 1, Once]) 
    (iSamp,iRepDiv) = case iVal of
                    (Group xs r) -> (show $ xs!!0,r)
                    (Layers xs r) -> (show $ xs!!0,r)
                    (Atom v r) -> (v,r)
                    otherwise -> ("~",Once)


popupIntWidget :: MonadWidget t m => Int -> Int -> Int -> Dynamic t Context -> GeneralPattern Int -> Event t (EditSignal (GeneralPattern Int)) -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal (GeneralPattern Int))))
popupIntWidget minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
  let iVal = getIVal iGenPat
  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
  --repOrDiv <- liftM joinDyn $ flippableWidget (return $ constDyn Once) (repDivWidget'' iRepDiv never) iRepDivViewable $ never  --@ this would be cleaner, not sure why it doesn't work...
  let iRepDivViewable = (and $ fmap (/=iRepDiv) [Rep 1, Div 1, Once])
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  inVal <- mapDyn (maybe iVal id . (readMaybe::String -> Maybe Int)) $ _textInput_value textField
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) False popupDisplayEv
  let popupDisplayEv = leftmost $ [ffilter id $ updated $ _textInput_hasFocus textField, closeEvent]  
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= combineDyn (\tog val-> if tog then val else Once) repDivToggle
  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
  let groupLayerEv = fmap fromJust $ ffilter (\x-> case x of (Just MakeGroup)->True; (Just MakeLayer)->True; otherwise -> False ) popupMenu
  genPatType <- holdDyn (Atom) $ fmap (\x-> (case x of MakeGroup -> Group; otherwise -> Layers) . (take 1 . repeat . (flip Atom) Once) )  groupLayerEv -- Dyn ()
  genPat <- combineDyn (\constructor v -> constructor v) genPatType inVal 
  genPat' <- combineDyn (\g r -> g r) genPat $ constDyn Once
  let upEvent = leftmost [fmap fromJust $ ffilter (\x-> or [x==Just MakeL3, x==Just MakeL4, x==Just Eval, x==Just DeleteMe]) popupMenu, (RebuildMe <$) groupLayerEv]
  mapDyn (\x-> (x,upEvent)) genPat'
  where 
    attrs = fromList $ zip ["class","step","min","max"] ["atomPopupInput",show step, show minVal, show maxVal]
    popup = basicPopup liveness [MakeRepOrDiv::EditSignal (GeneralPattern Int), MakeGroup, MakeLayer, DeleteMe]
    getIVal gP = case gP of 
      (Group xs r) -> getIVal (xs!!0)
      (Layers xs r) -> getIVal (xs!!0)
      (Atom x r) -> x
      otherwise -> (minVal)
    (iRepDiv) = case iGenPat of 
      (Group _ r) ->  r
      (Layers _ r) -> r
      (Atom _ r) ->   r
      otherwise ->    Once


popupDoubleWidget :: MonadWidget t m => Double -> Double -> Double -> Dynamic t Context -> GeneralPattern Double -> Event t (EditSignal (GeneralPattern Double)) -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal (GeneralPattern Double))))
popupDoubleWidget minVal maxVal step liveness iGenPat editEv = elClass "div" "atomPopup" $ mdo
  let iVal = getIVal iGenPat
  textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
  --repOrDiv <- liftM joinDyn $ flippableWidget (return $ constDyn Once) (repDivWidget'' iRepDiv never) iRepDivViewable $ never  --@ this would be cleaner, not sure why it doesn't work...
  let iRepDivViewable = (and $ fmap (/=iRepDiv) [Rep 1, Div 1, Once])
  repOrDivEv <- liftM switchPromptlyDyn $ flippableWidget (return never) (repDivWidget' iRepDiv never) iRepDivViewable $ updated repDivToggle
  inVal <- mapDyn (maybe iVal id . (readMaybe::String -> Maybe Double)) $ _textInput_value textField
  popupMenu <- liftM (switchPromptlyDyn) $ flippableWidget (return never) (popup) False popupDisplayEv
  let popupDisplayEv = leftmost $ [ffilter id $ updated $ _textInput_hasFocus textField, closeEvent]  
  repDivToggle <- toggle iRepDivViewable $ ffilter (== Just MakeRepOrDiv) popupMenu
  repOrDiv <- holdDyn iRepDiv repOrDivEv >>= combineDyn (\tog val-> if tog then val else Once) repDivToggle
  let closeEvent = (False <$)  $ ffilter (isNothing)  popupMenu
  let groupLayerEv = fmap fromJust $ ffilter (\x-> case x of (Just MakeGroup)->True; (Just MakeLayer)->True; otherwise -> False ) popupMenu
  genPatType <- holdDyn (Atom) $ fmap (\x-> (case x of MakeGroup -> Group; otherwise -> Layers) . (take 1 . repeat . (flip Atom) Once) )  groupLayerEv -- Dyn ()
  genPat <- combineDyn (\constructor v -> constructor v) genPatType inVal 
  genPat' <- combineDyn (\g r -> g r) genPat $ constDyn Once
  let upEvent = leftmost [fmap fromJust $ ffilter (\x-> or [x==Just MakeL3, x==Just MakeL4, x==Just Eval, x==Just DeleteMe]) popupMenu, (RebuildMe <$) groupLayerEv]
  mapDyn (\x-> (x,upEvent)) genPat'
  where 
    attrs = fromList $ zip ["class","step","min","max"] ["atomPopupInput",show step, show minVal, show maxVal]
    popup = basicPopup liveness [MakeRepOrDiv::EditSignal (GeneralPattern Double), MakeGroup, MakeLayer, DeleteMe]
    getIVal gP = case gP of 
      (Group xs r) -> getIVal (xs!!0)
      (Layers xs r) -> getIVal (xs!!0)
      (Atom x r) -> x
      otherwise -> (minVal+maxVal)/2
    (iRepDiv) = case iGenPat of 
      (Group _ r) ->  r
      (Layers _ r) -> r
      (Atom _ r) ->   r
      otherwise ->    Once




------------------------
----  GenPat Double   --
------------------------
-- A groupable/layerable/atomizable widget for General Pattern Doubles
-- vMin and vMax denote the rane of possible values, step = the stepsize of each increment


aGLDoubleWidget::(MonadWidget t m) => Double -> Double -> Double -> GeneralPattern Double -> Event t (EditSignal (GeneralPattern Double)) -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal (GeneralPattern Double))))
aGLDoubleWidget vMin vMax step (Atom iVal _) _ = elAttr "table" ("class"=:"aGLNumberWidgetTable") $ mdo
  (genPat,deleteEvent) <- el "tr" $ do
    genPat <- elAttr "td" ("class"=:"aGLNumberWidgetTable-textFieldtd") $ do
      let attrs = fromList $ zip ["style","step","min","max"] ["width=40px",show step, show vMin, show vMax]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      inVal' <- mapDyn (\str-> if isJust (readMaybe str::Maybe Double) then (read str::Double) else (vMax-vMin)/2+vMin) inVal
      mapDyn (\x->Atom (max vMin $ min vMax x) Once) inVal'
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ ("class"=:"aGLNumberWidgetTable-deletetd")
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLNumberWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLNumberWidgetTable-layertd")
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) genPat''
aGLDoubleWidget vMin vMax step _ e = aGLDoubleWidget vMin vMax step (Atom 0 Once) e

----------------------
--  GenPat Ints     --
----------------------
-- A groupable/layerable/atomizable widget for General Pattern Ints
-- vMin and vMax denote the rane of possible values, step = the stepsize of each increment

aGLIntWidget::(MonadWidget t m) => Int -> Int -> Int -> GeneralPattern Int -> Event t (EditSignal (GeneralPattern Int)) -> m (Dynamic t (GeneralPattern Int, Event t (EditSignal (GeneralPattern Int))))
aGLIntWidget vMin vMax step (Atom iVal _) _ = elAttr "table" ("class"=:"aGLNumberWidgetTable") $ mdo
  (genPat,deleteEvent) <- elAttr "tr" (empty) $ do
    genPat <- elAttr "td" ("class"=:"aGLNumberWidgetTable-textFieldtd") $ do
      let attrs = fromList $ zip ["style","step","min","max"] ["width:40px",show step, show vMin, show vMax]
      textField <- textInput $ def & textInputConfig_attributes .~ constDyn attrs & textInputConfig_initialValue .~ (show iVal) & textInputConfig_inputType .~"number"
      let inVal = _textInput_value textField
      inVal' <- mapDyn (\str-> if isJust (readMaybe str::Maybe Int) then (read str::Int) else 0) inVal
      mapDyn (\x->Atom (max vMin $ min vMax x) Once) inVal'
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "class"=:"aGLNumberWidgetTable-deletetd"
    return (genPat, deleteButton)
  (layerEvent, groupEvent) <- el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLNumberWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLNumberWidgetTable-layertd")
    return $ (layerButton, groupButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x->(x,leftmost [rebuildEvent, deleteEvent])) genPat''
aGLIntWidget vMin vMax step _ e = aGLIntWidget vMin vMax step (Atom 0 Once) e



----------------------
--  GenPat Strings  --
----------------------
-- The following 3 widget functions compose of a container which interpserses aGLStringWidgets
-- (something that returns an Atom, Group or Layer (aGL)) with + buttons. The widgets can be
-- recursively constructed: each individual widget can be turned into a container itself (as a group or layer).

-- Individual string widgets (able to turn into a container themselves by signaling their container in the returned event)
aGLStringWidget::(MonadWidget t m) => GeneralPattern String -> Event t (EditSignal (GeneralPattern String)) -> m (Dynamic t (GeneralPattern String, Event t (EditSignal (GeneralPattern String))))
--aGLStringWidget (Atom iVal iReps) _ = elAttr "div" ("style"=:"display:inline-block;") $ elAttr "table" tableAttrs $ mdo
aGLStringWidget (Atom iVal iReps) _ = elAttr "table" ("class"=:"aGLStringWidgetTable") $ mdo
  genPat <- el "tr" $ do
    val <- elAttr "td" ("class"=:"aGLStringWidgetTable-textFieldtd") $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("style"=:"width:60px") & textInputConfig_initialValue .~ (iVal)
      let val = _textInput_value inputField
      return val
    reps <- repDivWidget iReps
    combineDyn (\x y -> Atom x y) val reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLNumberWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLNumberWidgetTable-layertd")
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ "class"=:"aGLNumberWidgetTable-deletetd"
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = (RebuildMe <$) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x-> (x,leftmost [rebuildEvent, deleteEvent])) genPat''
aGLStringWidget _ e = aGLStringWidget (Atom "~" Once) e


-- atomWidgetInt


------------------------------------------
--             UTILITY GENPAT'S         --
------------------------------------------

--Slider w/ a range and stepsize
sliderWidget::MonadWidget t m => (Double,Double)-> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
sliderWidget (minVal,maxVal) stepsize iVal _ = do
  text $ show minVal
  let attrs = constDyn $ fromList $ zip ["type","min","max","step","style"] ["range",show minVal,show maxVal,show stepsize,"width:75px"]
  slider<-textInput $ def & textInputConfig_inputType .~ "range" & textInputConfig_attributes .~ attrs
  text $ show maxVal
  let panVal = _textInput_value slider
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Double) then Atom (read x::Double) Once else Atom 0.5 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))


-- A clickable td element. Each click cycles to the next element in the map. Updated with a RepOrDiv event.
-- rep/div values get shown on the button too.
clickListWidget::(MonadWidget t m, Show a, Eq a) => Map Int a ->  GeneralPattern a -> Event t RepOrDiv -> m (Dynamic t (GeneralPattern a, Event t (EditSignal a)))
clickListWidget cycleMap (Atom iVal iReps) updatedReps = mdo
  let initialNum = maybe (0::Int) id $ Data.List.findIndex (==iVal) $ elems cycleMap
  sampleButton <- tdButtonAttrs' showVal (iVal) $ "class"=:"clickListtd"
  num <- count sampleButton >>= mapDyn (\x-> (x+initialNum) `mod` length cycleMap)
  str'' <- mapDyn (\x-> maybe iVal id $ Data.Map.lookup x cycleMap) num
  let str' = updated str''
  str <- holdDyn (iVal) str'
  reps <- holdDyn (iReps) updatedReps
  returnSample <- combineDyn (\x r -> Atom x r) str reps
  showVal <- mapDyn show returnSample
  mapDyn (\x->(x,never)) returnSample

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
countStepWidget step (Atom iUpVal _) _ = elAttr "table" ("class"=:"countWidgetTable") $ mdo
  upCount <- el "tr" $ do
    elAttr "td" ("class"=:"countWidgetTable") $ dynText upValShow
    upButton <- tdButtonAttrs "▲" () ("class"=:"countWidgetTable-upArrowtd") >>= count
    return upButton
  (deleteEvent,downCount) <- el "tr" $ do
    deleteButton <- tdButtonAttrs "-" (DeleteMe) $ "style"=:"text-align:center; background-color:lightblue;border 1pt solidblack"
    downButton <- tdButtonAttrs "▼" () ("class"=:"countWidgetTable-downArrowtd") >>= count
    return $ (deleteButton, downButton)
  upVal <- combineDyn (\a b ->  (a*step)-(b*step)+(iUpVal)) upCount downCount
  upValShow <- forDyn upVal show
  --repsHold <- holdDyn iUpVal $ updated repeats
  mapDyn (\x->(Atom x Once,deleteEvent)) upVal
countStepWidget step _ e = countStepWidget step (Atom 0 Once) e

-- widget with a slider returning a single Atom with range [minVal,maxVal] and stepsize specified
doubleSliderWidget::MonadWidget t m => (Double,Double) -> Double -> GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
doubleSliderWidget (minVal,maxVal) stepsize (Atom iEnd Once) _ = elAttr "table" ("class"=:"doubleSliderWidget") $ mdo
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


-- < and > buttons, background color fill illustrateds value
-- see iclc fixed, end widget
--  -----------------
--  --  <   >   -  --
--  -----------------
faderButtonWidget::MonadWidget t m => GeneralPattern Double -> Event t () -> m (Dynamic t (GeneralPattern Double, Event t (EditSignal a)))
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



charWidget'::MonadWidget t m => GeneralPattern Char-> Event t () -> m (Dynamic t (GeneralPattern Char,Event t (EditSignal a)))
charWidget' (Atom iVal reps) _ = do
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

-- used in charContainer, example in Vowel in ICLCStacked widget
charWidget::(MonadWidget t m) => GeneralPattern Char -> Event t (EditSignal (GeneralPattern Char)) -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal (GeneralPattern Char))))
charWidget (Atom iVal iReps) _ = elAttr "table" ("class"=:"aGLStringWidgetTable") $ mdo
  genPat <- el "tr" $ do
    val <- el "td" $ do
      inputField <- textInput $ def & textInputConfig_attributes .~ constDyn ("class"=:"aGLNumberWidgetTable-textFieldtd") & textInputConfig_initialValue .~ ([iVal])
      let val = _textInput_value inputField
      forDyn val (\x-> maybe (' ') id $ listToMaybe x)
    reps <- repDivWidget iReps
    combineDyn (\x y -> Atom x y) val reps
  (layerEvent, groupEvent, deleteEvent) <- el "tr" $ elAttr "td" ("colspan"=:"3") $ el "tr" $ do
    groupButton <- tdButtonAttrs " [] " (MakeGroup) $ ("class"=:"aGLStringWidgetTable-grouptd")
    layerButton <- tdButtonAttrs " [,,] " (MakeLayer) $ ("class"=:"aGLStringWidgetTable-layertd")
    deleteButton <- tdButtonAttrs " - " (DeleteMe) $ ("class"=:"aGLStringWidgetTable-deletetd")
    return $ (layerButton, groupButton, deleteButton)
  groupToggle <- toggle False groupEvent
  layerToggle <- toggle False layerEvent
  genPat' <- combineDyn (\u tog-> if tog then Group [u] Once else u) genPat groupToggle
  genPat''<- combineDyn (\u tog-> case u of (Atom a x) -> if tog then Layers [u] Once else u; otherwise-> u) genPat' layerToggle
  let rebuildEvent = fmap (const RebuildMe) $ leftmost [layerEvent, groupEvent]
  mapDyn (\x-> (x,leftmost [rebuildEvent, deleteEvent])) genPat''
charWidget _ e = charWidget (Atom ' ' Once) e

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
  genPat <- combineDyn (\str rep-> if isJust (readMaybe str::Maybe Int) then Atom (read str::Int) rep else Atom 0 rep ) inputVal repeats'
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
  panVal' <- forDyn panVal (\x-> if isJust (readMaybe x::Maybe Int) then Atom (read x::Int) Once else Atom 16 Once)
  deleteButton <- button' "-" DeleteMe
  forDyn panVal' (\k -> (k,deleteButton))


---- uses clickListWidget as a base widget, intersperses with + buttons
sampleNameWidget::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a)))
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




---- Eldad's Widgets:
sButtonContainer::MonadWidget t m => GeneralPattern SampleName -> Event t () -> m (Dynamic t (GeneralPattern SampleName, Event t (EditSignal a), Event t Hint))
sButtonContainer (Atom iSamp iReps) _ = elAttr "table" tableAttrs $ mdo
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
  combineDyn (\x y -> (Atom x y,deleteEvent,hints)) sample repeats''
  where tableAttrs=("style"=:"margin:5px;display:inline-table;background-color:lightgreen;width:10%;padding:6px;border-spacing:5px;border: 3pt solid black")
sButtonContainer _ e = sButtonContainer (Atom "~" Once) e

sButtonWidget::MonadWidget t m => SampleName -> Dynamic t RepOrDiv -> m (Dynamic t SampleName)
sButtonWidget iSamp reps = mdo
  let sMap = fromList $ zip [(0::Int)..] ["~","bd","sn","cp","hh","arpy","glitch","tabla"]
  let iNum = maybe (0::Int) id $ Data.List.findIndex (==iSamp) $ elems sMap
  sampleButton <- tdButtonAttrs' (showSample) (iSamp) $ "style"=:"width:60%;text-align:center;background-color:lightblue"
  num <- count sampleButton >>= mapDyn (\x-> (x+iNum) `mod` length sMap)
  sName <- mapDyn (\x-> maybe ("~") id $ Data.Map.lookup x sMap) num
  atom <- combineDyn (Atom) sName reps
  showSample <- mapDyn (show) atom
  return sName

-- returns atom of a character
vowelButtonWidget::MonadWidget t m =>  GeneralPattern Char -> Event t () -> m (Dynamic t (GeneralPattern Char, Event t (EditSignal a)))
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



-- For faderButtonWidget - gradient string used to show the
makeStyleString gradient =
  "background: -webkit-linear-gradient(90deg,lightgreen "++ (show $ x+1) ++ "%, white "++(show $ x) ++ "%);"++
    "background: -o-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x)++ "%);" ++
      "background: -moz-linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white "++ (show x) ++ "%);" ++
        "background: linear-gradient(90deg, lightgreen "++ (show $ x+1) ++ "%, white " ++ (show x) ++ "%);"
  where x = gradient*100
