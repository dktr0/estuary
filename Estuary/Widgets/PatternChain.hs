{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.PatternChain where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Reflex.Container
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.SpecificPattern
import Control.Monad
import Data.Map
import qualified Estuary.Widgets.SpecificPattern as Sp
import qualified Estuary.Widgets.GeneralPattern as G
import qualified Data.List


iclcFixedStruct:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
iclcFixedStruct iChain _ = elAttr "div" (empty) $ do
  s<- elAttr "div" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "Sound"
    (pat,_)<- Sp.specificStringContainer (S Blank) never >>= splitDyn
    forDyn pat (\x-> TransformedPattern [NoTransformer] x)
  end <- elAttr "div" ("style"=:"background-color:Lightyellow") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "End"
    (pat,_) <- Sp.endContainerWidget (End $ Atom 0.5 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "div" ("style"=:"background-color:wheat") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "Vowel"
    (pat,_) <- Sp.charContainerWidget (Vowel $ Atom '~' Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  up <- elAttr "tr" ("style"=:"background-color:lightcyan;display:inline-block") $ do
    elAttr "div" ("style"=:"font-size:100%;margin:5px;display:inline-block") $ text "Up"
    (pat,_) <- Sp.specificDoubleContainer (Up $ Atom 0 Once) never >>= splitDyn
    forDyn pat (\x -> TransformedPattern [NoTransformer] x)
  patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
  patChain' <- combineDyn (\e p-> PatternChain' e Merge p) end patChain''
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
  mapDyn (\x-> (x,never)) patChain
  where
    toPatternChain (TransformedPattern _ (End  (Group [] _))) = EmptyPatternChain
    toPatternChain (TransformedPattern _ (Vowel (Group [] _))) = EmptyPatternChain
    toPatternChain x = PatternChain x

icoahWidget:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
icoahWidget iChain _ = elAttr "table" ("cellspacing"=:"0") $ do
  s<- elAttr "tr" ("style"=:"vertical-align:center;background-color:lightgrey") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "S"
    (pat,_)<- Sp.sampleContainerWidget' (S $ Blank) never >>= splitDyn
    forDyn pat (\x-> TransformedPattern [NoTransformer] x)
  end <- elAttr "tr" ("style"=:"background-color:Lightyellow") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px") $ text "End"
    (pat,_) <- Sp.endContainerWidget (End $ Atom 0.5 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "tr" ("style"=:"background-color:wheat") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Vowel"
    (pat,_) <- Sp.charContainerWidget (Vowel $ Atom '~' Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  up <- elAttr "tr" ("style"=:"background-color:lightcyan") $ do
    elAttr "td" ("style"=:"font-size:100%;margin:5px;") $ text "Up"
    (pat,_) <- Sp.upContainerWidget (Up $ Atom 0 Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
  patChain' <- combineDyn (\e p-> PatternChain' e Merge p) end patChain''
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
  mapDyn (\x-> (x,never)) patChain
  where
    toPatternChain (TransformedPattern _ (End  (Group [] _))) = EmptyPatternChain
    toPatternChain (TransformedPattern _ (Vowel (Group [] _))) = EmptyPatternChain
    toPatternChain x = PatternChain x


patternCombinatorDropDown :: MonadWidget t m => PatternCombinator -> Event t () -> m (Dynamic t (PatternCombinator,Event t GenericSignal))
patternCombinatorDropDown iValue _ = do
  let ddMap = constDyn $ fromList $ [ (x,show x) | x <- [Merge,Add,Subtract,Multiply,Divide] ]
  dd <- dropdown iValue ddMap def
  mapDyn (\x -> (x,never)) $ _dropdown_value dd

patternChainToList :: PatternChain -> [Either TransformedPattern PatternCombinator]
patternChainToList (EmptyPatternChain) = []
patternChainToList (PatternChain x) = [Left x]
patternChainToList (PatternChain' x y z) = [Left x,Right y] ++ (patternChainToList z)

patternChainToList' :: PatternChain -> [Either (Either TransformedPattern PatternCombinator) ()]
patternChainToList' p = (Right ()):(f p)
  where
    f (EmptyPatternChain) = []
    f (PatternChain x) = [Left (Left x),Right ()]
    f (PatternChain' x y z) = [Left (Left x),Right (),Left (Right y)] ++ (f z)

listToPatternChain :: [Either TransformedPattern PatternCombinator] -> PatternChain
listToPatternChain [] = EmptyPatternChain
listToPatternChain ((Left x):[]) = PatternChain x
listToPatternChain ((Left x):(Right y):z) = PatternChain' x y (listToPatternChain z)

patternChainAdd :: (Ord k,Num k) => Map k (Either TransformedPattern PatternCombinator) -> k
  -> [(k,Construction (Either (Either TransformedPattern PatternCombinator) () ))]
patternChainAdd m k | Data.Map.null m = [(k+1,Insert (Left (Left def))),(k+2,Insert (Right ()))]
                    | k<(fst (findMin m)) = [(k+1,Insert (Left (Left def))),(k+2,Insert (Right ())),(k+3,Insert (Left (Right Merge)))]
                    | otherwise = [(k,Insert (Right ())),(k+1,Insert (Left (Right Merge))),(k+2,Insert (Left (Left def)))]
  where def = TransformedPattern [] emptySPattern

patternChainDel:: (Ord k,Num k) => Map k (Either TransformedPattern PatternCombinator) -> k
  -> [(k,Construction (Either (Either TransformedPattern PatternCombinator) () ))]
patternChainDel m k | Data.Map.null m = []
                    | Data.Map.size m == 1 = [(k,Delete),(k+1,Delete)]
                    | k==(fst (findMax m)) = [(k-1,Delete),(k,Delete),(k+1,Delete)]
                    | otherwise = [(k,Delete),(k+1,Delete),(k+2,Delete)]

-- Widget used in iclc stacked pattern demo
iclcForStacked :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
iclcForStacked iValue _ = mdo
  let iMap = fromList $ zip ([0..]::[Int]) $ patternChainToList' iValue
  let cEvents = mergeWith union [addMap,deleteMap]
  let patternOrCombinatorWidget = eitherWidget transformedPatternWidget patternCombinatorDropDown
  (values,events) <- eitherContainer' iMap cEvents never never patternOrCombinatorWidget (pingButton'' "+")
  let addKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let addList = attachDynWith (\a b -> concat (Prelude.map (patternChainAdd a) b)) values addKeys
  let addList' = traceEvent "addList" addList
  let addMap = fmap (fromList) addList'
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
  let deleteList = attachDynWith (\a b -> concat (Prelude.map (patternChainDel a) b)) values deleteKeys
  let deleteList' = traceEvent "deleteList" deleteList
  let deleteMap = fmap (fromList) deleteList'
  mapDyn ((\x -> (x,never)) . listToPatternChain . elems) values


iclcTextWidget :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
iclcTextWidget iValue _ = mdo
  let iMap = fromList $ zip ([0..]::[Int]) $ patternChainToList' iValue
  let cEvents = mergeWith union [addMap,deleteMap]
  let patternOrCombinatorWidget = eitherWidget transformedPatternTextWidget patternCombinatorDropDown
  (values,events) <- eitherContainer' iMap cEvents never never patternOrCombinatorWidget (pingButton'' "+")
  let addKeys = fmap (keys . Data.Map.filter (==Ping)) events
  let addList = attachDynWith (\a b -> concat (Prelude.map (patternChainAdd a) b)) values addKeys
  let addList' = traceEvent "addList" addList
  let addMap = fmap (fromList) addList'
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
  let deleteList = attachDynWith (\a b -> concat (Prelude.map (patternChainDel a) b)) values deleteKeys
  let deleteList' = traceEvent "deleteList" deleteList
  let deleteMap = fmap (fromList) deleteList'
  mapDyn ((\x -> (x,never)) . listToPatternChain . elems) values



trivialPatternChain :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
trivialPatternChain iValue _ = do
  x <- button' "just an S atom" $ PatternChain (TransformedPattern [] (S (Atom "bd" Once)))
  el "br" blank
  y <- button' "merge S and N" $ PatternChain' (TransformedPattern [] (S (Atom "hh" (Rep 4)))) Merge (PatternChain (TransformedPattern [] (nPatternFromList [0..3])))
  el "br" blank
  z <- button' "adding Ns" $ PatternChain' (TransformedPattern [] (N (Atom 60 Once))) Add (PatternChain (TransformedPattern [] (N (Atom 7 Once))))
  el "br" blank
  xyz <- holdDyn iValue $ leftmost [x,y,z]
  mapDyn (\a -> (a,never)) xyz

  -- patternChainAdd :: (Ord k,Num k) => Map k (Either TransformedPattern PatternCombinator) -> k
  --   -> [(k,Construction (Either (Either TransformedPattern PatternCombinator) () ))]

-- patternChainWidget' :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
-- patternChainWidget' iValue _ = mdo
--   let iMap = fromList $ zip ([0..]::[Int]) $ patternChainToList' iValue
--   let cEvents = mergeWith union [addMap,deleteMap]
--   let patternOrCombinatorWidget = eitherWidget transformedPatternWidget patternCombinatorDropDown
--
--   addButton <- pingButton'' "+"
--
--   (values,events) <- eitherContainer' iMap cEvents never never patternOrCombinatorWidget addButton
--   let addKeys = fmap (keys . Data.Map.filter (==Ping)) events -- [ping event keys]
--   let addList = attachDynWith (\a b -> concat (Prelude.map (patternChainAdd a) b)) values addKeys
--   let addList' = traceEvent "addList" addList
--   let addMap = fmap (fromList) addList'
--   let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
--   let deleteList = attachDynWith (\a b -> concat (Prelude.map (patternChainDel a) b)) values deleteKeys
--   let deleteList' = traceEvent "deleteList" deleteList
--   let deleteMap = fmap (fromList) deleteList'
--   mapDyn ((\x -> (x,never)) . listToPatternChain . elems) values
