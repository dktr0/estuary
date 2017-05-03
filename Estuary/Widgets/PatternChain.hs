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


iclcFixedStruct:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t (EditSignal a)))
iclcFixedStruct iChain _ = elAttr "div" (empty) $ do
  s<- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "Sound"
    (pat,_)<- Sp.specificContainer (S $ Atom "~" Inert Once) never >>= splitDyn
    forDyn pat (\x-> TransformedPattern [NoTransformer] x)
  end <- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "End"
    (pat,_) <- Sp.endContainerWidget (End $ Atom 0.5 Inert Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  vowel <- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "Vowel"
    (pat,_) <- Sp.charContainerWidget (Vowel $ Atom '~' Inert Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  up <- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "Up"
    (pat,_) <- Sp.specificContainer (Up $ Atom 0 Inert Once) never >>= splitDyn
    forDyn pat (\x -> TransformedPattern [NoTransformer] x)
  patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
  patChain' <- combineDyn (\e p-> PatternChain' e Merge p) end patChain''
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain' s
  mapDyn (\x-> (x,never)) patChain


icoahWidget:: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t (EditSignal a), Event t Hint))
icoahWidget iChain _ = elAttr "table" ("class"=:"multiPatternTable") $ do
  (s,hints) <- elAttr "tr" ("class"=:"multiPatternTable-tr") $ do
    elAttr "td" ("class"=:"multiPatternTable-td") $ text "S"
    swidget <- Sp.sampleContainerWidget (S $ Blank Inert Once) never
    pat <- mapDyn (\(x,_,_) -> x) swidget
    pat' <- mapDyn (TransformedPattern [NoTransformer]) pat
    hints' <- liftM (switchPromptlyDyn) $ mapDyn (\(_,_,x) -> x) swidget
    return (pat',hints')
  vowel <- elAttr "tr" ("class"=:"multiPatternTable-tr") $ do
    elAttr "td" ("class"=:"multiPatternTable-td") $ text "Vowel"
    (pat,_) <- Sp.charContainerWidget (Vowel $ Atom '~' Inert Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  up <- elAttr "tr" ("class"=:"multiPatternTable-tr") $ do
    elAttr "td" ("class"=:"multiPatternTable-td") $ text "Up"
    (pat,_) <- Sp.upContainerWidget (Up $ Atom 0 Inert Once) never >>= splitDyn
    forDyn pat (TransformedPattern [NoTransformer])
  patChain''<- combineDyn (\v u -> PatternChain' v Merge (PatternChain u)) vowel up
  patChain <- combineDyn (\chain pat -> PatternChain' pat Merge chain) patChain'' s
  mapDyn (\x-> (x,never,hints)) patChain

simpleFixedInterface :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t (EditSignal a),Event t Hint))
simpleFixedInterface i e = do
  x <- divClass "twoStackedPatternsLeft" $ icoahWidget i e
  y <- divClass "twoStackedPatternsRight" $ divClass "paddedText" $ do
    el "div" $ text "In this interface you can combine simple patterns of samples (s), vowels and pitch changes (up). Click a + button to an add element to one of those three patterns. A ~ in an s pattern represents silence - you can click on the ~ to advance to a named sample/sound, and click again to advance through to further sounds. Clicking the up or down arrows repeats an element within the time allotted to it in the overall pattern."
  return x


patternCombinatorDropDown :: MonadWidget t m => PatternCombinator -> Event t () -> m (Dynamic t (PatternCombinator,Event t (EditSignal a)))
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
iclcForStacked :: (MonadWidget t m) => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t (EditSignal a)))
iclcForStacked iValue _ = mdo
  let iMap = fromList $ zip ([0..]::[Int]) $ patternChainToList' iValue
  let cEvents = mergeWith union [addMap,deleteMap]
  let patternOrCombinatorWidget = eitherWidget transformedPatternWidget patternCombinatorDropDown
  (values,events) <- eitherContainer' iMap cEvents never never patternOrCombinatorWidget (makeNewButton "+")
  let addKeys = fmap (keys . Data.Map.filter (==MakeNew)) events
  let addList = attachDynWith (\a b -> concat (Prelude.map (patternChainAdd a) b)) values addKeys
  let addList' = traceEvent "addList" addList
  let addMap = fmap (fromList) addList'
  let deleteKeys = fmap (keys . Data.Map.filter (==DeleteMe)) events
  let deleteList = attachDynWith (\a b -> concat (Prelude.map (patternChainDel a) b)) values deleteKeys
  let deleteList' = traceEvent "deleteList" deleteList
  let deleteMap = fmap (fromList) deleteList'
  mapDyn ((\x -> (x,never)) . listToPatternChain . elems) values
