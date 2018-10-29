{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.PatternChain where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
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

import Estuary.Types.Hint

toCombinedTransPat::[TransformedPattern] -> TransformedPattern
toCombinedTransPat (UntransformedPattern x:[]) = UntransformedPattern x
toCombinedTransPat (TransformedPattern trans x:[]) = TransformedPattern trans x
toCombinedTransPat ((UntransformedPattern x):xs) = TransformedPattern (Combine x Merge) $ toCombinedTransPat xs
toCombinedTransPat ((TransformedPattern trans tpat):xs) = TransformedPattern trans $ toCombinedTransPat $ tpat:xs


iclcFixedStruct:: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern, Event t (EditSignal a)))
iclcFixedStruct iChain _ = elAttr "div" (empty) $ do
  s<- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "Sound"
    (pat,_)<- Sp.specificContainer (S $ Atom "~" Inert Once) never >>= mapDyn (\(x,y,_)->(x,y)) >>= splitDyn
    forDyn pat UntransformedPattern
  end <- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "End"
    (pat,_) <- Sp.endContainerWidget (End $ Atom 0.5 Inert Once) never >>= splitDyn
    forDyn pat UntransformedPattern
  vowel <- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "Vowel"
    (pat,_) <- Sp.charContainerWidget (Vowel $ Atom '~' Inert Once) never >>= splitDyn
    forDyn pat UntransformedPattern
  up <- elAttr "div" ("class"=:"singlePatternDiv") $ do
    elAttr "div" ("class"=:"singlePatternDiv-label") $ text "Up"
    (pat,_) <- Sp.specificContainer (Up $ Atom 0 Inert Once) never >>= mapDyn (\(x,y,_)->(x,y)) >>= splitDyn
    forDyn pat UntransformedPattern
  combineDyn (\a b ->[a,b]) s end >>= combineDyn (:) up >>= combineDyn (:) vowel >>= mapDyn (\x -> (toCombinedTransPat x,never))



icoahWidget:: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern, Event t (EditSignal a), Event t Hint))
icoahWidget iChain _ = elAttr "table" ("class"=:"multiPatternTable") $ do
  (s,hints) <- elAttr "tr" ("class"=:"multiPatternTable-tr") $ do
    elAttr "td" ("class"=:"multiPatternTable-td") $ text "S"
    swidget <- Sp.sampleContainerWidget (S $ Blank Inert) never
    pat <- mapDyn (\(x,_,_) -> x) swidget
    pat' <- mapDyn UntransformedPattern pat
    hints' <- liftM (switchPromptlyDyn) $ mapDyn (\(_,_,x) -> x) swidget
    return (pat',hints')
  vowel <- elAttr "tr" ("class"=:"multiPatternTable-tr") $ do
    elAttr "td" ("class"=:"multiPatternTable-td") $ text "Vowel"
    (pat,_) <- Sp.charContainerWidget (Vowel $ Atom '~' Inert Once) never >>= splitDyn
    forDyn pat UntransformedPattern
  up <- elAttr "tr" ("class"=:"multiPatternTable-tr") $ do
    elAttr "td" ("class"=:"multiPatternTable-td") $ text "Up"
    (pat,_) <- Sp.upContainerWidget (Up $ Atom 0 Inert Once) never >>= splitDyn
    forDyn pat UntransformedPattern
  combineDyn (\a b ->[a,b]) s vowel >>= combineDyn (:) up >>= mapDyn (\x -> (toCombinedTransPat x,never,hints))


simpleFixedInterface :: MonadWidget t m => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern, Event t (EditSignal a),Event t Hint))
simpleFixedInterface i e = do
  x <- divClass "twoStackedPatternsLeft" $ icoahWidget i e
  y <- divClass "twoStackedPatternsRight" $ divClass "paddedText" $ do
    el "div" $ text "In this interface you can combine simple patterns of samples (s), vowels and pitch changes (up). Click a + button to an add element to one of those three patterns. A ~ in an s pattern represents silence - you can click on the ~ to advance to a named sample/sound, and click again to advance through to further sounds. Clicking the up or down arrows repeats an element within the time allotted to it in the overall pattern."
  return x



patternChainDel:: (Ord k,Num k) => Map k (Either TransformedPattern PatternCombinator) -> k
  -> [(k,Construction (Either (Either TransformedPattern PatternCombinator) () ))]
patternChainDel m k | Data.Map.null m = []
                    | Data.Map.size m == 1 = [(k,Delete),(k+1,Delete)]
                    | k==(fst (findMax m)) = [(k-1,Delete),(k,Delete),(k+1,Delete)]
                    | otherwise = [(k,Delete),(k+1,Delete),(k+2,Delete)]



transformedPatternToList::TransformedPattern -> [Either (Either TransformedPattern PatternCombinator) ()]
transformedPatternToList x = (Right ()):(f x)
  where
    f (UntransformedPattern s) = [Left $ Left $ UntransformedPattern s]
    f (TransformedPattern (Combine sPat comb) tpat) = [Left $ Left $ UntransformedPattern sPat,Right(),Left $ Right comb] ++ (f tpat)
    f y = [Left $ Left y, Right ()]



iclcForStacked :: (MonadWidget t m) => TransformedPattern -> Event t () -> m (Dynamic t (TransformedPattern, Event t (EditSignal a), Event t Hint))
iclcForStacked tPat _= resettableTransformedPatternWidget tPat never


