module Estuary.Widgets.StackedPatterns where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Reflex.Container
import Estuary.Widgets.Generic
import Estuary.Widgets.PatternChain
import Control.Monad
import Data.Map

-- from Estuary.Tidal.Types:
-- data StackedPatterns = StackedPatterns [PatternChain]

stackedPatternsWidget :: MonadWidget t m => StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t ()))
stackedPatternsWidget (StackedPatterns xs) _ = elAttr "table" ("class"=:"stackedPatternTable") $ elAttr "tr" ("class"=:"stackedPatternTable-tr") $ do
  c <- wfor xs $ \x -> elAttr "td" ("class"=:"stackedPatternTable-td") $ do
    y <- iclcForStacked x never
    mapDyn fst y
  mapDyn (\x -> (StackedPatterns x,never)) c

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t (), Event t Hint))
twoStackedPatterns = do
  x <- divClass "twoStackedPatternsLeft" $ iclcForStacked EmptyPatternChain never
  y <- divClass "twoStackedPatternsRight" $ iclcForStacked EmptyPatternChain never
  x' <- mapDyn fst x
  y' <- mapDyn fst y
  combineDyn (\a b -> (StackedPatterns [a,b],never,never)) x' y'


textPanel :: MonadWidget t m => m (Dynamic t (String,Event t ())) -- event is "evaluation"
textPanel = do
  eval <- button "evaluate"
  t <- textArea
  mapDyn (\x -> (x,eval)) $ _textArea_value t
  

ninePanels :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t (), Event t Hint))
ninePanels = do
  one <- iclcForStacked EmptyPatternChain never
  one' <- mapDyn fst one
  two <- iclcForStacked EmptyPatternChain never
  two' <- mapDyn fst two
  three <- iclcForStacked EmptyPatternChain never
  three' <- mapDyn fst three
  four <- iclcForStacked EmptyPatternChain never
  four' <- mapDyn fst four
  five <- iclcForStacked EmptyPatternChain never
  five' <- mapDyn fst five
  six <- iclcForStacked EmptyPatternChain never
  six' <- mapDyn fst six
  seven <- iclcForStacked EmptyPatternChain never
  seven' <- mapDyn fst seven
  eight <- iclcForStacked EmptyPatternChain never
  eight' <- mapDyn fst eight
  nine <- iclcForStacked EmptyPatternChain never
  nine' <- mapDyn fst nine
  x <- combineDyn (\a b -> [a,b]) one' three'
  x' <- combineDyn (\a b -> a ++ [b]) x five'
  x'' <- combineDyn (\a b -> a ++ [b]) x' seven'
  x''' <- combineDyn (\a b -> a ++ [b]) x'' nine'
  x'''' <- mapDyn (\a -> (StackedPatterns a,never,never)) x'''
