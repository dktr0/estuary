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

stackedPatternsWidget :: MonadWidget t m => StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t GenericSignal))
stackedPatternsWidget (StackedPatterns xs) _ = elAttr "table" ("class"=:"stackedPatternTable") $ elAttr "tr" ("class"=:"stackedPatternTable-tr") $ do
  c <- wfor xs $ \x -> elAttr "td" ("class"=:"stackedPatternTable-td") $ do
    y <- iclcForStacked x never
    mapDyn fst y
  mapDyn (\x -> (StackedPatterns x,never)) c

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t GenericSignal))
twoStackedPatterns = do
  x <- divClass "twoStackedPatternsLeft" $ iclcForStacked EmptyPatternChain never
  y <- divClass "twoStackedPatternsRight" $ iclcForStacked EmptyPatternChain never
  x' <- mapDyn fst x
  y' <- mapDyn fst y
  combineDyn (\a b -> (StackedPatterns [a,b],never)) x' y'
