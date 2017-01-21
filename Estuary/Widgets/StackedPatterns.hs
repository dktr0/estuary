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

-- Used in ICLC Stacked Patterns Widget
stackedPatternsWidget :: MonadWidget t m => StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t GenericSignal))
stackedPatternsWidget (StackedPatterns xs) _ = elAttr "table" ("class"=:"stackedPatternTable") $ elAttr "tr" ("class"=:"stackedPatternTable-tr") $ do
  c <- wfor xs $ \x -> elAttr "td" ("class"=:"stackedPatternTable-td") $ do
    y <- iclcForStacked x never
    mapDyn fst y
  mapDyn (\x -> (StackedPatterns x,never)) c