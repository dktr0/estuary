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
stackedPatternsWidget (StackedPatterns xs) _ = elAttr "table" tableAttrs $ elAttr "tr" trAttrs $ do
  c <- wfor xs $ \x -> elAttr "td" tdAttrs $ do
    y <- trivialPatternChain x never
    mapDyn fst y
  mapDyn (\x -> (StackedPatterns x,never)) c
  where
    placeHolder = PatternChain (TransformedPattern [] (S (Atom "bd" Once)))
    tableAttrs = singleton "style" "position: absolute; height: 90%; width:90%; border: 1px solid black;"
    trAttrs = singleton "style" "border: 1px solid grey;"
    tdAttrs = singleton "style" "border: 1px solid grey;"
