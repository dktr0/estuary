module Estuary.Widgets.StackedPatterns where

import Reflex
import Reflex.Dom
import Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types


stackedPatternsWidget :: Int -> Int -> StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t GenericSignal))
stackedPatternsWidget rows columns (StackedPatterns iValue) _ = el "table"  $ do
  rows <- forM [1..rows] $ \r -> el "tr" $ do
    columns <- forM [1..columns] $ \c -> el "td" $ trivialPatternChain
  let patterns =  mapDyn fst $ concat rows
  mapDyn (\a -> (StackedPatterns patterns,never))


trivialPatternChain :: PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
trivialPatternChain iValue _ = do
  text "trivialPatternChain"
  return $ constDyn (iValue,never)
