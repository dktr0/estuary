module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Widgets.StackedPatterns
import Estuary.Page
import Estuary.WebDirt.Stream

x = PatternChain (TransformedPattern [] (S (Atom "bd" Once)))

y = PatternChain' (TransformedPattern [] (S (Atom "hh" (Rep 4)))) Merge (PatternChain (TransformedPattern [] (nPatternFromList [0..3])))

z = PatternChain' (TransformedPattern [] (N (Atom 60 Once))) Add (PatternChain (TransformedPattern [] (N (Atom 7 Once))))

main :: IO ()
main = do
  webDirt <- webDirtStream
  mainWidget $ do
    let widget = stackedPatternsWidget (StackedPatterns [EmptyPatternChain,EmptyPatternChain]) never
    page' webDirt widget
