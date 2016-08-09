module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Widgets.StackedPatterns
import Estuary.Page
import Estuary.WebDirt.Stream

main :: IO ()
main = do
  webDirt <- webDirtStream
  mainWidget $ do
    let widget = stackedPatternsWidget (StackedPatterns [emptyPatternChain,emptyPatternChain]) never
    page' webDirt widget
