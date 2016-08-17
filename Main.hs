
module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.StackedPatterns
import Control.Monad (liftM)
import qualified Sound.Tidal.Context as Tidal
import Estuary.WebDirt.Stream
import Estuary.Page
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt

main :: IO ()
main = do
  wd <- webDirt
  stream <- webDirtStream webDirt
    webDirtWidget webDirt
    mainWidget $ do
    multipage stream pages

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t GenericSignal))
twoStackedPatterns = stackedPatternsWidget (StackedPatterns [EmptyPatternChain,EmptyPatternChain]) never

pages = [
  ("Two stacked patterns",widgetToPage twoStackedPatterns),
  ("S-Specific Pattern",widgetToPage $ sContainerWidget (Estuary.Tidal.Types.S Blank) never),
  ("Sample and Pan Pattern", widgetToPage $ panSampleWidget (Estuary.Tidal.Types.S Blank) never)
  ]
