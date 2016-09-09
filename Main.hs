
module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.StackedPatterns
import Estuary.Widgets.PatternChain as P
import Control.Monad (liftM)
import qualified Sound.Tidal.Context as Tidal
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.Page
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt
import Estuary.Widgets.ConferenceWidget as C

main :: IO ()
main = do
  wd <- webDirt
  initializeWebAudio wd
  stream <- webDirtStream wd
  mainWidget $ do
    webDirtWidget wd
    multipage stream pages

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t GenericSignal))
twoStackedPatterns = stackedPatternsWidget (StackedPatterns [EmptyPatternChain,EmptyPatternChain]) never

pages = [
  ("Conf3", widgetToPage $ C.eldadWidget'' EmptyPatternChain never),
  ("Conf2",widgetToPage $ C.eldadWidget' EmptyPatternChain never),
  ("Conf1",widgetToPage $ C.eldadWidget EmptyPatternChain never),
  ("Sample and Pan Pattern", widgetToPage $ panSampleWidget (Estuary.Tidal.Types.S Blank) never),
  ("Two stacked patterns",widgetToPage twoStackedPatterns)
  ]
