
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
import Data.Map
import Estuary.Widgets.IclcWidgets as I
import Control.Monad.IO.Class
import Estuary.Widgets.ConferenceWidget as C


main :: IO ()
main = do
  wd <- webDirt
  initializeWebAudio wd
  stream <- webDirtStream wd
  mainWidget $ do
    elAttr "img" (fromList $ zip ["src","width","height"] ["logo.png","168","59"]) blank
    startButton <- el "div" $ button "Start"
    --performEvent $ fmap (liftIO . (initializeWebAudio wd)) startButton
    performEvent $ fmap (liftIO . ( const $ initializeWebAudio wd)) startButton
    --webDirtWidget wd
    el "div" $ multipage stream pages

-- main :: IO ()
-- main = mainWidget $ do
--   elAttr "img" (fromList $ zip ["src","width","height"] ["logo.jpg","150","64"]) blank
--   wd <- webDirt
--   stream <- webDirtStream wd
--   webDirtWidget wd
--   multipage stream pages

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t GenericSignal))
twoStackedPatterns = stackedPatternsWidget (StackedPatterns [EmptyPatternChain,EmptyPatternChain]) never

hack::MonadWidget t m => m (Dynamic t (SpecificPattern, Event t ()))
hack = do
  (a,b) <- C.groupWidget Blank never >>= splitDyn
  forDyn a (\x-> (Estuary.Tidal.Types.S x,never))

pages = [
  ("ICLC Text Widget",widgetToPage $ stackedPatternsTextWidget (StackedPatterns [EmptyPatternChain]) never),
  ("ICLC Stacked Patterns Widget",widgetToPage twoStackedPatterns),
  ("ICLC Fixed Widget", widgetToPage $ I.iclcFixedStruct EmptyPatternChain never),
  ("ICOAH", widgetToPage $ C.eldadWidget''' EmptyPatternChain never),
  ("Conf3", widgetToPage $ C.eldadWidget'' EmptyPatternChain never),
  ("Conf2",widgetToPage $ C.eldadWidget' EmptyPatternChain never),
  ("Conf1",widgetToPage $ C.eldadWidget EmptyPatternChain never),
  ("Sample and Pan Pattern", widgetToPage $ panSampleWidget (Estuary.Tidal.Types.S Blank) never)
  ]
