
module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Reflex.Container
import Estuary.Widgets.StackedPatterns
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Control.Monad (liftM)
import qualified Sound.Tidal.Context as Tidal
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.Page
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt
import Data.Map
import Control.Monad.IO.Class


main :: IO ()
main = mainWidget $ do
  clickableDivClass' "bd " "noClass" ()
  popupSignalWidget
  popupSignalWidget
  clickableDivClass' "lkdjlksjdlkfjslkdfjslkdjflksdjflksdjflkdsjflksdjflksdfjlsdkjflsdkfjlsdkjflksdjflksdfjsldkfjsdlkfjslkdfjklsdfjklsdfjlkdsfjsldkjflsdkfjlsdkfjlsdkfjldskjflkdsfjsdslkdjflksdjflksdjflksdfjkdlsjflsdkjflksdjflsdkjfldskfjlsdf" "noClass" ()
  blank
