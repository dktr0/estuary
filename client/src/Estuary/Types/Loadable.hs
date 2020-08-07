module Estuary.Types.Loadable where

import GHCJS.Types
import Data.Text

class Loadable a where
  load :: a -> IO ()
  access :: a -> IO (Either LoadStatus JSVal)

data LoadStatus =
  NotLoaded |
  Loading |
  Loaded |
  LoadError Text deriving (Eq)
