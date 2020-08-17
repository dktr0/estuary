module Estuary.Types.Loadable where

import GHCJS.Types
import Data.Text

-- Instances of the class Loadable represent resources that can be asynchronously loaded
-- by the client (eg. turned into a JSVal that can be used in rendering). load will:
-- a) return the corresponding JSVal if it is already available
-- b) trigger asynchronous loading and return NotLoaded if it hadn't been requested before
-- c) return Loading if loading is still in progress
-- d) return LoadError and a text message if there was an error loading this resource
-- The default idea is that attempts to load a Loadable value that returns an error will
-- not trigger any reattempts at loading, but this could change in the future.  

class Loadable a where
  load :: a -> IO (Either LoadStatus JSVal)

data LoadStatus =
  NotLoaded |
  Loading |
  Loaded |
  LoadError Text deriving (Eq,Show)
