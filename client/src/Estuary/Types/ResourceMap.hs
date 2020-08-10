{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ResourceMap where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Types

import Estuary.Types.Loadable
import Estuary.Types.AudioResource

-- the client uses this, it's a map of URL (metas) plus the actual resources
-- the server and communications protocol will have different representations
type ResourceMap a = Map.Map Location a

type AudioMap = ResourceMap AudioResource

type Location = (Text,Int)

access :: Loadable a => Location -> ResourceMap a -> IO (Either LoadStatus JSVal)
access l m = case (Map.lookup l m) of
  Just x -> load x
  Nothing -> return (Left $ LoadError $ "no resource at location " <> T.pack (show l))
