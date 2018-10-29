{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.EditOrEval where

import Text.JSON
import Text.JSON.Generic
import Estuary.Utility (firstKey)

data EditOrEval a = Edit a | Evaluate a deriving (Show,Eq,Data,Typeable)

instance Data a => JSON (EditOrEval a) where
  showJSON = toJSON
  readJSON = fromJSON
