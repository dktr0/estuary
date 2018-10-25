{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Sited where

import Text.JSON
import Text.JSON.Generic
import Data.Maybe

data Sited a b = Sited {
  site :: a,
  thing :: b
  } deriving (Eq,Data,Typeable)

instance (Data a,Data b) => JSON (Sited a b) where
  showJSON = toJSON
  readJSON = fromJSON

justSited :: Eq a => a -> [Sited a b] -> [b]
justSited x1 = mapMaybe f
  where
    f (Sited x2 y) | x1 == x2 = Just y
    f _ = Nothing
