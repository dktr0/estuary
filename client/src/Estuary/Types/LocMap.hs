module Estuary.Types.LocMap where

import Data.Map as Map
import Data.IntMap as IntMap
import Data.Text
import Data.Maybe
import Data.List (find)

import Estuary.Types.Location


type LocMap a = Map Text (IntMap a)


empty :: LocMap a
empty = Map.empty


-- a lookup function that wraps around sample n values that are greater
-- than the maximum n value in a given sample bank
lookup :: Location -> LocMap a -> Maybe a
lookup (bankName,n) m = do
  bank <- Map.lookup bankName m
  let maxKey = Prelude.maximum $ IntMap.keys bank
  let n' = n `mod` (maxKey + 1)
  IntMap.lookup n' bank


-- append takes half of a location - just the bank name - and inserts it into the
-- specified bank at the lowest vacant n (>=0), returning the new map
append :: Text -> a -> LocMap a -> LocMap a
append bankName a m = Map.insert bankName newBank m
  where
    bank = Map.findWithDefault IntMap.empty bankName m
    n = maybe 0 id $ Data.List.find (not . (flip elem $ IntMap.keys bank)) [0..]
    newBank = IntMap.insert n a bank
