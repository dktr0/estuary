module Estuary.Types.LocMap where

import Data.Map as Map
import Data.IntMap as IntMap
import Data.Text
import Data.Maybe
import Data.List (find)

import Estuary.Types.Location

-- A LocMap is a Map-like structure specialized for the case where the keys/indices are Location-s ie. (Text,Int)
-- This allows lookup and append operations to be defined that take account of common expectations around sample banks
-- (eg. that the Int part of a Location is subject to "wraparound", and that we might want to append samples to the
-- "end" of an existing samplebank).

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


-- insert takes a location and an item to insert and inserts the item at that
-- location, overwriting any previous item at that location
insert :: Location -> a -> LocMap a -> LocMap a
insert (bankName,n) a m = Map.insert bankName newBank m
  where
    bank = Map.findWithDefault IntMap.empty bankName m
    newBank = IntMap.insert n a bank


-- delete takes a location and deletes the item at that location
delete :: Location -> LocMap a -> LocMap a
delete (bankName,n) m = Map.insert bankName newBank m
  where
    bank = Map.findWithDefault IntMap.empty bankName m
    newBank = IntMap.delete n bank
