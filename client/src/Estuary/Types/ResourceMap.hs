{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ResourceMap where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.Types

import Estuary.Types.Loadable
import Estuary.Types.AudioMeta
import Estuary.Types.AudioResource
import Estuary.Types.Samples

-- the client uses this, it's a map of URL (metas) plus the actual resources
-- the server and communications protocol will have different representations
type ResourceMap a = Map.Map Location a

type AudioMap = ResourceMap AudioResource

type Location = (Text,Int)

access :: Loadable a => Location -> ResourceMap a -> IO (Either LoadStatus JSVal)
access l m = do
  let l' = adjustedLocation l m
  case (Map.lookup l' m) of
    Just x -> do
      ls <- loadStatus x
      case ls of
        LoadError e -> return $ Left $ LoadError e
        otherwise -> Right <$> load x
    Nothing -> return (Left $ LoadError $ "no resource at location " <> T.pack (show l))

-- note: this might be more efficient with a different representation for ResourceMap
adjustedLocation :: Location -> ResourceMap a -> Location
adjustedLocation (bankName,n) m = (bankName,n')
  where
    ks = Map.keys $ Map.mapKeys snd $ Map.filterWithKey (\(k,_) a -> k == bankName) m
    n' = case ks of
      [] -> 0
      xs -> n `mod` (maximum ks + 1)


-- append takes half of a location - just the bank name - and inserts it into the
-- specified bank at the lowest vacant n (>=0), returning the new map
append :: Text -> a -> ResourceMap a -> ResourceMap a
append bankName a m = Map.insert (bankName,n) a m
  where
    prevNs = fmap snd $ Prelude.filter (\(x,y) -> x == bankName) $ Map.keys m
    n = maybe 0 id $ find (not . (flip elem $ prevNs)) [0..]

emptyResourceMap :: ResourceMap a
emptyResourceMap = Map.empty

-- SampleMap (defined in Estuary.Types.Samples) represents the legacy WebDirt sampleMap
-- we provide here a function that casts that to the newer AudioMap type so that it
-- can be the basis for amendments, additions, experiments

sampleMapToAudioMap :: SampleMap -> IO AudioMap
sampleMapToAudioMap sm = mapM audioResourceFromMeta c
  where
    a = fmap ((zip [0..]) . toList) $ unSampleMap sm -- :: Map Text [(Int,Text)]
    b = concat $ Map.elems $ Map.mapWithKey (\x ys -> fmap (\(y,url) -> ((x,y),url)) ys) a
    c = fmap (\url -> AudioMeta ("samples/" <> url) 0.0) $ Map.fromList b
