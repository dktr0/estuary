module Estuary.Languages.CineCer0.Spec (Spec(..),emptySpec) where

import Data.IntMap.Strict
import Data.Time

import Estuary.Languages.CineCer0.VideoSpec

data Spec = Spec {
  evalTime :: UTCTime,
  layerSpecMap :: IntMap LayerSpec 
  }

instance Show Spec where
  show s = show (layerSpecMap s)

emptySpec :: UTCTime -> Spec
emptySpec t = Spec {
  evalTime = t,
  layerSpecMap = empty
  }
