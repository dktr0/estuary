module Estuary.Types.Resources  where

import Data.Text
import Data.Map.Strict (Map)

import Data.Sequence(Seq)
import Data.Map.Strict (Map)
import Data.Sequence(Seq)
import qualified Data.Foldable as F

import Estuary.Types.Scope

-- {
--   "bd": [
--     {
--       "file": "bd_1.wav",
--       "mimetype": "audio/wav",
--       "length": 1.3,
--       "tags": ["bass", "drum"]
--      },
--     ...
--   ], ...
-- }

-- sent from ensemble, and updated from local
data Resources = Resources {
  audioResources :: ResourceMap AudioMeta,
  videoResources :: ResourceMap VideoMeta,
  imageResources :: ResourceMap ImageMeta
} deriving (Show)

newtype ResourceMap m = ResourceMap { unResourceMap :: Map Text (Seq (Resource m)) } deriving (Show)

resourceList :: ResourceMap m -> [(Resource m)]
resourceList m = F.concatMap F.toList (unResourceMap m)

data Resource m = Resource {
  file :: Text ,
  fileSize :: Integer,
  meta :: m,
  tags :: Seq Text,
  scope :: Scope
} deriving (Show)



data AspectRatio
  = FourOverThree
  | ThreeOverFour
  | SixteenOverNine
  | NineOverSixteen
  | Square
  | Rational Int Int
  | Irrational Double
  deriving (Show)

data AudioMeta = AudioMeta { audioDuration :: Double {- seconds -} } deriving (Show)
data VideoMeta = VideoMeta { videoDuration :: Double, videoResolution :: (Int, Int), videoAspectRatio :: AspectRatio } deriving (Show)
data ImageMeta = ImageMeta { imageResolution :: (Int, Int), imageAspectRatio :: AspectRatio } deriving (Show)
