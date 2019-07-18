module Estuary.Types.Resources where

import Data.Text

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
}

newtype ResourceMap m = ResourceMap { unResourceMap :: Map Text (Seq (Resource m)) }

data Resource m = Resource {
  file :: Text,
  mimetype :: Text,
  fileSize :: Double,
  meta :: m,
  tags :: Seq Text,
  scope :: Scope
}

data AspectRatio
  = FourOverThree
  | ThreeOverFour
  | SixteenOverNine
  | NineOverSixteen
  | Square
  | Rational Int Int
  | Irrational Double

data AudioMeta = AudioMeta { audioDuration :: Double }
data VideoMeta = VideoMeta { videoDuration :: Double, videoResolution :: (Int, Int), videoAspectRatio :: AspectRatio }
data ImageMeta = ImageMeta { imageResolution :: (Int, Int), imageAspectRatio :: AspectRatio }