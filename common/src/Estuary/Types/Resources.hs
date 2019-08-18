module Estuary.Types.Resources  where

import Data.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Map.Strict (Map)
import Data.Sequence (Seq, (|>))
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

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

emptyResources :: Resources
emptyResources = Resources {
  audioResources = emptyResourceMap,
  videoResources = emptyResourceMap,
  imageResources = emptyResourceMap
}

newtype ResourceMap m = ResourceMap { unResourceMap :: Map Text (Seq (Resource m)) } deriving (Show)
--Text is still resourceGroup (E.G. BUTTERFLIES.)

emptyResourceMap :: ResourceMap m
emptyResourceMap = ResourceMap Map.empty

insertResource :: Text -> Resource m -> ResourceMap m -> ResourceMap m
insertResource groupName resource (ResourceMap resources) =
  let group = (Map.findWithDefault Seq.empty groupName resources) |> resource
  in ResourceMap $ Map.insert groupName (Seq.sortOn resourceFileName group) resources

resolveResource :: ResourceMap m -> Text -> Int -> Maybe (Resource m)
resolveResource (ResourceMap resources) groupName number = do
  group <- Map.lookup groupName resources
  let idx = mod number (Seq.length group) in
    Seq.lookup idx group

resourceList :: ResourceMap m -> [(Resource m)]
resourceList m = F.concatMap F.toList (unResourceMap m)

data Resource m = Resource {
  resourceGroup :: Text,
  resourceFileName :: Text,
  resourceFileSize :: Integer,
  resourceMeta :: m,
  resourceTags :: Seq Text,
  resourceScope :: Scope
} deriving (Show)

data AspectRatio
  = FourOverThree
  | ThreeOverFour
  | SixteenOverNine
  | NineOverSixteen
  | Square
  | Custom Rational -- Int Int
--  deriving (Show)

instance Show AspectRatio where
  show FourOverThree = "4:3"
  show ThreeOverFour = "3:4"
  show SixteenOverNine = "16:9"
  show NineOverSixteen = "9:16"
  show Square = "1:1"
  show (Custom x) = show x

data AudioMeta = AudioMeta { audioDuration :: Double {- seconds -} } deriving (Show)
data VideoMeta = VideoMeta { videoDuration :: Double, videoResolution :: (Int, Int), videoAspectRatio :: AspectRatio } deriving (Show)
data ImageMeta = ImageMeta { imageResolution :: (Int, Int), imageAspectRatio :: AspectRatio } deriving (Show)
