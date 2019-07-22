module Estuary.Languages.Vide0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time

import Estuary.Types.Tempo

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int
  timeFunction :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime
  } deriving (Show)

--instance Show VideoSpec where
--  show vs n = vs ++ n


stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0
  --timeFunction = T.naturalRate
}


setSampleNumber :: VideoSpec -> Int -> VideoSpec
setSampleNumber vs n = vs { sourceNumber = n }

--playEvery :: Int -> VideoSpec -> VideoSpec
--playEvery n vs = vs { timeFunction = T.playEvery n }
