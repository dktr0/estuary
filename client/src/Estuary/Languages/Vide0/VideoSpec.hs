module Estuary.Languages.Vide0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time
import qualified Estuary.Languages.Vide0.PositionAndRate as T

import Estuary.Types.Tempo

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  timePosition :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime,
  timeRate :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime
  }

--instance Show VideoSpec where
--  show vs s p r =


stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0,
  timePosition = T.playNatural_Pos,
  timeRate = T.playNatural_Rate
}

setSourceNumber :: VideoSpec -> Int -> VideoSpec
setSourceNumber vs n = vs { sourceNumber = n }


playEvery :: Int -> VideoSpec -> VideoSpec
playEvery n vs = vs {
  timePosition = T.playEvery_Pos n,
  timeRate = T.playEvery_Rate n
  }
