module Estuary.Languages.Vide0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time

import qualified Estuary.Languages.Vide0.PositionAndRate as VT

import Estuary.Types.Tempo

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  playbackPosition :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime,
  playbackRate :: Tempo -> NominalDiffTime -> UTCTime -> Just Rational,
  posX :: Rational,
  posY :: Rational,
  width :: Rational,
  height :: Rational
  }

instance Show VideoSpec where
  show (VideoSpec vs n _ _ px py w h) = "Sample Video:" ++ show vs ++ " " ++ "Source Number:" ++ show n ++ " " ++ "Position:" ++ show px ++ show py ++ " " ++ "Size:" ++ show w ++ show h


stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0,
  playbackPosition = VT.playNatural_Pos,
  playbackRate = VT.playNatural_Rate,
  posX = 0.0,
  posY = 0.0,
  width = 1,
  height = 1
}

setSourceNumber :: VideoSpec -> Int -> VideoSpec
setSourceNumber vs n = vs { sourceNumber = n }

setPosCoord :: Rational -> Rational -> VideoSpec -> VideoSpec
setPosCoord m n vs = vs { posX = m, posY = n }


-- Time Functions --

playEvery :: Rational -> VideoSpec -> VideoSpec
playEvery n vs = vs {
  playbackPosition = VT.playEvery_Pos n,
  playbackRate = VT.playEvery_Rate n
  }

playRound :: VideoSpec -> VideoSpec
playRound vs = vs {
  playbackPosition = VT.playRound_Pos,
  playbackRate = VT.playRound_Rate
  }

playChop' :: Rational -> Rational -> VideoSpec -> VideoSpec
playChop' m n vs = vs {
  playbackPosition = VT.playChop_Pos' m n,
  playbackRate = VT.playChop_Rate' m n
  }

playChop :: Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop l m n vs = vs {
  playbackPosition = VT.playChop_Pos l m n,
  playbackRate = VT.playChop_Rate l m n
}
