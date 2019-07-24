module Estuary.Languages.CineCer0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time

import qualified Estuary.Languages.CineCer0.PositionAndRate as VT

import Estuary.Types.Tempo

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  playbackPosition :: Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime,
  playbackRate :: Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational,
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

-- Video posX, posY, and posCoord (x y)

setPosX :: Rational -> VideoSpec -> VideoSpec
setPosX n vs = vs { posX = n }

setPosY :: Rational -> VideoSpec -> VideoSpec
setPosY n vs = vs { posY = n }

setPosCoord :: Rational -> Rational -> VideoSpec -> VideoSpec
setPosCoord m n vs = vs { posX = m, posY = n }

-- Video width, height, and size (width height) --

setWidth :: Rational -> VideoSpec -> VideoSpec
setWidth n vs = vs { width = n }

setHeight :: Rational -> VideoSpec -> VideoSpec
setHeight n vs = vs { height = n }

setSize :: Rational -> Rational -> VideoSpec -> VideoSpec
setSize m n vs = vs { width = m, height = n }

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
