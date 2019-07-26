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
  --mask :: String,
  posX :: Rational,
  posY :: Rational,
  width :: Rational,
  height :: Rational,
  red :: Rational,
  green :: Rational,
  blue :: Rational,
  alpha :: Rational,
  hue :: Rational,
  saturation :: Rational

  }

instance Show VideoSpec where
  show (VideoSpec vs n _ _ px py w h r g b a _ _) = "Sample Video:" ++ show vs ++ " " ++ "Source Number:" ++ show n ++ " " ++ "Position:" ++ show px ++ show py ++ " " ++ "Size:" ++ show w ++ show h ++ " " ++ "Color:" ++ show r ++ show g ++ show b ++ " " ++ "Alpha " ++ show a


stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0,
  playbackPosition = VT.playNatural_Pos 0.0,
  playbackRate = VT.playNatural_Rate 0.0,
  --mask = "none"
  posX = 0.0,
  posY = 0.0,
  width = 1.0,
  height = 1.0,
  red = 1.0,
  green = 1.0,
  blue = 1.0,
  alpha = 1.0,
  hue = 0.0,
  saturation = 0.0
}

setSourceNumber :: VideoSpec -> Int -> VideoSpec
setSourceNumber vs n = vs { sourceNumber = n }

--maskVideo :: String -> VideoSpec -> VideoSpec
--maskVideo s vs = vs { mask = s }


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

-- colors --

setRGB :: Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
setRGB l m n vs = vs { red = l, green = m, blue = n }

setHue :: Rational -> VideoSpec -> VideoSpec
setHue n vs = vs { hue = n }

setSaturation :: Rational -> VideoSpec -> VideoSpec
setSaturation n vs = vs { saturation = n }


-- Set alpha --

setAlpha :: Rational -> VideoSpec -> VideoSpec
setAlpha n vs = vs { alpha = n }

--
-- Time Functions --

playNatural :: Rational -> VideoSpec -> VideoSpec
playNatural n vs = vs {
  playbackPosition = VT.playNatural_Pos n,
  playbackRate = VT.playNatural_Rate n
}

playEvery :: Rational -> Rational -> VideoSpec -> VideoSpec
playEvery m n vs = vs {
  playbackPosition = VT.playEvery_Pos m n,
  playbackRate = VT.playEvery_Rate m n
  }

playRound :: Rational -> VideoSpec -> VideoSpec
playRound n vs = vs {
  playbackPosition = VT.playRound_Pos n,
  playbackRate = VT.playRound_Rate n
  }

playChop' :: Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop' l m n vs = vs {
  playbackPosition = VT.playChop_Pos' l m n,
  playbackRate = VT.playChop_Rate' l m n
  }

playChop :: Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop k l m n vs = vs {
  playbackPosition = VT.playChop_Pos k l m n,
  playbackRate = VT.playChop_Rate k l m n
}

playChopSecs :: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec
playChopSecs k l m n vs = vs {
  playbackPosition = VT.playChopSecs_Pos k l m n,
  playbackRate = VT.playChopSecs_Rate k l m n
  }

playNow :: NominalDiffTime -> Rational -> VideoSpec -> VideoSpec
playNow m n vs = vs {
  playbackPosition = VT.playNow_Pos m n,
  playbackRate = VT.playNow_Rate m n
  }
