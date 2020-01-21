module Estuary.Languages.CineCer0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time

import qualified Estuary.Languages.CineCer0.Signals as Sig

import Estuary.Types.Tempo

--type Signal a = Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> a
-- This is in Types already!

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  playbackPosition :: Sig.Signal (Maybe NominalDiffTime),
  playbackRate :: Sig.Signal (Maybe Rational),
  posX :: Rational,
  posY :: Rational,
  width :: Rational,
  height :: Rational,
  opacity :: Sig.Signal Rational,
  blur :: Rational,
  brightness :: Rational,
  contrast :: Rational,
  grayscale :: Rational,
  saturate :: Rational
  }

-- instance Show VideoSpec where
--   show (VideoSpec vs n _ _ px py w h _) = "Sample Video:" ++ show vs ++ " " ++ "Source Number:" ++ show n ++ " " ++ "Position:" ++ show px ++ show py ++ " " ++ "Size:" ++ show w ++ show h ++ " "


emptyVideoSpec :: String -> VideoSpec
emptyVideoSpec x = VideoSpec {
  sampleVideo = "",
  sourceNumber = 0,
  playbackPosition = Sig.playNatural_Pos 0.0,
  playbackRate = Sig.playNatural_Rate 0.0,
  posX = 0.0,
  posY = 0.0,
  width = 0.0,
  height = 0.0,
  opacity = Sig.defaultOpacity,
  blur = 0.0,
  brightness = 100,
  contrast = 100,
  grayscale = 0,
  saturate = 1.0
}

stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0,
  playbackPosition = Sig.playNatural_Pos 0.0,
  playbackRate = Sig.playNatural_Rate 0.0,
  --mask = "none"
  posX = 0.0,
  posY = 0.0,
  width = 1.0,
  height = 1.0,
  opacity = Sig.defaultOpacity,
  blur = 0.0,
  brightness = 100,
  contrast = 100,
  grayscale = 0,
  saturate = 1.0
}

setSourceNumber :: VideoSpec -> Int -> VideoSpec
setSourceNumber vs n = vs { sourceNumber = n }

--
-- Geometric Functions --

setPosX :: Rational -> VideoSpec -> VideoSpec
setPosX n vs = vs { posX = n }

setPosY :: Rational -> VideoSpec -> VideoSpec
setPosY n vs = vs { posY = n }

setPosCoord :: Rational -> Rational -> VideoSpec -> VideoSpec
setPosCoord m n vs = vs { posX = m, posY = n }

setWidth :: Rational -> VideoSpec -> VideoSpec
setWidth n vs = vs { width = n }

setHeight :: Rational -> VideoSpec -> VideoSpec
setHeight n vs = vs { height = n }

setSize :: Rational -> Rational -> VideoSpec -> VideoSpec
setSize m n vs = vs { width = m, height = n }

--
-- Style Functions --

--setOpacity :: Rational -> VideoSpec -> VideoSpec
--setOpacity n vs = vs { opacity = n }

setOpacity :: Rational -> VideoSpec -> VideoSpec
setOpacity r vs = vs {
  opacity = \t ndt rt et -> r * ((opacity vs) t ndt rt et)
  }

-- defaultOpacity :: Tempo -> NominalDiffTime -> UTCTime -> Rational
-- defaultOpacity _ _ _ = 100

-- opacityChanger:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Rational
-- opacityChanger arg t len now = arg

changeOpacity :: Rational -> VideoSpec -> VideoSpec
changeOpacity n vs = vs {
  opacity = Sig.opacityChanger n
}

-- opacity :: Signal Rational -> VideoSpec -> VideoSpec
-- opacity s v = v {
-- 	_opacity = \a b c d -> (s a b c d) * ((_opacity v) a b c d)
-- 	_opacity = s * _opacity v
-- 	}
--
-- setOpacity :: Signal Rational -> VideoSpec -> VideoSpec
-- setOpacity s v = v { _opacity = s }

setBlur :: Rational -> VideoSpec -> VideoSpec
setBlur n vs = vs {blur = n}

setBrightness :: Rational -> VideoSpec -> VideoSpec
setBrightness n vs = vs {brightness = n}

setContranst :: Rational -> VideoSpec -> VideoSpec
setContranst n vs = vs {contrast = n}

setGrayscale :: Rational -> VideoSpec -> VideoSpec
setGrayscale n vs = vs {grayscale = n}

setSaturate :: Rational -> VideoSpec -> VideoSpec
setSaturate n vs = vs {saturate = n}

--
-- Time Functions --

playNatural :: Rational -> VideoSpec -> VideoSpec
playNatural n vs = vs {
  playbackPosition = Sig.playNatural_Pos n,
  playbackRate = Sig.playNatural_Rate n
}

playRound :: Rational -> VideoSpec -> VideoSpec
playRound n vs = vs {
  playbackPosition = Sig.playRound_Pos n,
  playbackRate = Sig.playRound_Rate n
  }

playRoundMetre :: Rational -> VideoSpec -> VideoSpec
playRoundMetre n vs = vs {
  playbackPosition = Sig.playRoundMetrePos n,
  playbackRate = Sig.playRoundMetreRate n
  }

playEvery :: Rational -> Rational -> VideoSpec -> VideoSpec
playEvery m n vs = vs {
  playbackPosition = Sig.playEvery_Pos m n,
  playbackRate = Sig.playEvery_Rate m n
  }

playChop' :: Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop' l m n vs = vs {
  playbackPosition = Sig.playChop_Pos' l m n,
  playbackRate = Sig.playChop_Rate' l m n
  }

playChop :: Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop k l m n vs = vs {
  playbackPosition = Sig.playChop_Pos k l m n,
  playbackRate = Sig.playChop_Rate k l m n
}

playChopSecs :: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec
playChopSecs k l m n vs = vs {
  playbackPosition = Sig.playChopSecs_Pos k l m n,
  playbackRate = Sig.playChopSecs_Rate k l m n
  }

playNow :: NominalDiffTime -> Rational -> VideoSpec -> VideoSpec
playNow m n vs = vs {
  playbackPosition = Sig.playNow_Pos m n,
  playbackRate = Sig.playNow_Rate m n
  }
