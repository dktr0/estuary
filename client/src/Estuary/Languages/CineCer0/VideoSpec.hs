{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.CineCer0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time
import Data.Text
import TextShow

import Data.Tempo

import Estuary.Languages.CineCer0.Signal

type Video = String
type Texto = String

-- sampleVideo

data VideoSpec = VideoSpec {
  textOrVideo :: Either Texto Video,  ----- both are synonims of Text
  anchorTime :: (Tempo -> UTCTime -> UTCTime),
  playbackPosition :: Signal (Maybe NominalDiffTime),
  playbackRate :: Signal (Maybe Rational),
  mute :: Signal Bool,
  volume :: Signal Rational,
  posX :: Signal Rational,
  posY :: Signal Rational,
  width :: Signal Rational,
  height :: Signal Rational,
  opacity :: Signal (Maybe Rational),
  blur :: Signal (Maybe Rational),
  brightness :: Signal (Maybe Rational),
  contrast :: Signal (Maybe Rational),
  grayscale :: Signal (Maybe Rational),
  saturate :: Signal (Maybe Rational),
  fontFamily :: Signal (Maybe String),
  fontSize :: Signal (Maybe Rational),
  mask :: Signal Text
  }

instance Show VideoSpec where
  show s = show $ textOrVideo s

emptyVideoSpec :: VideoSpec
emptyVideoSpec = VideoSpec {
  textOrVideo = Right "",
  anchorTime = defaultAnchor,
  playbackPosition = playNatural_Pos 0.0,
  playbackRate = playNatural_Rate 0.0,
  mute = constantSignal True,
  volume = constantSignal 0.0,
  posX = constantSignal 0.0,
  posY = constantSignal 0.0,
  width = constantSignal 1.0,
  height = constantSignal 1.0,
  opacity = constantSignal' Nothing,
  blur = constantSignal Nothing,
  brightness = constantSignal Nothing,
  contrast = constantSignal Nothing,
  grayscale = constantSignal Nothing,
  saturate = constantSignal Nothing,
  fontFamily = constantSignal Nothing,
  fontSize = constantSignal Nothing,
  mask = emptyText
}

stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = emptyVideoSpec { textOrVideo = Right x }


-- it should be just five arguments _ _ _ _ _
emptyText :: Signal Text
emptyText _ _ _ _ _ = Data.Text.empty

--
-- Style Functions --

setPosX :: Signal Rational -> VideoSpec -> VideoSpec
setPosX s v = v { posX = s }

shiftPosX :: Signal Rational -> VideoSpec -> VideoSpec
shiftPosX s v = v {
  posX = s * posX v
  }

setPosY :: Signal Rational -> VideoSpec -> VideoSpec
setPosY s v = v { posY = s }

shiftPosY :: Signal Rational -> VideoSpec -> VideoSpec
shiftPosY s v = v {
  posY = s * posY v
  }

setCoord :: Signal Rational -> Signal Rational -> VideoSpec -> VideoSpec
setCoord s1 s2 vs = vs { posX = s1, posY = s2}

shiftCoord :: Signal Rational -> Signal Rational -> VideoSpec -> VideoSpec
shiftCoord s1 s2 vs = vs {
  posX = s1 * posX vs,
  posY = s2 * posY vs
}

setWidth :: Signal Rational -> VideoSpec -> VideoSpec
setWidth s v = v { width = s }

shiftWidth :: Signal Rational -> VideoSpec -> VideoSpec
shiftWidth s v = v {
  width = s * width v
  }

setHeight :: Signal Rational -> VideoSpec -> VideoSpec
setHeight s v = v { height = s }

shiftHeight :: Signal Rational -> VideoSpec -> VideoSpec
shiftHeight s v = v {
  height = s * height v
  }

setSize :: Signal Rational -> VideoSpec -> VideoSpec
setSize s vs = vs { width = s, height = s}

shiftSize :: Signal Rational -> VideoSpec -> VideoSpec
shiftSize s vs = vs {
  width = s * width vs,
  height = s * height vs
}

-- Filters

setFontFamily :: Signal (Maybe String) -> VideoSpec -> VideoSpec
setFontFamily s v = v { fontFamily = s }

setFontSize :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setFontSize s v = v { fontSize = s }

setOpacity :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setOpacity s v = v { opacity = s }

shiftOpacity :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
shiftOpacity s v = v {
  opacity = multipleMaybeSignal s (opacity v)
  }

setBlur :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setBlur s v = v { blur = s }

shiftBlur :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
shiftBlur s v = v {
  blur = multipleMaybeSignal s (blur v)
  }

setBrightness :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setBrightness s v = v { brightness = s }

shiftBrightness :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
shiftBrightness s v = v {
  brightness = multipleMaybeSignal s (brightness v)
  }

setContrast :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setContrast s v = v { contrast = s }

shiftContrast :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
shiftContrast s v = v {
  contrast = multipleMaybeSignal s (contrast v)
  }

setGrayscale :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setGrayscale s v = v { grayscale = s }

shiftGrayscale :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
shiftGrayscale s v = v {
  grayscale = multipleMaybeSignal s (grayscale v)
  }

setSaturate :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
setSaturate s v = v { saturate = s }

shiftSaturate :: Signal (Maybe Rational) -> VideoSpec -> VideoSpec
shiftSaturate s v = v {
  saturate = multipleMaybeSignal s (saturate v)
  }


-- Masks

circleMask :: Signal Rational -> VideoSpec -> VideoSpec
circleMask s vs = vs {
  mask = \a b c d e -> "clip-path:circle(" <> (showt (realToFrac ((
  (((s a b c d e)*71)-71)*(-1)
  ) :: Rational) :: Double)) <> "% at 50% 50%);"
  }

circleMask' :: Signal Rational -> Signal Rational -> Signal Rational -> VideoSpec -> VideoSpec
circleMask' m n s vs = vs {
  mask = \a b c d e -> "clip-path:circle(" <> (showt (realToFrac ((
  (((m a b c d e)*71)-71)*(-1)
  ) :: Rational) :: Double)) <> "% at " <> (showt (realToFrac (((n a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((s a b c d e)*100) :: Rational) :: Double)) <> "%);"
  }

sqrMask :: Signal Rational -> VideoSpec -> VideoSpec
sqrMask s vs = vs {
  mask = \a b c d e -> "clip-path: inset(" <> (showt (realToFrac (((s a b c d e)*50) :: Rational) :: Double)) <> "%);"
  }

rectMask :: Signal Rational -> Signal Rational -> Signal Rational -> Signal Rational -> VideoSpec -> VideoSpec
rectMask m n s t vs = vs {
  mask = \ a b c d e -> "clip-path: inset(" <> (showt (realToFrac (((m a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((n a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((s a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((t a b c d e)*100) :: Rational) :: Double)) <> "%);"
  }

-- audio --  keep it simple just mute, unmute and volume

setMute :: VideoSpec -> VideoSpec
setMute v = v { mute = constantSignal True }

setUnmute :: VideoSpec -> VideoSpec
setUnmute v = v { mute = constantSignal False}

setVolume:: Signal Rational -> VideoSpec -> VideoSpec
setVolume vol v = v { volume = vol }

--
-- Time Functions --

-- anchorTime:: -- parser
quant:: Rational -> Rational -> VideoSpec -> VideoSpec
quant nc offset vs = vs { anchorTime = quantAnchor nc offset }

playNatural :: Rational -> VideoSpec -> VideoSpec
playNatural n vs = vs {
  playbackPosition = playNatural_Pos n,
  playbackRate = playNatural_Rate n
}

playSnap :: Rational -> VideoSpec -> VideoSpec
playSnap n vs = vs {
  playbackPosition = playRound_Pos n,
  playbackRate = playRound_Rate n
  }

playSnapMetre :: Rational -> VideoSpec -> VideoSpec
playSnapMetre n vs = vs {
  playbackPosition = playRoundMetre_Pos n,
  playbackRate = playRoundMetre_Rate n
  }

playEvery :: Rational -> Rational -> VideoSpec -> VideoSpec
playEvery m n vs = vs {
  playbackPosition = playEvery_Pos m n,
  playbackRate = playEvery_Rate m n
  }

playChop' :: Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop' l m n vs = vs {
  playbackPosition = playChop_Pos' l m n,
  playbackRate = playChop_Rate' l m n
  }

playChop :: Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec
playChop k l m n vs = vs {
  playbackPosition = playChop_Pos k l m n,
  playbackRate = playChop_Rate k l m n
}

playChopSecs :: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec
playChopSecs k l m n vs = vs {
  playbackPosition = playChopSecs_Pos k l m n,
  playbackRate = playChopSecs_Rate k l m n
  }

playNow :: NominalDiffTime -> Rational -> VideoSpec -> VideoSpec
playNow m n vs = vs {
  playbackPosition = playNow_Pos m n,
  playbackRate = playNow_Rate m n
  }
