{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.CineCer0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time
import Data.Text
import TextShow

import Data.Tempo

import Estuary.Languages.CineCer0.Signal

-- object: right is video left is text!!!!!
data ObjectSpec = ObjectSpec {
  object :: Either String String,  ----- both are synonims of Text
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

instance Show ObjectSpec where
  show s = show $ object s

emptyObjectSpec :: ObjectSpec
emptyObjectSpec = ObjectSpec {
  object = Right "",
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

stringToObjectSpec :: String -> ObjectSpec
stringToObjectSpec x = emptyObjectSpec { object = Right x }

xToObjectSpec :: [Char] -> ObjectSpec
xToObjectSpec x = emptyObjectSpec { object = Left x }


-- it should be just five arguments _ _ _ _ _
emptyText :: Signal Text
emptyText _ _ _ _ _ = Data.Text.empty

--
-- Style Functions --

setPosX :: Signal Rational -> ObjectSpec -> ObjectSpec
setPosX s v = v { posX = s }

shiftPosX :: Signal Rational -> ObjectSpec -> ObjectSpec
shiftPosX s v = v {
  posX = s * posX v
  }

setPosY :: Signal Rational -> ObjectSpec -> ObjectSpec
setPosY s v = v { posY = s }

shiftPosY :: Signal Rational -> ObjectSpec -> ObjectSpec
shiftPosY s v = v {
  posY = s * posY v
  }

setCoord :: Signal Rational -> Signal Rational -> ObjectSpec -> ObjectSpec
setCoord s1 s2 vs = vs { posX = s1, posY = s2}

shiftCoord :: Signal Rational -> Signal Rational -> ObjectSpec -> ObjectSpec
shiftCoord s1 s2 vs = vs {
  posX = s1 * posX vs,
  posY = s2 * posY vs
}

setWidth :: Signal Rational -> ObjectSpec -> ObjectSpec
setWidth s v = v { width = s }

shiftWidth :: Signal Rational -> ObjectSpec -> ObjectSpec
shiftWidth s v = v {
  width = s * width v
  }

setHeight :: Signal Rational -> ObjectSpec -> ObjectSpec
setHeight s v = v { height = s }

shiftHeight :: Signal Rational -> ObjectSpec -> ObjectSpec
shiftHeight s v = v {
  height = s * height v
  }

setSize :: Signal Rational -> ObjectSpec -> ObjectSpec
setSize s vs = vs { width = s, height = s}

shiftSize :: Signal Rational -> ObjectSpec -> ObjectSpec
shiftSize s vs = vs {
  width = s * width vs,
  height = s * height vs
}

-- Filters

setFontFamily :: Signal (Maybe String) -> ObjectSpec -> ObjectSpec
setFontFamily s v = v { fontFamily = s }

setFontSize :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setFontSize s v = v { fontSize = s }

setOpacity :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setOpacity s v = v { opacity = s }

shiftOpacity :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
shiftOpacity s v = v {
  opacity = multipleMaybeSignal s (opacity v)
  }

setBlur :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setBlur s v = v { blur = s }

shiftBlur :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
shiftBlur s v = v {
  blur = multipleMaybeSignal s (blur v)
  }

setBrightness :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setBrightness s v = v { brightness = s }

shiftBrightness :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
shiftBrightness s v = v {
  brightness = multipleMaybeSignal s (brightness v)
  }

setContrast :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setContrast s v = v { contrast = s }

shiftContrast :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
shiftContrast s v = v {
  contrast = multipleMaybeSignal s (contrast v)
  }

setGrayscale :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setGrayscale s v = v { grayscale = s }

shiftGrayscale :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
shiftGrayscale s v = v {
  grayscale = multipleMaybeSignal s (grayscale v)
  }

setSaturate :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
setSaturate s v = v { saturate = s }

shiftSaturate :: Signal (Maybe Rational) -> ObjectSpec -> ObjectSpec
shiftSaturate s v = v {
  saturate = multipleMaybeSignal s (saturate v)
  }


-- Masks

circleMask :: Signal Rational -> ObjectSpec -> ObjectSpec
circleMask s vs = vs {
  mask = \a b c d e -> "clip-path:circle(" <> (showt (realToFrac ((
  (((s a b c d e)*71)-71)*(-1)
  ) :: Rational) :: Double)) <> "% at 50% 50%);"
  }

circleMask' :: Signal Rational -> Signal Rational -> Signal Rational -> ObjectSpec -> ObjectSpec
circleMask' m n s vs = vs {
  mask = \a b c d e -> "clip-path:circle(" <> (showt (realToFrac ((
  (((m a b c d e)*71)-71)*(-1)
  ) :: Rational) :: Double)) <> "% at " <> (showt (realToFrac (((n a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((s a b c d e)*100) :: Rational) :: Double)) <> "%);"
  }

sqrMask :: Signal Rational -> ObjectSpec -> ObjectSpec
sqrMask s vs = vs {
  mask = \a b c d e -> "clip-path: inset(" <> (showt (realToFrac (((s a b c d e)*50) :: Rational) :: Double)) <> "%);"
  }

rectMask :: Signal Rational -> Signal Rational -> Signal Rational -> Signal Rational -> ObjectSpec -> ObjectSpec
rectMask m n s t vs = vs {
  mask = \ a b c d e -> "clip-path: inset(" <> (showt (realToFrac (((m a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((n a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((s a b c d e)*100) :: Rational) :: Double)) <> "% " <> (showt (realToFrac (((t a b c d e)*100) :: Rational) :: Double)) <> "%);"
  }

-- audio --  keep it simple just mute, unmute and volume

setMute :: ObjectSpec -> ObjectSpec
setMute v = v { mute = constantSignal True }

setUnmute :: ObjectSpec -> ObjectSpec
setUnmute v = v { mute = constantSignal False}

setVolume:: Signal Rational -> ObjectSpec -> ObjectSpec
setVolume vol v = v { volume = vol }

--
-- Time Functions --

-- anchorTime:: -- parser
quant:: Rational -> Rational -> ObjectSpec -> ObjectSpec
quant nc offset vs = vs { anchorTime = quantAnchor nc offset }

playNatural :: Rational -> ObjectSpec -> ObjectSpec
playNatural n vs = vs {
  playbackPosition = playNatural_Pos n,
  playbackRate = playNatural_Rate n
}

playSnap :: Rational -> ObjectSpec -> ObjectSpec
playSnap n vs = vs {
  playbackPosition = playRound_Pos n,
  playbackRate = playRound_Rate n
  }

playSnapMetre :: Rational -> ObjectSpec -> ObjectSpec
playSnapMetre n vs = vs {
  playbackPosition = playRoundMetre_Pos n,
  playbackRate = playRoundMetre_Rate n
  }

playEvery :: Rational -> Rational -> ObjectSpec -> ObjectSpec
playEvery m n vs = vs {
  playbackPosition = playEvery_Pos m n,
  playbackRate = playEvery_Rate m n
  }

playChop' :: Rational -> Rational -> Rational -> ObjectSpec -> ObjectSpec
playChop' l m n vs = vs {
  playbackPosition = playChop_Pos' l m n,
  playbackRate = playChop_Rate' l m n
  }

playChop :: Rational -> Rational -> Rational -> Rational -> ObjectSpec -> ObjectSpec
playChop k l m n vs = vs {
  playbackPosition = playChop_Pos k l m n,
  playbackRate = playChop_Rate k l m n
}

playChopSecs :: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> ObjectSpec -> ObjectSpec
playChopSecs k l m n vs = vs {
  playbackPosition = playChopSecs_Pos k l m n,
  playbackRate = playChopSecs_Rate k l m n
  }

playNow :: NominalDiffTime -> Rational -> ObjectSpec -> ObjectSpec
playNow m n vs = vs {
  playbackPosition = playNow_Pos m n,
  playbackRate = playNow_Rate m n
  }
