module Estuary.Languages.CineCer0.Parser (cineCer0) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time
import Language.Haskellish
import Data.Bifunctor

import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Spec
import Estuary.Languages.CineCer0.Signal

type H = Haskellish ()

cineCer0 :: UTCTime -> String -> Either String Spec
cineCer0 eTime s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = second fst $ runHaskellish (spec eTime) () x
    f (ParseFailed l s) = Left s

spec :: UTCTime -> H Spec
spec eTime = do
  vs <- fmap (fromList . zip [0..]) $ listOfDoStatements videoSpec
  return $ Spec {
    evalTime = eTime,
    videoSpecMap = vs
  }

videoSpec :: H VideoSpec
videoSpec =
  vs_vs <*> videoSpec <|>
  literalVideoSpec

-- //////////////

int :: H Int
int = fromIntegral <$> integer

ndt :: H NominalDiffTime
ndt = fromRational <$> rationalOrInteger

literalVideoSpec :: H VideoSpec
literalVideoSpec =
  fmap stringToVideoSpec string <|>
  emptyVideoSpec <$ reserved ""

-- //////////////

sigRat :: H (Signal Rational)
sigRat =
  rat_sigRat <*> rationalOrInteger <|>
  constantSignal <$> rationalOrInteger

rat_sigRat :: H (Rational -> Signal Rational)
rat_sigRat = rat_rat_sigRat <*> rationalOrInteger

rat_rat_sigRat :: H (Rational -> Rational -> Signal Rational)
rat_rat_sigRat = ndt_rat_rat_sigRat <*> ndt 

rat_rat_UTCTime:: H (Rational -> Rational -> Signal UTCTime) -- this is new ¡!
rat_rat_UTCTime = quant <$ reserved "quant" -- quant is setting the anchor time

ndt_rat_rat_sigRat :: H (NominalDiffTime -> Rational -> Rational -> Signal Rational)
ndt_rat_rat_sigRat = ramp <$ reserved "ramp"

-- //////////////

vs_vs :: H (VideoSpec -> VideoSpec)
vs_vs =
  sigRat_vs_vs <*> sigRat <|>
  rat_vs_vs <*> rationalOrInteger

sigRat_vs_vs :: H (Signal Rational -> VideoSpec -> VideoSpec)
sigRat_vs_vs =
  setPosX <$ reserved "setPosX" <|>
  shiftPosX <$ reserved "posX" <|>
  setPosY <$ reserved "setPosY" <|>
  shiftPosY <$ reserved "posX" <|>
  setWidth <$ reserved "setWidth" <|>
  shiftWidth <$ reserved "width" <|>
  setHeight <$ reserved "setHeight" <|>
  shiftHeight <$ reserved "height" <|>
  setOpacity <$ reserved "setOpacity" <|>
  shiftOpacity <$ reserved "opacity" <|>
  setBlur <$ reserved "setBlur" <|>
  shiftBlur <$ reserved "blur" <|>
  setBrightness <$ reserved "setBrightness" <|>
  shiftBrightness <$ reserved "brightness" <|>
  setContrast <$ reserved "setContrast" <|>
  shiftContrast <$ reserved "contrast" <|>
  setGrayscale <$ reserved "setGrayscale" <|>
  shiftGrayscale <$ reserved "grayscale" <|>
  setSaturate <$ reserved "setSaturate" <|>
  shiftSaturate <$ reserved "saturate" <|>
  setSize <$ reserved "setSize" <|>
  shiftSize <$ reserved "size" <|>
  sigRat_sigRat_vs_vs <*> sigRat

sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> VideoSpec -> VideoSpec)
sigRat_sigRat_vs_vs =
  setCoord <$ reserved "setCoord" <|>
  shiftCoord <$ reserved "coord"

rat_vs_vs :: H (Rational -> VideoSpec -> VideoSpec)
rat_vs_vs =
  playNatural <$ reserved "natural" <|>
  playRound <$ reserved "round" <|>
  playRoundMetre <$ reserved "roundMetre" <|>
  rat_rat_vs_vs <*> rationalOrInteger <|>
  ndt_rat_vs_vs <*> ndt

rat_rat_vs_vs :: H (Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_vs_vs =
  playEvery <$ reserved "every" <|>
  rat_rat_rat_vs_vs <*> rationalOrInteger <|>
  ndt_rat_rat_vs_vs <*> ndt <|>
--  quant <$ reserved "quant" -- esto es igual a: fmap . const == fmap (quant) . "quant"  ¿?
--  (reserved "quant" >> return quant) -- Look here! parenthesis make this work properly

rat_rat_rat_vs_vs :: H (Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_vs_vs =
  playChop' <$ reserved "chop'" <|>
  rat_rat_rat_rat_vs_vs <*> rationalOrInteger

rat_rat_rat_rat_vs_vs :: H (Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_rat_vs_vs =
  playChop <$ reserved "chop"

ndt_rat_vs_vs :: H (NominalDiffTime -> Rational -> VideoSpec -> VideoSpec)
ndt_rat_vs_vs =
  playNow <$ reserved "now"

ndt_rat_rat_vs_vs :: H (NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec)
ndt_rat_rat_vs_vs =
  ndt_ndt_rat_rat_vs_vs <*> ndt

ndt_ndt_rat_rat_vs_vs :: H (NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec)
ndt_ndt_rat_rat_vs_vs = playChopSecs <$ reserved "chopSecs" -- time function
