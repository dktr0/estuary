module Estuary.Languages.CineCer0.Parser (cineCer0) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time
import Language.Haskellish

import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Spec

cineCer0 :: UTCTime -> String -> Either String Spec
cineCer0 eTime s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runHaskellish (spec eTime) x
    f (ParseFailed l s) = Left s

spec :: UTCTime -> Haskellish Spec
spec eTime = do
  vs <- fmap (fromList . zip [0..]) $ listOfDoStatements videoSpec
  return $ Spec {
    evalTime = eTime,
    videoSpecMap = vs
  }

videoSpec :: Haskellish VideoSpec
videoSpec =
  literalVideoSpec <|>
  int_VideoSpec <*> int <|>
  videoSpec_videoSpec <*> videoSpec

int :: Haskellish Int
int = fromIntegral <$> integer

nominalDiffTime :: Haskellish NominalDiffTime
nominalDiffTime = fromRational <$> rationalOrInteger

literalVideoSpec :: Haskellish VideoSpec
literalVideoSpec =
  fmap stringToVideoSpec string <|>
  fmap emptyVideoSpec string

videoSpec_int_videoSpec :: Haskellish (VideoSpec -> Int -> VideoSpec)
videoSpec_int_videoSpec = setSourceNumber <$ reserved ":"

int_VideoSpec :: Haskellish (Int -> VideoSpec)
int_VideoSpec = videoSpec_int_videoSpec <*> videoSpec

rat_videoSpec_videoSpec :: Haskellish (Rational -> VideoSpec -> VideoSpec)
rat_videoSpec_videoSpec =
  playNatural <$ reserved "natural" <|> --time function
  playRound <$ reserved "round" <|> -- time function
  playRoundMetre <$ reserved "roundMetre" <|> -- time function
  setWidth <$ reserved "w" <|>
  setHeight <$ reserved "h" <|>
  setPosX <$ reserved "posX" <|>
  setPosY <$ reserved "posY" <|>
  setOpacity <$ reserved "opacity" <|>
  setBlur <$ reserved "blur" <|>
  setBrightness <$ reserved "brightness" <|>
  setContranst <$ reserved "contrast" <|>
  setGrayscale <$ reserved "grayscale" <|>
  setSaturate <$ reserved "saturate"

rat_rat_videoSpec_videoSpec :: Haskellish (Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_videoSpec_videoSpec =
  playEvery <$ reserved "every" <|> --time function
  setPosCoord <$ reserved "pos" <|>
  setSize <$ reserved "size"

rat_rat_rat_videoSpec_videoSpec :: Haskellish (Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_videoSpec_videoSpec =
  playChop' <$ reserved "chop'" -- time function

rat_rat_rat_rat_videoSpec_videoSpec :: Haskellish (Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_rat_videoSpec_videoSpec =
  playChop <$ reserved "chop" -- time function

nd_rat_videoSpec_videoSpec :: Haskellish (NominalDiffTime -> Rational -> VideoSpec -> VideoSpec)
nd_rat_videoSpec_videoSpec = playNow <$ reserved "now" -- time function

nd_nd_rat_rat_videoSpec_videoSpec :: Haskellish (NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec)
nd_nd_rat_rat_videoSpec_videoSpec = playChopSecs <$ reserved "chopSecs" -- time function

videoSpec_videoSpec :: Haskellish (VideoSpec -> VideoSpec)
videoSpec_videoSpec =
  --string_VideoSpec_VideoSpec <*> string <|> --mask function
  rat_videoSpec_videoSpec <*> rationalOrInteger <|> -- time function
  rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <|> -- pos function
  rat_rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <|> -- time function
  rat_rat_rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <|> -- time function
  nd_rat_videoSpec_videoSpec <*> nominalDiffTime <*> rationalOrInteger <|> -- time function
  nd_nd_rat_rat_videoSpec_videoSpec <*> nominalDiffTime <*> nominalDiffTime <*> rationalOrInteger <*> rationalOrInteger -- time function
