module Estuary.Languages.CineCer0.Parser (cineCer0,CineCer0Spec) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time

import Estuary.Languages.ExpParser
import Estuary.Languages.CineCer0.VideoSpec

type CineCer0Spec = IntMap VideoSpec

cineCer0 :: String -> Either String CineCer0Spec
cineCer0 s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runExpParser cineCer0Spec x
    f (ParseFailed l s) = Left s

cineCer0Spec :: ExpParser CineCer0Spec
cineCer0Spec = fmap (fromList . zip [0..]) $ listOfDoStatements videoSpec


videoSpec :: ExpParser VideoSpec
videoSpec =
  literalVideoSpec <|>
  int_VideoSpec <*> int <|>
  videoSpec_videoSpec <*> videoSpec


int :: ExpParser Int
int = fromIntegral <$> integer

nominalDiffTime :: ExpParser NominalDiffTime
nominalDiffTime = fromRational <$> rationalOrInteger

--
-- Set Video with default options --

literalVideoSpec :: ExpParser VideoSpec
literalVideoSpec = fmap stringToVideoSpec string
-- fmap :: (a->b) -> f a -> f b
-- entonces con el fmap estamos usando (a -> b) que es igual a String -> VideoSpec que es igual a stringToVideoSpec, después le damos el f a que es el string y nos va a devolver f b que es ExpParser VideoSpec

--
-- Set Source Number --

videoSpec_int_videoSpec :: ExpParser (VideoSpec -> Int -> VideoSpec)
videoSpec_int_videoSpec = setSourceNumber <$ reserved ":"

int_VideoSpec :: ExpParser (Int -> VideoSpec)
int_VideoSpec = videoSpec_int_videoSpec <*> videoSpec

--
-- Mask a Video --

--string_VideoSpec_VideoSpec :: ExpParser (String -> VideoSpec -> VideoSpec)
--string_VideoSpec_VideoSpec = maskVideo <$ reserved "mask"


rat_videoSpec_videoSpec :: ExpParser (Rational -> VideoSpec -> VideoSpec)
rat_videoSpec_videoSpec =
  playNatural <$ reserved "playNatural" <|> --time function
  playRound <$ reserved "playRound" <|> -- time function
  setWidth <$ reserved "w" <|>
  setHeight <$ reserved "h" <|>
  setPosX <$ reserved "posX" <|>
  setPosY <$ reserved "posY" <|>
  setAlpha <$ reserved "alpha" <|>
  setHue <$ reserved "hue" <|>
  setSaturation <$ reserved "saturation"


rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_videoSpec_videoSpec =
  playEvery <$ reserved "playEvery" <|> --time function
  setPosCoord <$ reserved "pos" <|> -- position video coordinates
  setSize <$ reserved "size" --size (w h) amounts


rat_rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_videoSpec_videoSpec =
  playChop' <$ reserved "playChop'" <|> -- time function
  setRGB <$ reserved "color"


rat_rat_rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_rat_videoSpec_videoSpec =
  playChop <$ reserved "playChop" -- time function


nd_rat_videoSpec_videoSpec :: ExpParser (NominalDiffTime -> Rational -> VideoSpec -> VideoSpec)
nd_rat_videoSpec_videoSpec = playNow <$ reserved "playNow" -- time function


nd_nd_rat_rat_videoSpec_videoSpec :: ExpParser (NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> VideoSpec -> VideoSpec)
nd_nd_rat_rat_videoSpec_videoSpec = playChopSecs <$ reserved "playChopSecs" -- time function


videoSpec_videoSpec :: ExpParser (VideoSpec -> VideoSpec)
videoSpec_videoSpec =
  --string_VideoSpec_VideoSpec <*> string <|> --mask function
  rat_videoSpec_videoSpec <*> rationalOrInteger <|> -- time function
  rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <|> -- pos function
  rat_rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <|> -- time function
  rat_rat_rat_rat_videoSpec_videoSpec <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <*> rationalOrInteger <|> -- time function
  nd_rat_videoSpec_videoSpec <*> nominalDiffTime <*> rationalOrInteger <|> -- time function
  nd_nd_rat_rat_videoSpec_videoSpec <*> nominalDiffTime <*> nominalDiffTime <*> rationalOrInteger <*> rationalOrInteger -- time function
