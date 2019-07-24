module Estuary.Languages.Vide0.Parser (vide0) where

import Language.Haskell.Exts
import Control.Applicative

import Estuary.Languages.ExpParser
import Estuary.Languages.Vide0.VideoSpec


vide0 :: String -> Either String VideoSpec
vide0 = f . parseExp
  where
    f (ParseOk x) = runExpParser videoSpec x
    f (ParseFailed l s) = Left s


videoSpec :: ExpParser VideoSpec
videoSpec =
  literalVideoSpec <|>
  int_VideoSpec <*> int <|>
  videoSpec_videoSpec <*> videoSpec

--
-- Set Video with default options --

literalVideoSpec :: ExpParser VideoSpec
literalVideoSpec = fmap stringToVideoSpec string
-- fmap :: (a->b) -> f a -> f b
-- entonces con el fmap estamos usando (a -> b) que es igual a String -> VideoSpec que es igual a stringToVideoSpec, después le damos el f a que es el string y nos va a devolver f b que es ExpParser VideoSpec

--
-- Set Source Number --

int :: ExpParser Int
int = fromIntegral <$> integer

videoSpec_int_videoSpec :: ExpParser (VideoSpec -> Int -> VideoSpec)
videoSpec_int_videoSpec = setSourceNumber <$ reserved ":"

int_VideoSpec :: ExpParser (Int -> VideoSpec)
int_VideoSpec = videoSpec_int_videoSpec <*> videoSpec

--
-- ExpParser (Rational -> VideoSpec -> VideoSpec) --

rat_videoSpec_videoSpec :: ExpParser (Rational -> VideoSpec -> VideoSpec)
rat_videoSpec_videoSpec =
  playEvery <$ reserved "playEvery" <|> --time function
  setWidth <$ reserved "w" <|>
  setHeight <$ reserved "h" <|>
  setPosX <$ reserved "posX" <|>
  setPosY <$ reserved "posY"

--
-- ExpParser (Rational -> Rational -> VideoSpec -> VideoSpec) --

rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_videoSpec_videoSpec =
  setPosCoord <$ reserved "pos" <|> -- position video coordinates
  playChop' <$ reserved "playChop'" <|> -- time function
  setSize <$ reserved "size" --size (w h) amounts

  --
  -- ExpParser (Rational -> Rational -> Rational -> VideoSpec -> VideoSpec) --

rat_rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_rat_videoSpec_videoSpec = playChop <$ reserved "playChop" -- time function

--
-- ExpParser (VideoSpec -> VideoSpec) --

videoSpec_videoSpec :: ExpParser (VideoSpec -> VideoSpec)
videoSpec_videoSpec =
  playRound <$ reserved "playRound" <|> -- time function
  rat_videoSpec_videoSpec <*> rational <|> -- time function
  rat_rat_videoSpec_videoSpec <*> rational <*> rational <|> -- pos function
  rat_rat_rat_videoSpec_videoSpec <*> rational <*> rational <*> rational -- time function
