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
  videoSpec_videoSpec <*> videoSpec -- <|>
  --rational_rational_videoSpec <*> rational <*> rational

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
-- Set Position Cordinates --

rational_rational_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> VideoSpec -> VideoSpec)
rational_rational_videoSpec_videoSpec = setPosCoord <$ reserved "pos"

--
-- Time Functions --

--int_videoSpec_videoSpec :: ExpParser (Rational -> VideoSpec -> VideoSpec) --bueno
int_videoSpec_videoSpec :: ExpParser (Int -> VideoSpec -> VideoSpec) --borrar
int_videoSpec_videoSpec = playEvery <$ reserved "playEvery"

--int_int_videoSpec_videoSpec :: ExParser (Int -> Int -> VideoSpec -> VideoSpec)
--int_int_videoSpec_videoSpec = playchop' <$ reserved "playChop'"

--int_int_int_videoSpec_videoSpec :: ExParser (Int -> Int -> Int -> VideoSpec -> VideoSpec)
--int_int_int_videoSpec_videoSpec = playchop <$ reserved "playChop"


--
-- ExpParser (VideoSpec -> VideoSpec) --

videoSpec_videoSpec :: ExpParser (VideoSpec -> VideoSpec)
videoSpec_videoSpec =
  playRound <$ reserved "playRound" <|>
  int_videoSpec_videoSpec <*> int <|>
  rational_rational_videoSpec_videoSpec <*> rational <*> rational
  --int_int_videoSpec_videoSpec <*> int <*> int <|>
  --int_int_int_videoSpec_videoSpec <*> int <*> int <*> int
