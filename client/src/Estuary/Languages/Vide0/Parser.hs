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
  int_VideoSpec <*> int
  --videoSpec_videoSpec <*> videoSpec


literalVideoSpec :: ExpParser VideoSpec
literalVideoSpec = fmap stringToVideoSpec string
-- fmap :: (a->b) -> f a -> f b
-- entonces con el fmap estamos usando (a -> b) que es igual a String -> VideoSpec que es igual a stringToVideoSpec, después le damos el f a que es el string y nos va a devolver f b que es ExpParser VideoSpec

--
-- Second option = play video:# (default time)

int :: ExpParser Int
int = fromIntegral <$> integer

videoSpec_int_videoSpec :: ExpParser (VideoSpec -> Int -> VideoSpec)
videoSpec_int_videoSpec = setSampleNumber <$ reserved ":"

int_VideoSpec :: ExpParser (Int -> VideoSpec)
int_VideoSpec = videoSpec_int_videoSpec <*> videoSpec

--
-- Third option = change time to the video:#

--int_videoSpec_videoSpec :: ExpParser (Int -> VideoSpec -> VideoSpec)
--int_videoSpec_videoSpec = playEvery <$ reserved "playEvery"

--videoSpec_videoSpec :: ExpParser (VideoSpec -> VideoSpec)
--videoSpec_videoSpec = int_videoSpec_videoSpec <*> int
