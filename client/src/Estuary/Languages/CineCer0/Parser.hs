module Estuary.Languages.CineCer0.Parser (escuchar,CineCer0Spec) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time

import Estuary.Languages.ExpParser
import Estuary.Languages.CineCer0.VideoSpec

type CineCer0Spec = IntMap VideoSpec

escuchar :: String -> Either String CineCer0Spec
escuchar s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runExpParser cineCer0Spec x
    f (ParseFailed l s) = Left s

cineCer0Spec :: ExpParser CineCer0Spec
cineCer0Spec = fmap (fromList . zip [0..]) $ listOfDoStatements videoSpec

videoSpec :: ExpParser VideoSpec
videoSpec =
  --videoMiscelanea <|>
  literalVideoSpec <|>
  int_VideoSpec <*> int <|>
  videoSpec_videoSpec <*> videoSpec

-- videoMiscelanea :: ExpParser VideoSpec
-- videoMiscelanea = do
--   miscelanea
--   v <- literalVideoSpec
--   return v
--
-- miscelanea :: ExpParser ()
-- miscelanea = () <$ reserved "me"

int :: ExpParser Int
int = fromIntegral <$> integer

nominalDiffTime :: ExpParser NominalDiffTime
nominalDiffTime = fromRational <$> rationalOrInteger

literalVideoSpec :: ExpParser VideoSpec
literalVideoSpec =
  fmap stringToVideoSpec string <|>
  fmap emptyVideoSpec string

videoSpec_int_videoSpec :: ExpParser (VideoSpec -> Int -> VideoSpec)
videoSpec_int_videoSpec = setSourceNumber <$ reserved ":"

int_VideoSpec :: ExpParser (Int -> VideoSpec)
int_VideoSpec = videoSpec_int_videoSpec <*> videoSpec

rat_videoSpec_videoSpec :: ExpParser (Rational -> VideoSpec -> VideoSpec)
rat_videoSpec_videoSpec =
  playNatural <$ reserved "playNatural" <|> --time function
  playRound <$ reserved "playRound" <|> -- time function
  setWidth <$ reserved "w" <|>
  setHeight <$ reserved "h" <|>
  setPosX <$ reserved "posX" <|>
  setPosY <$ reserved "posY" <|>
  setOpacity <$ reserved "opacity" <|>
  setHue <$ reserved "hue" <|>
  setSaturation <$ reserved "saturation"

rat_rat_videoSpec_videoSpec :: ExpParser (Rational -> Rational -> VideoSpec -> VideoSpec)
rat_rat_videoSpec_videoSpec =
  playEvery <$ reserved "playEvery" <|> --time function

  setPosCoord <$ (reserved "sentada" <|> reserved "escuchar" <|> reserved "escucho" <|> reserved "escuchando" <|> reserved "suena" <|> reserved "gustan" <|> reserved "recuerdo" <|> reserved "recordando" <|> reserved "atraviesan" <|> reserved "atravesando" <|> reserved "hago" <|> reserved "hacen") <|>

  setSize <$ (reserved "consciente" <|> reserved "conscientes" <|> reserved "extraño" <|> reserved "extraña" <|> reserved "extraños" <|> reserved "extrañas" <|> reserved "diferente" <|> reserved "diferentes" <|> reserved "fuerte" <|> reserved "fuertes" <|> reserved "tempestuoso" <|> reserved "tempestuosa" <|> reserved "tempestuosos" <|> reserved "tempestuosas" <|> reserved "transformado" <|> reserved "transformada" <|> reserved "transformados" <|> reserved "transformadas")

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
