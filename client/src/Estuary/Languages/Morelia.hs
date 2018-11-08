module Estuary.Languages.Morelia (morelia) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.ParamPatternable (parseBP')

morelia :: String -> Either ParseError Tidal.ControlPattern
morelia x = parse moreliaParser "morelia" $ filter (/='?') x

moreliaParser :: GenParser Char a Tidal.ControlPattern
moreliaParser = do
  xs <- many thingParser
  return $ Tidal.stack xs

thingParser :: GenParser Char a Tidal.ControlPattern
thingParser = do
  xs <- many (oneOf " ")
  ys <- romanNumeral
  zs <- many (oneOf " ")
  char '~'
  return $ anotherFunction (length xs) ys (length zs)

anotherFunction :: Int -> String -> Int -> Tidal.ControlPattern
anotherFunction pos sampleName divider = Tidal.s $ parseBP' x
  where
    x = tildes ++ " " ++ sampleName ++ "/" ++ (show divider)
    tildes = intercalate " " (replicate pos "~")

romanNumeral :: GenParser Char a String
romanNumeral = choice [
  try (string "X" >> return "sugar"),
  try (string "IX" >> return "able"),
  try (string "VIII" >> return "peri"),
  try (string "VII" >> return "jazz"),
  try (string "VI" >> return "moan"),
  try (string "V" >> return "kurt"),
  try (string "IV" >> return "feelfx"),
  try (string "III" >> return "hh"),
  try (string "II" >> return "sn"),
  try (string "I" >> return "bd")
  ]
