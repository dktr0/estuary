module Estuary.Languages.CQenze (cqenzeParamPattern) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal

cqenzeParamPattern :: String -> Tidal.ParamPattern
cqenzeParamPattern x = either (const Tidal.silence) id $ parse cqenzeParser "(unknown)" x

cqenzeParser :: GenParser Char a Tidal.ParamPattern
cqenzeParser = sepBy cqenzeLine (char '\n') >>= return . Tidal.stack

cqenzeLine :: GenParser Char a Tidal.ParamPattern
cqenzeLine = do
  x <- sampleName
  y <- patternList
  z <- transformations
  return $ z $ Tidal.s $ Tidal.p $ makeTidalPattern x y

sampleName :: GenParser Char a String
sampleName = many (noneOf "+-")

patternList :: GenParser Char a [Bool]
patternList = many (oneOf "+-") >>= return . fmap (=='+')

transformations :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
transformations = many (noneOf "\n") >>= return . (foldl (.) id) . fmap charToTransformation

charToTransformation :: Char -> (Tidal.ParamPattern -> Tidal.ParamPattern)
charToTransformation '?' = Tidal.degrade
charToTransformation 'f' = Tidal.fast 2
charToTransformation 's' = Tidal.slow 2
charToTransformation 'r' = Tidal.rev
charToTransformation _ = id

makeTidalPattern :: String -> [Bool] -> String
makeTidalPattern x ys = intercalate " " $ fmap (bool "~" x) ys
