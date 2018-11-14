module Estuary.Languages.CQenze (cqenzeControlPattern) where

import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.ParamPatternable (parseBP')


cqenzeControlPattern :: String -> Either ParseError Tidal.ControlPattern
cqenzeControlPattern x = parse cqenzeParser "CQenze" x

cqenzeParser :: GenParser Char a Tidal.ControlPattern
cqenzeParser = sepBy cqenzeLine (char '\n') >>= return . Tidal.stack

cqenzeLine :: GenParser Char a Tidal.ControlPattern
cqenzeLine = do
  x <- sampleName
  y <- patternList
  z <- transformations
  return $ z $ Tidal.s $ parseBP' $ makeTidalPattern x y

sampleName :: GenParser Char a String
sampleName = many (noneOf "+-")

patternList :: GenParser Char a [Bool]
patternList = many (oneOf "+-") >>= return . fmap (=='+')

transformations :: GenParser Char a (Tidal.ControlPattern -> Tidal.ControlPattern)
transformations = many (noneOf "\n") >>= return . (foldl (.) id) . fmap charToTransformation

charToTransformation :: Char -> (Tidal.ControlPattern -> Tidal.ControlPattern)
charToTransformation '?' = Tidal.degrade
charToTransformation 'f' = Tidal.fast 2
charToTransformation 's' = Tidal.slow 2
charToTransformation 'r' = Tidal.rev
charToTransformation 'b' = Tidal.brak
charToTransformation 'c' = Tidal.chop 2
charToTransformation _ = id

makeTidalPattern :: String -> [Bool] -> String
makeTidalPattern x ys = intercalate " " $ fmap (bool "~" x) ys
