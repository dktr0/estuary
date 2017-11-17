module Estuary.Languages.CQenze where

import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal

cqenzeParamPattern :: String -> Tidal.ParamPattern
cqenzeParamPattern x = t $ Tidal.s $ Tidal.p y
  where
    t = stringToTransformation $ getTransformations x
    y = makeTidalPattern sampleName patternList
    sampleName = getSampleName x
    patternList = getPatternList x

stringToTransformation :: String -> (Tidal.ParamPattern -> Tidal.ParamPattern)
stringToTransformation = foldl (.) id . fmap charToTransformation

charToTransformation :: Char -> (Tidal.ParamPattern -> Tidal.ParamPattern)
charToTransformation '?' = Tidal.degrade
charToTransformation 'f' = Tidal.fast 2
charToTransformation 's' = Tidal.slow 2
charToTransformation 'r' = Tidal.rev
charToTransformation _ = id

makeTidalPattern :: String -> [Bool] -> String
makeTidalPattern x ys = intercalate " " $ fmap (bool "~" x) ys

isPatternElem :: Char -> Bool
isPatternElem '+' = True
isPatternElem '-' = True
isPatternElem _ = False

getSampleName :: String -> String
getSampleName = takeWhile (not . isPatternElem)

getPatternList :: String -> [Bool]
getPatternList = fmap f . findPattern
  where
    findPattern = takeWhile isPatternElem . (dropWhile (not . isPatternElem))
    f '+' = True
    f _ = False

getTransformations :: String -> String
getTransformations = dropWhile isPatternElem . dropWhile (not . isPatternElem)
