module Estuary.Languages.MiniTidal (miniTidalPattern) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List (intercalate)
import Data.Bool (bool)
import qualified Sound.Tidal.Context as Tidal

miniTidalPattern :: String -> Tidal.ParamPattern
miniTidalPattern x = either (const Tidal.silence) id $ parse miniTidalParser "(unknown)" $ filter (/='?') x

miniTidalParser :: GenParser Char a Tidal.ParamPattern
miniTidalParser = spaces >> patternOrTransformedPattern

patternOrTransformedPattern :: GenParser Char a (Tidal.ParamPattern)
patternOrTransformedPattern = choice [
  try transformedPattern0,
  try transformedPattern1,
  try transformedPattern2,
  specificPattern
  ]

transformedPattern0 :: GenParser Char a (Tidal.ParamPattern)
transformedPattern0 = do
  x <- specificPattern
  m <- mergeOperator
  y <- patternOrTransformedPattern
  spaces
  return $ m x y

transformedPattern1 :: GenParser Char a (Tidal.ParamPattern)
transformedPattern1 = do
  x <- patternTransformation
  char '$'
  spaces
  y <- patternOrTransformedPattern
  spaces
  return $ x y

transformedPattern2 :: GenParser Char a (Tidal.ParamPattern)
transformedPattern2 = do
  x <- patternTransformation
  char '('
  spaces
  y <- patternOrTransformedPattern
  spaces
  char ')'
  return $ x y

patternTransformation :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
patternTransformation = do
  spaces
  x <- choice [ try patternTransformationInBrackets, patternTransformations]
  spaces
  return x

patternTransformationInBrackets :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
patternTransformationInBrackets = do
  char '('
  spaces
  x <- patternTransformations
  spaces
  char ')'
  spaces
  return x

patternTransformations :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
patternTransformations = choice [
  try mergedPattern,
  try mergedPattern1,
  try (string "brak" >> return Tidal.brak),
  try (string "rev" >> return Tidal.rev),
  try (string "palindrome" >> return Tidal.palindrome),
  try (string "fast" >> spaces >> fractional3 False >>= return . Tidal.fast),
  try (string "density" >> spaces >> fractional3 False >>= return . Tidal.density),
  try (string "slow" >> spaces >> fractional3 False >>= return . Tidal.slow),
  try (string "chop" >> spaces >> int >>= return . Tidal.chop),
  try (string "striate" >> spaces >> int >>= return . Tidal.striate),
  try (string "iter" >> spaces >> int >>= return . Tidal.iter),
  try (string "trunc" >> spaces >> fractional3 False >>= return . Tidal.trunc),
  try striate',
  try shiftLeft,
  try shiftRight,
  try stut,
  try swingBy,
  try append,
  try every,
  try whenmod,
  string "jux" >> spaces >> patternTransformation >>= return . Tidal.jux
  ]

shiftLeft :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
shiftLeft = do
  x <- fractional3 False
  spaces
  string "<~"
  spaces
  return $ (x Tidal.<~)

shiftRight :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
shiftRight = do
  x <- fractional3 False
  spaces
  string "~>"
  spaces
  return $ (x Tidal.~>)

striate' :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
striate' = do
  string "striate'"
  spaces
  i <- int
  spaces
  d <- fractional3 False
  spaces
  return $ Tidal.striate' i d

stut :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
stut = do
  string "stut"
  spaces
  n <- int
  spaces
  x <- fractional3 False
  spaces
  y <- fractional3 False
  spaces
  return $ Tidal.stut n x y

swingBy :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
swingBy = do
  string "swingBy"
  spaces
  x <- fractional3 False
  spaces
  y <- int
  spaces
  return $ Tidal.swingBy x (fromIntegral y)

every :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
every = do
  string "every"
  spaces
  n <- int
  spaces
  t <- patternTransformationInBrackets
  return $ Tidal.every n t

whenmod :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
whenmod = do
  string "whenmod"
  spaces
  x <- int
  spaces
  y <- int
  spaces
  t <- patternTransformationInBrackets
  return $ Tidal.whenmod x y t

append :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
append = do
  string "append" >> spaces
  x <- patternOrTransformedPattern
  spaces
  return $ Tidal.append x

mergedPattern :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
mergedPattern = do
  x <- specificPattern
  spaces
  m <- mergeOperator
  return $ m x

mergedPattern1 :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern)
mergedPattern1 = do
  m <- mergeOperator
  spaces
  x <- specificPattern
  return $ \y -> m y x


mergeOperator :: GenParser Char a (Tidal.ParamPattern -> Tidal.ParamPattern -> Tidal.ParamPattern)
mergeOperator = choice [
  try (char '#' >> spaces >> return (Tidal.#)),
  try (string "|=|" >> spaces >> return (Tidal.|=|)),
  try (string "|+|" >> spaces >> return (Tidal.|+|)),
  try (string "|-|" >> spaces >> return (Tidal.|-|)),
  try (string "|*|" >> spaces >> return (Tidal.|*|)),
  (string "|/|" >> spaces >> return (Tidal.|/|))
  ]

-- specificPatternDouble :: String -> (Tidal.Pattern Double -> Tidal.ParamPattern) -> GenParser Char a (Tidal.ParamPattern)
specificPatternDouble s f = string s >> spaces >> doublePattern >>= return . f

-- specificPatternGeneric :: Tidal.Parseable a => String -> (Tidal.Pattern a -> Tidal.ParamPattern) -> GenParser Char a (Tidal.ParamPattern)
specificPatternGeneric s f = string s >> spaces >> genericPattern >>= return . f

specificPattern :: GenParser Char a (Tidal.ParamPattern)
specificPattern = choice [
  try (string "silence" >> spaces >> return Tidal.silence),
  try (specificPatternGeneric "s" Tidal.s),
  try (specificPatternGeneric "sound" Tidal.sound),
  try (specificPatternGeneric "n" Tidal.n),
  try (specificPatternDouble "up" Tidal.up),
  try (specificPatternDouble "speed" Tidal.speed),
  try (specificPatternGeneric "vowel" Tidal.vowel),
  try (specificPatternDouble "pan" Tidal.pan),
  try (specificPatternDouble "shape" Tidal.shape),
  try (specificPatternDouble "gain" Tidal.gain),
  try (specificPatternDouble "accelerate" Tidal.accelerate),
  try (specificPatternDouble "bandf" Tidal.bandf),
  try (specificPatternDouble "bandq" Tidal.bandq),
  try (specificPatternDouble "begin" Tidal.begin),
  try (specificPatternGeneric "coarse" Tidal.coarse),
  try (specificPatternDouble "crush" Tidal.crush),
  try (specificPatternGeneric "cut" Tidal.cut),
  try (specificPatternDouble "cutoff" Tidal.cutoff),
  try (specificPatternDouble "delayfeedback" Tidal.delayfeedback),
  try (specificPatternDouble "delaytime" Tidal.delaytime),
  try (specificPatternDouble "delay" Tidal.delay),
  try (specificPatternDouble "end" Tidal.end),
  try (specificPatternDouble "hcutoff" Tidal.hcutoff),
  try (specificPatternDouble "hresonance" Tidal.hresonance),
  try (specificPatternGeneric "loop" Tidal.loop),
  try (specificPatternDouble "resonance" Tidal.resonance),
  try (specificPatternDouble "shape" Tidal.shape),
  specificPatternGeneric "unit" Tidal.unit
  ]

genericPattern :: Tidal.Parseable b => GenParser Char a (Tidal.Pattern b)
genericPattern = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  spaces
  return $ Tidal.p x

doublePattern :: GenParser Char a (Tidal.Pattern Double)
doublePattern = (try genericPattern) <|> oscillators

oscillators :: GenParser Char a (Tidal.Pattern Double)
oscillators = choice [
  try (string "sinewave" >> spaces >> return Tidal.sinewave),
  try (string "sinewave1" >> spaces >> return Tidal.sinewave1),
  try (string "sine" >> spaces >> return Tidal.sine),
  try (string "sine1" >> spaces >> return Tidal.sine1),
--  try (string "cosine" >> spaces >> return Tidal.cosine),
  try (string "sawwave" >> spaces >> return Tidal.sawwave),
  try (string "sawwave1" >> spaces >> return Tidal.sawwave1),
  try (string "saw" >> spaces >> return Tidal.saw),
  try (string "saw1" >> spaces >> return Tidal.saw1),
  try (string "tri" >> spaces >> return Tidal.tri),
  try (string "tri1" >> spaces >> return Tidal.tri1),
  try (string "triwave" >> spaces >> return Tidal.triwave),
  try (string "triwave1" >> spaces >> return Tidal.triwave1),
  try (string "square" >> spaces >> return Tidal.square),
  try (string "square1" >> spaces >> return Tidal.square1),
  try (string "squarewave" >> spaces >> return Tidal.squarewave),
  string "squarewave1" >> spaces >> return Tidal.squarewave1
  ]
