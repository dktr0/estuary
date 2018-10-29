module Estuary.Languages.MiniTidal (miniTidalParser) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List (intercalate)
import Data.Bool (bool)
import Sound.Tidal.Context as T hiding ((<|>))

miniTidalParser :: String -> Either ParseError ParamPattern
miniTidalParser x = parse miniTidal "miniTidal" $ filter (/='?') x

miniTidal :: Parser ParamPattern
miniTidal = choice [
  try $ spaces >> eof >> return silence,
  do
    spaces
    x <- paramPattern
    eof
    return x
  ]

paramPattern :: Parser ParamPattern
paramPattern = chainl1 unmergedParamPattern paramPatternMergeOperators

unmergedParamPattern :: Parser ParamPattern
unmergedParamPattern = choice [
  try $ inBrackets paramPattern,
  try $ transformedPattern paramPattern,
  specificParamPattern
  ]

transformedPattern :: Parser (Pattern a) -> Parser (Pattern a)
transformedPattern p = do
  t <- patternTransformation p
  p' <- inBracketsOrApplied p
  return $ t p'

transformedPattern' :: Parser (Pattern a) -> Parser (Pattern a)
transformedPattern' p = do
  t <- patternTransformation p
  p' <- p
  return $ t p'

doublePattern :: Parser (Pattern Double)
doublePattern = chainl1 unmergedDoublePattern numPatternMergeOperators

unmergedDoublePattern :: Parser (Pattern Double)
unmergedDoublePattern = choice [
  inBrackets doublePattern,
  try $ transformedPattern' simplePattern,
  try $ transformedPattern doublePattern,
  simplePattern,
  oscillators
  ]

genericPattern :: (Parseable a, Enumerable a) => Parser (Pattern a)
genericPattern = choice [
  inBrackets genericPattern,
  try $ transformedPattern genericPattern,
  simplePattern
  ]

numPatternMergeOperators :: (Num a, Parseable a,Fractional a) => Parser (Pattern a -> Pattern a -> Pattern a)
numPatternMergeOperators = choice [
  spaces >> char '+' >> spaces >> return (+),
  spaces >> char '-' >> spaces >> return (-),
  spaces >> char '*' >> spaces >> return (*),
  spaces >> char '/' >> spaces >> return (/)
  ]

paramPatternTransformation :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformation = inBracketsOrNot paramPatternTransformations

patternTransformation :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformation p = inBracketsOrNot (patternTransformations p)

paramPatternTransformations :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformations = choice [
  try $ mergedPattern,
  try $ mergedPattern1,
  try (string "chop" >> spaces >> int >>= return . chop),
  try (string "striate" >> spaces >> int >>= return . striate),
  try Estuary.Languages.MiniTidal.striate',
  try Estuary.Languages.MiniTidal.stut,
  string "jux" >> spaces >> paramPatternTransformation >>= return . jux,
  try $ patternTransformations paramPattern
  ]

patternTransformations :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformations p = choice [
  try (string "brak" >> return brak),
  try (string "rev" >> return rev),
  try (string "palindrome" >> return palindrome),
  try (string "fast" >> spaces >> fractional3 False >>= return . fast),
  try (string "density" >> spaces >> fractional3 False >>= return . density),
  try (string "slow" >> spaces >> fractional3 False >>= return . slow),
  try (string "iter" >> spaces >> int >>= return . iter),
  try (string "trunc" >> spaces >> fractional3 False >>= return . trunc),
  try shiftLeft,
  try shiftRight,
  try Estuary.Languages.MiniTidal.swingBy,
  try $ Estuary.Languages.MiniTidal.append p,
  try $ Estuary.Languages.MiniTidal.every p,
  try $ Estuary.Languages.MiniTidal.whenmod p
  ]

shiftLeft :: Parser (Pattern a -> Pattern a)
shiftLeft = do
  x <- fractional3 False
  spaces
  string "<~"
  spaces
  return $ (x <~)

shiftRight :: Parser (Pattern a -> Pattern a)
shiftRight = do
  x <- fractional3 False
  spaces
  string "~>"
  spaces
  return $ (x ~>)

striate' :: Parser (ParamPattern -> ParamPattern)
striate' = do
  string "striate'"
  spaces
  i <- int
  spaces
  d <- fractional3 False
  spaces
  return $ T.striate' i d

stut :: Parser (ParamPattern -> ParamPattern)
stut = do
  string "stut"
  spaces
  n <- int
  spaces
  x <- fractional3 False
  spaces
  y <- fractional3 False
  spaces
  return $ T.stut n x y

swingBy :: Parser (Pattern a -> Pattern a)
swingBy = do
  string "swingBy"
  spaces
  x <- fractional3 False
  spaces
  y <- int
  spaces
  return $ T.swingBy x (fromIntegral y)

every :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
every p = do
  string "every"
  spaces
  n <- int
  spaces
  t <- patternTransformation p
  return $ T.every n t

whenmod :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
whenmod p = do
  string "whenmod"
  spaces
  x <- int
  spaces
  y <- int
  spaces
  t <- patternTransformation p
  return $ T.whenmod x y t

append :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
append p = do
  string "append" >> spaces
  x <- p
  spaces
  return $ T.append x


mergedPattern :: Parser (ParamPattern -> ParamPattern)
mergedPattern = do
  x <- paramPattern
  spaces
  m <- paramPatternMergeOperators
  return $ m x

mergedPattern1 :: Parser (ParamPattern -> ParamPattern)
mergedPattern1 = do
  m <- paramPatternMergeOperators
  spaces
  x <- paramPattern
  return $ \y -> m y x

paramPatternMergeOperators :: Parser (ParamPattern -> ParamPattern -> ParamPattern)
paramPatternMergeOperators = choice [
  try (char '#' >> spaces >> return (#)),
  try (string "|=|" >> spaces >> return (|=|)),
  try (string "|+|" >> spaces >> return (|+|)),
  try (string "|-|" >> spaces >> return (|-|)),
  try (string "|*|" >> spaces >> return (|*|)),
  (string "|/|" >> spaces >> return (|/|))
  ]

specificPatternDouble :: String -> (Pattern Double -> ParamPattern) -> Parser (ParamPattern)
specificPatternDouble s f = do
  string s >> spaces
  x <- choice [
    char '$' >> spaces >> doublePattern,
    doublePattern
    ]
  return $ f x

specificPatternGeneric :: (Parseable a, Enumerable a) => String -> (Pattern a -> ParamPattern) -> Parser (ParamPattern)
specificPatternGeneric s f = do
  string s >> spaces
  x <- choice [
    char '$' >> spaces >> genericPattern,
    genericPattern
    ]
  return $ f x

specificParamPattern :: Parser (ParamPattern)
specificParamPattern = choice [
  try (string "silence" >> spaces >> return silence),
  try (specificPatternGeneric "s" s),
  try (specificPatternGeneric "sound" sound),
  try (specificPatternDouble "n" n),
  try (specificPatternDouble "up" up),
  try (specificPatternDouble "speed" speed),
  try (specificPatternGeneric "vowel" vowel),
  try (specificPatternDouble "pan" pan),
  try (specificPatternDouble "shape" shape),
  try (specificPatternDouble "gain" gain),
  try (specificPatternDouble "accelerate" accelerate),
  try (specificPatternDouble "bandf" bandf),
  try (specificPatternDouble "bandq" bandq),
  try (specificPatternDouble "begin" begin),
  try (specificPatternGeneric "coarse" coarse),
  try (specificPatternDouble "crush" crush),
  try (specificPatternGeneric "cut" cut),
  try (specificPatternDouble "cutoff" cutoff),
  try (specificPatternDouble "delayfeedback" delayfeedback),
  try (specificPatternDouble "delaytime" delaytime),
  try (specificPatternDouble "delay" delay),
  try (specificPatternDouble "end" end),
  try (specificPatternDouble "hcutoff" hcutoff),
  try (specificPatternDouble "hresonance" hresonance),
  try (specificPatternGeneric "loop" loop),
  try (specificPatternDouble "resonance" resonance),
  try (specificPatternDouble "shape" shape),
  specificPatternGeneric "unit" unit
  ]

simplePattern :: (Parseable b, Enumerable b) => Parser (Pattern b)
simplePattern = do
  char '"'
  x <- Text.ParserCombinators.Parsec.many (noneOf "\"")
  char '"'
  spaces
  return $ p x

oscillators :: Parser (Pattern Double)
oscillators = choice [
  try (string "sinewave1" >> spaces >> return sinewave1),
  try (string "sinewave" >> spaces >> return sinewave),
  try (string "sine1" >> spaces >> return sine1),
  try (string "sine" >> spaces >> return sine),
--  try (string "cosine" >> spaces >> return cosine),
  try (string "sawwave1" >> spaces >> return sawwave1),
  try (string "sawwave" >> spaces >> return sawwave),
  try (string "saw1" >> spaces >> return saw1),
  try (string "saw" >> spaces >> return saw),
  try (string "triwave1" >> spaces >> return triwave1),
  try (string "triwave" >> spaces >> return triwave),
  try (string "tri1" >> spaces >> return tri1),
  try (string "tri" >> spaces >> return tri),
  try (string "squarewave1" >> spaces >> return squarewave1),
  try (string "square1" >> spaces >> return square1),
  try (string "square" >> spaces >> return square),
  string "squarewave" >> spaces >> return squarewave
  ]

inBrackets :: Parser a -> Parser a
inBrackets p = do
  char '('
  spaces
  x <- p
  spaces
  char ')'
  spaces
  return x

applied :: Parser a -> Parser a
applied p = do
  char '$'
  spaces
  x <- p
  spaces
  return x

notInBrackets :: Parser a -> Parser a
notInBrackets p = do
  x <- p
  spaces
  return x

inBracketsOrApplied :: Parser a -> Parser a
inBracketsOrApplied p = inBrackets p <|> applied p

inBracketsOrNot :: Parser a -> Parser a
inBracketsOrNot p = inBrackets p <|> notInBrackets p
