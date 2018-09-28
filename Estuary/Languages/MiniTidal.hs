module Estuary.Languages.MiniTidal (miniTidalParser) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List (intercalate)
import Data.Bool (bool)
import Sound.Tidal.Context as T hiding ((<|>))

miniTidalParser :: String -> ParamPattern
miniTidalParser x = either (const silence) id $ parse miniTidal "(unknown)" $ filter (/='?') x

miniTidal :: Parser ParamPattern
miniTidal = spaces >> paramPattern

paramPattern :: Parser ParamPattern
paramPattern = choice [
  try $ transformedPattern0,
  try $ transformedPattern1 specificParamPattern,
  try $ transformedPattern2 specificParamPattern,
  specificParamPattern
  ]

pattern :: Parser (Pattern a) -> Parser (Pattern a)
pattern p = choice [
  try $ transformedPattern1 p,
  try $ transformedPattern2 p,
  p
  ]

transformedPattern0 :: Parser ParamPattern -- specialized for ParamPattern because so are the merge operators...
transformedPattern0 = do
  x <- paramPattern
  m <- mergeOperator
  y <- paramPattern
  spaces
  return $ m x y

transformedPattern1 :: Parser (Pattern a) -> Parser (Pattern a)
transformedPattern1 p = do
  x <- patternTransformation p
  char '$'
  spaces
  y <- pattern p
  spaces
  return $ x y

transformedPattern2 :: Parser (Pattern a) -> Parser (Pattern a)
transformedPattern2 p = do
  x <- patternTransformation p
  char '('
  spaces
  y <- pattern p
  spaces
  char ')'
  spaces
  return $ x y

patternTransformation :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformation p = do
  spaces
  x <- choice [ try $ patternTransformationInBrackets p, patternTransformations p]
  spaces
  return x

patternTransformationInBrackets :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformationInBrackets p = do
  char '('
  spaces
  x <- patternTransformations p
  spaces
  char ')'
  spaces
  return x

paramPatternTransformations :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformations = choice [
  try $ mergedPattern p,
  try $ mergedPattern1 p,
  try (string "chop" >> spaces >> int >>= return . chop),
  try (string "striate" >> spaces >> int >>= return . striate),
  try Estuary.Languages.MiniTidal.striate',
  try Estuary.Languages.MiniTidal.stut,
  string "jux" >> spaces >> patternTransformation p >>= return . jux,
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
  t <- patternTransformationInBrackets p
  return $ T.every n t

whenmod :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
whenmod p = do
  string "whenmod"
  spaces
  x <- int
  spaces
  y <- int
  spaces
  t <- patternTransformationInBrackets p
  return $ T.whenmod x y t

append :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
append p = do
  string "append" >> spaces
  x <- pattern p
  spaces
  return $ T.append x

mergedPattern :: Parser (ParamPattern -> ParamPattern)
mergedPattern p = do
  x <- paramPattern
  spaces
  m <- mergeOperator
  return $ m x

mergedPattern1 :: Parser (ParamPattern -> ParamPattern)
mergedPattern1 = do
  m <- mergeOperator
  spaces
  x <- paramPattern
  return $ \y -> m y x


mergeOperator :: Parser (ParamPattern -> ParamPattern -> ParamPattern)
mergeOperator = choice [
  try (char '#' >> spaces >> return (#)),
  try (string "|=|" >> spaces >> return (|=|)),
  try (string "|+|" >> spaces >> return (|+|)),
  try (string "|-|" >> spaces >> return (|-|)),
  try (string "|*|" >> spaces >> return (|*|)),
  (string "|/|" >> spaces >> return (|/|))
  ]

-- specificPatternDouble :: String -> (Pattern Double -> ParamPattern) -> GenParser Char a (ParamPattern)
specificPatternDouble s f = string s >> spaces >> doublePattern >>= return . f

-- specificPatternGeneric :: Parseable a => String -> (Pattern a -> ParamPattern) -> GenParser Char a (ParamPattern)
specificPatternGeneric s f = string s >> spaces >> genericPattern >>= return . f

{-
paramPatternGeneric :: Parseable a => String -> (Pattern a -> ParamPattern) -> Parser ParamPattern
paramPatternGeneric s f = do
  string s >> spaces
  x <- choice [
    try genericPattern,


  ]
  return $ f x
-}

specificParamPattern :: GenParser Char a (ParamPattern)
specificParamPattern = choice [
  try (string "silence" >> spaces >> return silence),
  try (specificPatternGeneric "s" s),
  try (specificPatternGeneric "sound" sound),
  try (specificPatternGeneric "n" n),
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

genericPattern :: (Parseable b, Enumerable b) => GenParser Char a (Pattern b)
genericPattern = do
  char '"'
  x <- Text.ParserCombinators.Parsec.many (noneOf "\"")
  char '"'
  spaces
  return $ p x

doublePattern :: GenParser Char a (Pattern Double)
doublePattern = (try genericPattern) <|> oscillators

oscillators :: GenParser Char a (Pattern Double)
oscillators = choice [
  try (string "sinewave" >> spaces >> return sinewave),
  try (string "sinewave1" >> spaces >> return sinewave1),
  try (string "sine" >> spaces >> return sine),
  try (string "sine1" >> spaces >> return sine1),
--  try (string "cosine" >> spaces >> return cosine),
  try (string "sawwave" >> spaces >> return sawwave),
  try (string "sawwave1" >> spaces >> return sawwave1),
  try (string "saw" >> spaces >> return saw),
  try (string "saw1" >> spaces >> return saw1),
  try (string "tri" >> spaces >> return tri),
  try (string "tri1" >> spaces >> return tri1),
  try (string "triwave" >> spaces >> return triwave),
  try (string "triwave1" >> spaces >> return triwave1),
  try (string "square" >> spaces >> return square),
  try (string "square1" >> spaces >> return square1),
  try (string "squarewave" >> spaces >> return squarewave),
  string "squarewave1" >> spaces >> return squarewave1
  ]
