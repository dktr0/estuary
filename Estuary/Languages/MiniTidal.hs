module Estuary.Languages.MiniTidal (miniTidalParser) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.List (intercalate)
import Data.Bool (bool)
import Sound.Tidal.Context (ParamPattern,Pattern,Enumerable,Parseable,Time)
import qualified Sound.Tidal.Context as T

miniTidalParser :: String -> Either ParseError ParamPattern
miniTidalParser x = parse miniTidal "miniTidal" $ filter (/='?') x

miniTidal :: Parser ParamPattern
miniTidal = choice [
  try $ spaces >> eof >> return T.silence,
  do
    spaces
    x <- mergedParamPatterns
    eof
    return x
  ]

mergedParamPatterns :: Parser ParamPattern
mergedParamPatterns = chainl1 paramPattern paramPatternMergeOperators

mergedDoublePatterns :: Parser (Pattern Double)
mergedDoublePatterns = chainl1 doublePattern numPatternMergeOperators

mergedIntPatterns :: Parser (Pattern Int)
mergedIntPatterns = chainl1 intPattern intPatternMergeOperators

mergedTimePatterns :: Parser (Pattern Time)
mergedTimePatterns = chainl1 timePattern numPatternMergeOperators

paramPatternMergeOperators :: Parser (ParamPattern -> ParamPattern -> ParamPattern)
paramPatternMergeOperators = choice [
  reservedOp "#" >> return (T.#),
  reservedOp "|=|" >> return (T.|=|),
  reservedOp "|+|" >> return (T.|+|),
  reservedOp "|-|" >> return (T.|-|),
  reservedOp "|*|" >> return (T.|*|),
  reservedOp "|/|" >> return (T.|/|)
  ]

numPatternMergeOperators :: (Num a, Parseable a,Fractional a) => Parser (Pattern a -> Pattern a -> Pattern a)
numPatternMergeOperators = choice [
  reservedOp "+" >> return (+),
  reservedOp "-" >> return (-),
  reservedOp "*" >> return (*),
  reservedOp "/" >> return (/)
  ]

intPatternMergeOperators :: Parser (Pattern Int -> Pattern Int -> Pattern Int)
intPatternMergeOperators = choice [
  reservedOp "+" >> return (+),
  reservedOp "-" >> return (-),
  reservedOp "*" >> return (*)
  ]

silence :: Parser (Pattern a)
silence = reserved "silence" >> return T.silence

paramPattern :: Parser ParamPattern
paramPattern = choice [
  silence,
  parens mergedParamPatterns,
  parensOrNot specificPatternDouble <*> simplePattern, -- *** rework later as "manyParensOrNot"
  parensOrNot specificPatternDouble <*> applied mergedDoublePatterns,
  parensOrNot specificPatternDouble <*> parens mergedDoublePatterns,
  parensOrNot specificPatternInt <*> simplePattern,
  parensOrNot specificPatternInt <*> applied mergedIntPatterns,
  parensOrNot specificPatternInt <*> parens mergedIntPatterns,
  parensOrNot specificPatternString <*> simplePattern,
  parensOrNot paramPatternTransformation <*> silence,
  parensOrNot paramPatternTransformation <*> applied mergedParamPatterns,
  parensOrNot paramPatternTransformation <*> parens mergedParamPatterns
  ]

doublePattern :: Parser (Pattern Double)
doublePattern = choice [
  silence,
  oscillator,
  simplePattern,
  parens mergedDoublePatterns,
  parensOrNot (patternTransformation doublePattern) <*> silence, -- *** not sure if doublePattern is sufficiently general here
  parensOrNot (patternTransformation doublePattern) <*> oscillator,
  parensOrNot (patternTransformation doublePattern) <*> simplePattern,
  parensOrNot (patternTransformation doublePattern) <*> parens mergedDoublePatterns,
  parensOrNot (patternTransformation doublePattern) <*> applied mergedDoublePatterns
  ]

intPattern :: Parser (Pattern Int)
intPattern = choice [
  silence,
  simplePattern,
  parens mergedIntPatterns,
  parensOrNot (patternTransformation intPattern) <*> silence, -- *** not sure if intPattern is sufficiently general here
  parensOrNot (patternTransformation intPattern) <*> simplePattern,
  parensOrNot (patternTransformation intPattern) <*> parens mergedIntPatterns,
  parensOrNot (patternTransformation intPattern) <*> applied mergedIntPatterns
  ]

timePattern :: Parser (Pattern Time)
timePattern = choice [
  silence,
  simpleTime,
  simplePattern,
  parens mergedTimePatterns,
  parensOrNot (patternTransformation timePattern) <*> silence, -- ** not sure if timePattern is sufficiently general here
  parensOrNot (patternTransformation timePattern) <*> simpleTime,
  parensOrNot (patternTransformation timePattern) <*> simplePattern,
  parensOrNot (patternTransformation timePattern) <*> parens mergedTimePatterns,
  parensOrNot (patternTransformation timePattern) <*> applied mergedTimePatterns
  ]

simpleTime :: Parser (Pattern Time)
simpleTime = (pure . fromIntegral <$> integer) <|> (pure . toRational <$> double)

simpleInteger :: Parser (Pattern Integer)
simpleInteger = pure <$> integer

paramPatternTransformation :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformation = choice [
  reserved "chop" >> intPattern >>= return . T.chop,
  reserved "striate" >> intPattern >>= return . T.striate,
  (reserved "striate'" >> return T.striate') <*> intPattern <*> doublePattern,
  (reserved "stut" >> return T.stut) <*> simpleInteger <*> doublePattern <*> timePattern, -- *** simpleInteger needs to be "integerPattern" which we don't have (why is this Integer instead of Int? maybe just an accident in the Tidal codebase)
  reserved "jux" >> parens paramPatternTransformation >>= return . T.jux,
  patternTransformation paramPattern
  ]

patternTransformation :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformation p = choice [
  reserved "brak" >> return T.brak,
  reserved "rev" >> return T.rev,
  reserved "palindrome" >> return T.palindrome,
  reserved "fast" >> timePattern >>= return . T.fast,
  reserved "density" >> timePattern >>= return . T.density,
  reserved "slow" >> timePattern >>= return . T.slow,
  reserved "iter" >> intPattern >>= return . T.iter,
  reserved "trunc" >> timePattern >>= return . T.trunc,
  shiftLeft,
  shiftRight,
  (reserved "swingBy" >> return T.swingBy) <*> timePattern <*> timePattern,
  (reserved "every" >> return T.every) <*> intPattern <*> patternTransformation p, -- *** note: the recursion without the transformation type isn't adequate for parampatterns
  (reserved "whenmod" >> return T.whenmod) <*> int <*> int <*> patternTransformation p,
  (reserved "append" >> return T.append) <*> p
  ]

shiftLeft :: Parser (Pattern a -> Pattern a)
shiftLeft = do
  x <- timePattern
  reservedOp "<~"
  return $ (x T.<~)

shiftRight :: Parser (Pattern a -> Pattern a)
shiftRight = do
  x <- timePattern
  reservedOp "~>"
  return $ (x T.~>)

specificPatternDouble :: Parser (Pattern Double -> ParamPattern)
specificPatternDouble = choice [
  reserved "n" >> return T.n,
  reserved "up" >> return T.up,
  reserved "speed" >> return T.speed,
  reserved "pan" >> return T.pan,
  reserved "shape" >> return T.shape,
  reserved "gain" >> return T.gain,
  reserved "accelerate" >> return T.accelerate,
  reserved "bandf" >> return T.bandf,
  reserved "bandq" >> return T.bandq,
  reserved "begin" >> return T.begin,
  reserved "crush" >> return T.crush,
  reserved "cutoff" >> return T.cutoff,
  reserved "delayfeedback" >> return T.delayfeedback,
  reserved "delaytime" >> return T.delaytime,
  reserved "delay" >> return T.delay,
  reserved "end" >> return T.end,
  reserved "hcutoff" >> return T.hcutoff,
  reserved "hresonance" >> return T.hresonance,
  reserved "resonance" >> return T.resonance,
  reserved "shape" >> return T.shape,
  reserved "loop" >> return T.loop
  ]

specificPatternString :: Parser (Pattern String -> ParamPattern)
specificPatternString = choice [
  reserved "s" >> return T.s,
  reserved "sound" >> return T.sound,
  reserved "vowel" >> return T.vowel,
  reserved "unit" >> return T.unit
  ]

specificPatternInt :: Parser (Pattern Int -> ParamPattern)
specificPatternInt = choice [
  reserved "coarse" >> return T.coarse,
  reserved "cut" >> return T.cut
  ]

simplePattern :: (Parseable b, Enumerable b) => Parser (Pattern b)
simplePattern = do
  reservedOp "\""
  x <- Text.ParserCombinators.Parsec.many (noneOf "\"") -- ** this bypassing of tokenizing is probably not correct
  reservedOp "\""
  return $ T.p x

oscillator :: Parser (Pattern Double)
oscillator = choice [
  reserved "sinewave1" >> return T.sinewave1,
  reserved "sinewave" >> return T.sinewave,
  reserved "sine1" >> return T.sine1,
  reserved "sine" >> return T.sine,
  reserved "sawwave1" >> return T.sawwave1,
  reserved "sawwave" >> return T.sawwave,
  reserved "saw1" >> return T.saw1,
  reserved "saw" >> return T.saw,
  reserved "triwave1" >> return T.triwave1,
  reserved "triwave" >> return T.triwave,
  reserved "tri1" >> return T.tri1,
  reserved "tri" >> return T.tri,
  reserved "squarewave1" >> return T.squarewave1,
  reserved "square1" >> return T.square1,
  reserved "square" >> return T.square,
  reserved "squarewave" >> return T.squarewave
  ]

integerAsPattern :: Parser (Pattern Integer)
integerAsPattern = do
  x <- integer
  return $ pure x

double :: GenParser Char a Double
double = choice [float,fromIntegral <$> integer]

int :: GenParser Char a Int
int = fmap (fromIntegral) integer

applied :: Parser a -> Parser a
applied p = reservedOp "$" >> p

parensOrApplied :: Parser a -> Parser a
parensOrApplied p = parens p <|> applied p

parensOrNot :: Parser a -> Parser a
parensOrNot p = parens p <|> p

parensAppliedOrNot :: Parser a -> Parser a
parensAppliedOrNot p = parens p <|> applied p <|> p

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["chop","striate","striate'","stut","jux","brak","rev",
    "palindrome","fast","density","slow","iter","trunc","swingBy","every","whenmod",
    "append","silence","s","sound","n","up","speed","vowel","pan","shape","gain",
    "accelerate","bandf","bandq","begin","coarse","crush","cut","cutoff","delayfeedback",
    "delaytime","delay","end","hcutoff","hresonance","loop","resonance","shape","unit",
    "sinewave1","sinewave","sine1","sine","sawwave1","sawwave","saw1","saw",
    "triwave1","triwave","tri1","tri","squarewave1","square1","square","squarewave"],
  P.reservedOpNames = ["+","-","*","/","<~","~>","#","|=|","|+|","|-|","|*|","|/|","$","\""]
  }

identifier = P.identifier tokenParser
reserved = P.reserved tokenParser
operator = P.operator tokenParser
reservedOp = P.reservedOp tokenParser
charLiteral = P.charLiteral tokenParser
stringLiteral = P.stringLiteral tokenParser
natural = P.natural tokenParser
integer = P.integer tokenParser
float = P.float tokenParser
naturalOrFloat = P.naturalOrFloat tokenParser
decimal = P.decimal tokenParser
hexadecimal = P.hexadecimal tokenParser
octal = P.octal tokenParser
symbol = P.symbol tokenParser
lexeme = P.lexeme tokenParser
whiteSpace = P.whiteSpace tokenParser
parens = P.parens tokenParser
braces = P.braces tokenParser
angles = P.angles tokenParser
brackets = P.brackets tokenParser
semi = P.semi tokenParser
comma = P.comma tokenParser
colon = P.colon tokenParser
dot = P.dot tokenParser
semiSep = P.semiSep tokenParser
semiSep1 = P.semiSep1 tokenParser
commaSep = P.commaSep tokenParser
commaSep1 = P.commaSep1 tokenParser
