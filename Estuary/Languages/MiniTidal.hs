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
    x <- paramPattern
    eof
    return x
  ]

paramPattern :: Parser ParamPattern
paramPattern = chainl1 unmergedParamPattern paramPatternMergeOperators

unmergedParamPattern :: Parser ParamPattern
unmergedParamPattern = choice [
  brackets paramPattern,
  transformedPattern paramPatternTransformations paramPattern,
  specificParamPattern
  ]

transformedPattern :: (Parser (Pattern a -> Pattern a)) -> Parser (Pattern a) -> Parser (Pattern a)
transformedPattern t p = t <*> inBracketsOrApplied p

transformedPattern' :: (Parser (Pattern a -> Pattern a)) -> Parser (Pattern a) -> Parser (Pattern a)
transformedPattern' t p = t <*> p

doublePattern :: Parser (Pattern Double)
doublePattern = chainl1 unmergedDoublePattern numPatternMergeOperators

unmergedDoublePattern :: Parser (Pattern Double)
unmergedDoublePattern = choice [
  brackets doublePattern,
  try $ transformedPattern' (patternTransformations genericPattern) simplePattern,
  try $ transformedPattern (patternTransformations genericPattern) doublePattern,
  simplePattern,
  oscillators
  ]

genericPattern :: (Parseable a, Enumerable a) => Parser (Pattern a)
genericPattern = choice [
  brackets genericPattern,
  try $ transformedPattern (patternTransformations genericPattern) genericPattern,
  simplePattern
  ]

numPatternMergeOperators :: (Num a, Parseable a,Fractional a) => Parser (Pattern a -> Pattern a -> Pattern a)
numPatternMergeOperators = choice [
  reservedOp "+" >> return (+),
  reservedOp "-" >> return (-),
  reservedOp "*" >> return (*),
  reservedOp "/" >> return (/)
  ]

paramPatternTransformation :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformation = inBracketsOrNot paramPatternTransformations

patternTransformation :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformation p = inBracketsOrNot (patternTransformations p)

paramPatternTransformations :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformations = choice [
  reserved "chop" >> intAsPattern >>= return . T.chop,
  reserved "striate" >> intAsPattern >>= return . T.striate,
  (reserved "striate'" >> return T.striate) <*> intAsPattern <*> double,
  (reserved "stut" >> return T.stut) <*> integerAsPattern <*> doubleAsPattern <*> rationalPattern,
  reserved "jux" >> paramPatternTransformation >>= return . T.jux,
  patternTransformations paramPattern
  ]


patternTransformations :: Parser (Pattern a) -> Parser (Pattern a -> Pattern a)
patternTransformations p = choice [
  reserved "brak" >> return T.brak,
  reserved "rev" >> return T.rev,
  reserved "palindrome" >> return T.palindrome,
  reserved "fast" >> timePattern >>= return . T.fast,
  reserved "density" >> timePattern >>= return . T.density,
  reserved "slow" >> timePattern >>= return . T.slow,
  reserved "iter" >> intAsPattern >>= return . T.iter,
  reserved "trunc" >> timePattern >>= return . T.trunc,
  shiftLeft,
  shiftRight,
  (reserved "swingBy" >> return T.swingBy) <*> timePattern <*> timePattern, -- do we need fromIntegral on second argument (int)?
  (reserved "every" >> return T.every) <*> intAsPattern <*> patternTransformation p,
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

paramPatternMergeOperators :: Parser (ParamPattern -> ParamPattern -> ParamPattern)
paramPatternMergeOperators = choice [
  reservedOp "#" >> return (T.#),
  reservedOp "|=|" >> return (T.|=|),
  reservedOp "|+|" >> return (T.|+|),
  reservedOp "|-|" >> return (T.|-|),
  reservedOp "|*|" >> return (T.|*|),
  reservedOp "|/|" >> return (T.|/|)
  ]

specificPatternDouble :: String -> (Pattern Double -> ParamPattern) -> Parser (ParamPattern)
specificPatternDouble s f = do
  reserved s >> spaces
  x <- choice [
    reservedOp "$" >> doublePattern,
    doublePattern
    ]
  return $ f x

specificPatternGeneric :: (Parseable a, Enumerable a) => String -> (Pattern a -> ParamPattern) -> Parser (ParamPattern)
specificPatternGeneric s f = do
  reserved s >> spaces
  x <- choice [
    reservedOp "$" >> genericPattern,
    genericPattern
    ]
  return $ f x

specificParamPattern :: Parser (ParamPattern)
specificParamPattern = choice [
  reserved "silence" >> return T.silence,
  specificPatternGeneric "s" T.s,
  specificPatternGeneric "sound" T.sound,
  specificPatternDouble "n" T.n,
  specificPatternDouble "up" T.up,
  specificPatternDouble "speed" T.speed,
  specificPatternGeneric "vowel" T.vowel,
  specificPatternDouble "pan" T.pan,
  specificPatternDouble "shape" T.shape,
  specificPatternDouble "gain" T.gain,
  specificPatternDouble "accelerate" T.accelerate,
  specificPatternDouble "bandf" T.bandf,
  specificPatternDouble "bandq" T.bandq,
  specificPatternDouble "begin" T.begin,
  specificPatternGeneric "coarse" T.coarse,
  specificPatternDouble "crush" T.crush,
  specificPatternGeneric "cut" T.cut,
  specificPatternDouble "cutoff" T.cutoff,
  specificPatternDouble "delayfeedback" T.delayfeedback,
  specificPatternDouble "delaytime" T.delaytime,
  specificPatternDouble "delay" T.delay,
  specificPatternDouble "end" T.end,
  specificPatternDouble "hcutoff" T.hcutoff,
  specificPatternDouble "hresonance" T.hresonance,
  specificPatternGeneric "loop" T.loop,
  specificPatternDouble "resonance" T.resonance,
  specificPatternDouble "shape" T.shape,
  specificPatternGeneric "unit" T.unit
  ]

simplePattern :: (Parseable b, Enumerable b) => Parser (Pattern b)
simplePattern = do
  reservedOp "\""
  x <- Text.ParserCombinators.Parsec.many (noneOf "\"") -- ** this bypassing of tokenizing is probably not correct
  reservedOp "\""
  return $ T.p x

oscillators :: Parser (Pattern Double)
oscillators = choice [
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

timePattern :: Parser (Pattern Time)
timePattern = do
  x <- double
  return $ pure (fromIntegral x)

rationalPattern :: Parser (Pattern Rational)
rationalPattern = do
  x <- double
  return $ pure (fromIntegral x)

intAsPattern :: Parser (Pattern Int)
intAsPattern = do
  x <- int
  return $ pure x

integerAsPattern :: Parser (Pattern Integer)
integerAsPattern = do
  x <- integer
  return $ pure x

doubleAsPattern :: Parser (Pattern Double)
doubleAsPattern = do
  x <- double
  return $ pure x




applied :: Parser a -> Parser a
applied p = reservedOp "$" >> p

inBracketsOrApplied :: Parser a -> Parser a
inBracketsOrApplied p = brackets p <|> applied p

inBracketsOrNot :: Parser a -> Parser a
inBracketsOrNot p = brackets p <|> p

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

double :: GenParser Char a Double
double = choice $ fmap try [float,fromIntegral <$> integer]

int :: GenParser Char a Int
int = fmap (fromIntegral) integer
