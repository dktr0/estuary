{-# LANGUAGE FlexibleInstances #-}

module Estuary.Languages.MiniTidal (miniTidalParser) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.List (intercalate)
import Data.Bool (bool)
import Data.Ratio
import Sound.Tidal.Context (ParamPattern,Pattern,Enumerable,Parseable,Time,ParamMap)
import qualified Sound.Tidal.Context as T

miniTidalParser :: String -> Either ParseError ParamPattern
miniTidalParser x = parse miniTidal "miniTidal" $ filter (/='?') x

miniTidal :: Parser ParamPattern
miniTidal = whiteSpace >> choice [
  eof >> return T.silence,
  do
    x <- topLevelParamPattern
    eof
    return x
  ]

class Pattern' a where
  simplePattern :: Parser (Pattern a)
  complexPattern :: Parser (Pattern a)
  mergeOperator :: Parser (Pattern a -> Pattern a -> Pattern a)
  transformationWithoutArgs :: Parser (Pattern a -> Pattern a)
  transformationWithArgs :: Parser (Pattern a -> Pattern a)

topLevelParamPattern :: Parser ParamPattern
topLevelParamPattern = chainl1 pattern paramPatternMergeOperator

pattern :: Pattern' a => Parser (Pattern a)
pattern = choice [
  nestedParens $ chainl1 pattern mergeOperator,
  parensOrNot complexPattern,
  parensOrNot transformedPattern,
  parensOrNot simplePattern,
  silence
  ]

silence :: Parser (Pattern a)
silence = function "silence" >> return T.silence

transformedPattern :: Pattern' a => Parser (Pattern a)
transformedPattern = (transformationWithArgs <|> transformationWithoutArgs) <*> patternArg

patternArg :: Pattern' a => Parser (Pattern a)
patternArg = choice [
  try $ parensOrApplied $ chainl1 pattern mergeOperator,
  try $ parensOrApplied transformedPattern,
  try $ parensOrApplied complexPattern,
  appliedOrNot simplePattern,
  appliedOrNot silence
  ]


instance Pattern' ParamMap where
  simplePattern = choice []
  complexPattern = specificParamPatterns
  mergeOperator = paramPatternMergeOperator
  transformationWithArgs = paramPatternTransformation <|> patternTransformationWithArgs
  transformationWithoutArgs = patternTransformationWithoutArgs

paramPatternMergeOperator :: Parser (ParamPattern -> ParamPattern -> ParamPattern)
paramPatternMergeOperator = choice [
  op "#" >> return (T.#),
  op "|=|" >> return (T.|=|),
  op "|+|" >> return (T.|+|),
  op "|-|" >> return (T.|-|),
  op "|*|" >> return (T.|*|),
  op "|/|" >> return (T.|/|)
  ]

specificParamPatterns :: Parser ParamPattern
specificParamPatterns = choice [
  (function "coarse" >> return T.coarse) <*> patternArg,
  (function "cut" >> return T.cut) <*> patternArg,
  (function "n" >> return T.n) <*> patternArg,
  (function "up" >> return T.up) <*> patternArg,
  (function "speed" >> return T.speed) <*> patternArg,
  (function "pan" >> return T.pan) <*> patternArg,
  (function "shape" >> return T.shape) <*> patternArg,
  (function "gain" >> return T.gain) <*> patternArg,
  (function "accelerate" >> return T.accelerate) <*> patternArg,
  (function "bandf" >> return T.bandf) <*> patternArg,
  (function "bandq" >> return T.bandq) <*> patternArg,
  (function "begin" >> return T.begin) <*> patternArg,
  (function "crush" >> return T.crush) <*> patternArg,
  (function "cutoff" >> return T.cutoff) <*> patternArg,
  (function "delayfeedback" >> return T.delayfeedback) <*> patternArg,
  (function "delaytime" >> return T.delaytime) <*> patternArg,
  (function "delay" >> return T.delay) <*> patternArg,
  (function "end" >> return T.end) <*> patternArg,
  (function "hcutoff" >> return T.hcutoff) <*> patternArg,
  (function "hresonance" >> return T.hresonance) <*> patternArg,
  (function "resonance" >> return T.resonance) <*> patternArg,
  (function "shape" >> return T.shape) <*> patternArg,
  (function "loop" >> return T.loop) <*> patternArg,
  (function "s" >> return T.s) <*> patternArg,
  (function "sound" >> return T.sound) <*> patternArg,
  (function "vowel" >> return T.vowel) <*> patternArg,
  (function "unit" >> return T.unit) <*> patternArg
  ]

paramPatternTransformation :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformation = choice [
  function "chop" >> patternArg >>= return . T.chop,
  function "striate" >> patternArg >>= return . T.striate,
  (function "striate'" >> return T.striate') <*> patternArg <*> patternArg,
  (function "stut" >> return T.stut) <*> patternArg <*> patternArg <*> patternArg,
  function "jux" >> patternTransformationArg >>= return . T.jux
  ]

patternTransformationArg :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationArg = appliedOrNot transformationWithoutArgs <|> parensOrApplied transformationWithArgs

patternTransformationWithoutArgs :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationWithoutArgs = choice [
  function "brak" >> return T.brak,
  function "rev" >> return T.rev,
  function "palindrome" >> return T.palindrome
  ]

patternTransformationWithArgs :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationWithArgs= parensOrNot $ choice [
  function "fast" >> patternArg >>= return . T.fast,
  function "density" >> patternArg >>= return . T.density,
  function "slow" >> patternArg >>= return . T.slow,
  function "iter" >> patternArg >>= return . T.iter,
  function "trunc" >> patternArg >>= return . T.trunc,
  (function "swingBy" >> return T.swingBy) <*> patternArg <*> patternArg,
  (function "append" >> return T.append) <*> patternArg,
  (function "every" >> return T.every) <*> patternArg <*> patternTransformationArg,
  (function "whenmod" >> return T.whenmod) <*> int <*> int <*> patternTransformationArg
  ]

oscillator :: Parser (Pattern Double)
oscillator = choice [
  function "sinewave1" >> return T.sinewave1,
  function "sinewave" >> return T.sinewave,
  function "sine1" >> return T.sine1,
  function "sine" >> return T.sine,
  function "sawwave1" >> return T.sawwave1,
  function "sawwave" >> return T.sawwave,
  function "saw1" >> return T.saw1,
  function "saw" >> return T.saw,
  function "triwave1" >> return T.triwave1,
  function "triwave" >> return T.triwave,
  function "tri1" >> return T.tri1,
  function "tri" >> return T.tri,
  function "squarewave1" >> return T.squarewave1,
  function "square1" >> return T.square1,
  function "square" >> return T.square,
  function "squarewave" >> return T.squarewave
  ]


instance Pattern' Int where
  simplePattern = choice [
    pure <$> int,
    T.p <$> stringLiteral
    ]
  complexPattern = choice [
    function "pure" >> int >>= return . pure,
    function "return" >> int >>= return . return
    ]
  mergeOperator = numMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs


instance Pattern' Integer where
  simplePattern = choice [
    pure <$> integer,
    T.p <$> stringLiteral
    ]
  complexPattern = choice [
    function "pure" >> integer >>= return . pure,
    function "return" >> integer >>= return . return
    ]
  mergeOperator = numMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs


instance Pattern' Double where
  simplePattern = choice [
    oscillator,
    pure <$> double,
    T.p <$> stringLiteral
    ]
  complexPattern = choice [
    function "pure" >> double >>= return . pure,
    function "return" >> double >>= return . return
    ]
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs


instance Pattern' (Ratio Integer) where
  simplePattern = choice [
    pure . fromIntegral <$> integer,
    pure . toRational <$> double,
    T.p <$> stringLiteral
    ]
  complexPattern = choice [
    function "pure" >> integer >>= return . pure . fromIntegral,
    function "return" >> double >>= return . return . toRational
    ]
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs


instance Pattern' String where
  simplePattern = T.p <$> stringLiteral
  complexPattern = choice [
    function "pure" >> stringLiteral >>= return . pure,
    function "return" >> stringLiteral >>= return . return
    ]
  mergeOperator = choice [] -- ??
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs


fractionalMergeOperator :: Fractional a => Parser (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator = op "/" >> return (/)

numMergeOperator :: Num a => Parser (Pattern a -> Pattern a -> Pattern a)
numMergeOperator = choice [
  op "+" >> return (+),
  op "-" >> return (-),
  op "*" >> return (*)
  ]

double :: Parser Double
double = parensOrNot $ choice [float,fromIntegral <$> integer]

int :: Parser Int
int = parensOrNot $ fromIntegral <$> integer

function :: String -> Parser ()
function x = reserved x <|> try (parens (function x))

op :: String -> Parser ()
op x = reservedOp x <|> try (parens (op x))

parensOrNot :: Parser a -> Parser a
parensOrNot p = p <|> try (parens (parensOrNot p))

nestedParens :: Parser a -> Parser a
nestedParens p = try (parens p) <|> try (parens (nestedParens p))

applied :: Parser a -> Parser a
applied p = op "$" >> p

appliedOrNot :: Parser a -> Parser a
appliedOrNot p = applied p <|> p

parensOrApplied :: Parser a -> Parser a
parensOrApplied p = try (parens p) <|> try (applied p)

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["chop","striate","striate'","stut","jux","brak","rev",
    "palindrome","fast","density","slow","iter","trunc","swingBy","every","whenmod",
    "append","silence","s","sound","n","up","speed","vowel","pan","shape","gain",
    "accelerate","bandf","bandq","begin","coarse","crush","cut","cutoff","delayfeedback",
    "delaytime","delay","end","hcutoff","hresonance","loop","resonance","shape","unit",
    "sinewave1","sinewave","sine1","sine","sawwave1","sawwave","saw1","saw",
    "triwave1","triwave","tri1","tri","squarewave1","square1","square","squarewave",
    "pure","return"],
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
