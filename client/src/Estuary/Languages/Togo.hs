module Estuary.Languages.Togo (togo) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Bool
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Tidal.Bjorklund as Tidal

-- Examples:
-- 3x8/8<<1 "bd n=0 shape=0.2" "cp"
-- 3X8 (2x2 "bd" "cp") "sn"
-- 3x8 "bd:0"
-- 3_8 "cp"

togo :: String -> Either ParseError ControlPattern
togo = parse togoParser "togo"

togoParser :: Parser ControlPattern
togoParser = whiteSpace >> choice [
  eof >> return Tidal.silence,
  do
    ps <- semiSep1 togoPattern
    eof
    return $ Tidal.stack ps
    ]

togoPattern :: Parser ControlPattern
togoPattern = choice [parens togoPattern, euclidPatterns, samplePattern, silence ]

togoPatternAsArg :: Parser ControlPattern
togoPatternAsArg = choice [ parens togoPattern, samplePattern, silence ]

euclidPatterns :: Parser ControlPattern
euclidPatterns = choice [try euclidFullPattern,try euclidPattern,euclidInvPattern]

euclidFullPattern :: Parser ControlPattern
euclidFullPattern = do
  a <- natural
  reservedOp "X"
  b <- natural
  rotation <- (Tidal.rotR . toRational . (/fromIntegral b)) <$> rotationParser
  slowness <- slownessParser
  c <- togoPatternAsArg
  d <- togoPatternAsArg
  let n1 = fromIntegral a
  let n2 = fromIntegral b
  return $ slowness $ rotation $ _euclidFull n1 n2 c d

slownessParser :: Parser (ControlPattern -> ControlPattern)
slownessParser = option (Tidal._slow 1) $ reservedOp "/" >> double >>= return . Tidal._slow . toRational

rotationParser :: Parser Double
rotationParser = option 0 $ choice [
  reservedOp ">>" >> double,
  reservedOp "<<" >> (* (-1)) <$> double
  ]

euclidPattern :: Parser ControlPattern
euclidPattern = do
  a <- natural
  reservedOp "x"
  b <- natural
  rotation <- (Tidal.rotR . toRational . (/fromIntegral b)) <$> rotationParser
  slowness <- slownessParser
  c <- togoPatternAsArg
  let n1 = fromIntegral a
  let n2 = fromIntegral b
  return $ slowness $ rotation $ _euclid n1 n2 c

euclidInvPattern :: Parser ControlPattern
euclidInvPattern = do
  a <- natural
  reservedOp "_"
  b <- natural
  rotation <- (Tidal.rotR . toRational . (/fromIntegral b)) <$> rotationParser
  slowness <- slownessParser
  c <- togoPatternAsArg
  let n1 = fromIntegral a
  let n2 = fromIntegral b
  return $ slowness $ rotation $ _euclidInv n1 n2 c

-- alternate definitions of _euclid,_euclidInv,and _euclidFull for performance gains... keeping them for now because Tidal has only adopted one of them and we're not sure what the performance impact of that is...

_euclid :: Int -> Int -> ControlPattern -> ControlPattern
_euclid n k a = Tidal.fastcat $ fmap (bool Tidal.silence a) $ Tidal.bjorklund (n,k)

_euclidInv :: Int -> Int -> ControlPattern -> ControlPattern
_euclidInv n k a = Tidal.fastcat $ fmap (bool a Tidal.silence) $ Tidal.bjorklund (n,k)

_euclidFull :: Int -> Int -> ControlPattern -> ControlPattern -> ControlPattern
_euclidFull n k a b = Tidal.fastcat $ fmap (bool b a) $ Tidal.bjorklund (n,k)

samplePattern :: Parser ControlPattern
samplePattern = do
  char '\"' >> whiteSpace
  s <- identifier
  kv <- keysValuesParser
  char '\"' >> whiteSpace
  return $ kv $ (Tidal.s $ pure s)

keysValuesParser :: Parser (ControlPattern -> ControlPattern)
keysValuesParser = option id $ do
  xs <- many1 keyValueParser
  return $ foldl1 (.) xs

keyValueParser :: Parser (ControlPattern -> ControlPattern)
keyValueParser = do
  x <- choice [
    (reserved "n" >> reservedOp "=" >> return Tidal.n) <*> doublePatternParser,
    (reserved "shape" >> reservedOp "=" >> return Tidal.shape) <*> doublePatternParser,
    (reserved "up" >> reservedOp "=" >> return Tidal.up) <*> doublePatternParser,
    (reserved "note" >> reservedOp "=" >> return Tidal.note) <*> doublePatternParser,
    (reserved "coarse" >> reservedOp "=" >> return Tidal.coarse) <*> intPatternParser,
    (reserved "cut" >> reservedOp "=" >> return Tidal.cut) <*> intPatternParser,
    (reserved "speed" >> reservedOp "=" >> return Tidal.speed) <*> doublePatternParser,
    (reserved "pan" >> reservedOp "=" >> return Tidal.pan) <*> doublePatternParser,
    (reserved "gain" >> reservedOp "=" >> return Tidal.gain) <*> doublePatternParser,
    (reserved "accelerate" >> reservedOp "=" >> return Tidal.accelerate) <*> doublePatternParser,
    (reserved "bandf" >> reservedOp "=" >> return Tidal.bandf) <*> doublePatternParser,
    (reserved "bandq" >> reservedOp "=" >> return Tidal.bandq) <*> doublePatternParser,
    (reserved "begin" >> reservedOp "=" >> return Tidal.begin) <*> doublePatternParser,
    (reserved "crush" >> reservedOp "=" >> return Tidal.crush) <*> doublePatternParser,
    (reserved "cutoff" >> reservedOp "=" >> return Tidal.cutoff) <*> doublePatternParser,
    (reserved "delayfeedback" >> reservedOp "=" >> return Tidal.delayfeedback) <*> doublePatternParser,
    (reserved "delaytime" >> reservedOp "=" >> return Tidal.delaytime) <*> doublePatternParser,
    (reserved "delay" >> reservedOp "=" >> return Tidal.delay) <*> doublePatternParser,
    (reserved "end" >> reservedOp "=" >> return Tidal.end) <*> doublePatternParser,
    (reserved "hcutoff" >> reservedOp "=" >> return Tidal.hcutoff) <*> doublePatternParser,
    (reserved "hresonance" >> reservedOp "=" >> return Tidal.hresonance) <*> doublePatternParser,
    (reserved "loop" >> reservedOp "=" >> return Tidal.loop) <*> doublePatternParser,
    (reserved "vowel" >> reservedOp "=" >> return Tidal.vowel) <*> stringPatternParser,
    (reserved "unit" >> reservedOp "=" >> return Tidal.unit) <*> stringPatternParser
    ]
  return $ \cp -> cp Tidal.# x

intPatternParser :: Parser (Pattern Int)
intPatternParser = fromIntegral <$> integer

doublePatternParser :: Parser (Pattern Double)
doublePatternParser = pure <$> double

stringPatternParser :: Parser (Pattern String)
stringPatternParser = pure <$> identifier

silence :: Parser ControlPattern
silence = choice [ reserved "silence", reserved "~" ] >> return Tidal.silence

double :: Parser Double
double = choice [
  try $ symbol "-" >> float >>= return . (* (-1)),
  try $ float,
  fromInteger <$> integer
  ]

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.opStart = oneOf "Xx_/><",
  P.opLetter = oneOf "Xx_/><",
  P.reservedNames = [
    "silence","~","coarse","cut","speed","pan","gain","accelerate","bandf",
    "bandq","begin","crush","cutoff","delayfeedback","delaytime","delay","end",
    "hcutoff","hresonance","loop","vowel","unit","n","shape","note","up"],
  P.reservedOpNames = ["X","x","_","/",">>","<<"]
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
