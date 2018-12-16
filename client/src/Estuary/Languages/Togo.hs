module Estuary.Languages.Togo (togo) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal

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
  let n1 = pure $ fromIntegral a
  let n2 = pure $ fromIntegral b
  let onPattern = Tidal.fast (pure $ fromIntegral b) c
  let offPattern = Tidal.fast (pure $ fromIntegral b) d
  return $ slowness $ rotation $ Tidal.stack [Tidal.euclid n1 n2 onPattern, Tidal.euclidInv n1 n2 offPattern]

slownessParser :: Parser (ControlPattern -> ControlPattern)
slownessParser = option (Tidal.slow 1) $ reservedOp "/" >> double >>= return . Tidal.slow . pure . toRational

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
  let n1 = pure $ fromIntegral a
  let n2 = pure $ fromIntegral b
  let onPattern = Tidal.fast (pure $ fromIntegral b) c
  return $ slowness $ rotation $ Tidal.euclid n1 n2 onPattern

euclidInvPattern :: Parser ControlPattern
euclidInvPattern = do
  a <- natural
  reservedOp "_"
  b <- natural
  rotation <- (Tidal.rotR . toRational . (/fromIntegral b)) <$> rotationParser
  slowness <- slownessParser
  c <- togoPatternAsArg
  let n1 = pure $ fromIntegral a
  let n2 = pure $ fromIntegral b
  let offPattern = Tidal.fast (pure $ fromIntegral b) c
  return $ slowness $ rotation $ Tidal.euclidInv n1 n2 offPattern

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
