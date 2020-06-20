module Estuary.Languages.TiempoEspacio.Leer (leer) where

import Data.List

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim (many)
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal
import Control.Monad (forever)

parseBP' :: (Tidal.Enumerable a, Tidal.Parseable a) => String -> Tidal.Pattern a
parseBP' = (either (const Tidal.silence)  id). Tidal.parseBP

leer :: String -> Either ParseError Tidal.ControlPattern
leer s = parse leerParser "leer" s

leerParser :: Parser Tidal.ControlPattern
leerParser = whiteSpace >> choice [
  eof >> return Tidal.silence,
  do
    expr <- nuevaOracion leerPattern
    eof
    return $ Tidal.stack expr
  ]

--Esta función divide oraciones
nuevaOracion s = sepBy s (symbol ".")

leerPattern :: Parser Tidal.ControlPattern
leerPattern = oracion

oracion :: Parser Tidal.ControlPattern
oracion = do
  nu <- option id numbers
  option () miscelanea
  option () miscelanea
  p <- option id pronouns
  ns <- option id adverbs
  fn <- option id fakeverb
  option () miscelanea
  option () miscelanea
  v <- option [" "] (many verbs)
  option () miscelanea
  option () miscelanea
  a <- option id gerundio
  option () miscelanea
  option () miscelanea
  n <- option id nouns
  option () miscelanea
  option () miscelanea
  n' <- option id nouns
  option () miscelanea
  option () miscelanea
  n'' <- option id nouns
  option () miscelanea
  option () miscelanea
  n''' <- option id nouns
  option () miscelanea
  option () miscelanea
  n'''' <- option id nouns
  option () miscelanea
  option () miscelanea
  n''''' <- option id nouns
  option () miscelanea
  return $ nu $ p $ ns $ a $ fn $ n $ n' $ n'' $ n''' $ n'''' $ n''''' $ Tidal.s $ parseBP' $ (unwords v)


-- ////////////////

verbs = choice [expandVerbs, try verbOrVerb'']

expandVerbs = do
  v'' <- (brackets $ many verbOrVerb'')
  o' <- option "/" operator
  n' <- option 1 int
  return $ "[" ++ (unwords v'') ++ "]" ++ o' ++ (show n')

verbOrVerb'' = choice [try changingVerb, try verbOrVerb']

changingVerb = do
  v'' <- (angles $ many verbOrVerb')
  return $ "<" ++ (unwords v'') ++ ">"

verbOrVerb' = choice [try multiplyVerb, try maybeVerb, try verbOrVerb]

multiplyVerb = do
  v' <- verbOrVerb
  (symbol "*")
  n <- int
  return $ v' ++ "*" ++ (show n)

maybeVerb = do
  v <- verbOrVerb
  (symbol "?")
  return $ v ++ "?"

verbOrVerb = choice [try verbNumber, try verb]

verbNumber = do
  v <- verb
  (symbol ":")
  s <- option 0 int
  return $ v ++ ":" ++ (show s)

-- ////////////////

miscelanea :: Parser ()
miscelanea = choice [
        reserved "myself" >> return (),
        reserved "on" >> return (),
        reserved "in" >> return (),
        reserved "that" >> return (),
        reserved "The" >> return (),
        reserved "the" >> return (),
        reserved "to" >> return (),
        reserved "with" >> return (),
        reserved "everything" >> return (),
        reserved "everything" >> return (),
        reserved "suspended" >> return (),
        reserved "without" >> return (),
        reserved "through" >> return (),
        reserved "Through" >> return (),
        reserved "down" >> return ()
      ]

-- //////
numbers :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
numbers = choice [
  (reserved "one_" <|> reserved "two_" <|> reserved "three_") >> return Tidal.jux <*> numbers',
  (reserved "four_" <|> reserved "five_" <|> reserved "six_") >> return Tidal.juxBy <*> parentsdoublePattern <*> numbers'
  ]

numbers' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
numbers' = return Tidal.rev

pronouns :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
pronouns = ((reserved "I" <|> reserved "We" <|> reserved "we" <|> reserved "They" <|> reserved "they") >> return Tidal.striate) <*> option 0 intPattern'

adverbs :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
adverbs = (reserved "always" <|> reserved "Sometimes" <|> reserved "sometimes" <|> reserved "Often" <|> reserved "often" <|> reserved "rarelly" <|> reserved "never") >> return Tidal.every <*> option 0 intPattern' <*> fakeverb

fakeverb :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
fakeverb = choice [
  (reserved "want" <|> reserved "wants" <|> reserved "wanted") >> option (Tidal.slow 1) (double' >>= return . Tidal.slow . pure . toRational),
  (reserved "like" <|> reserved "likes" <|> reserved "liked") >> option (Tidal.slow 1) (double' >>= return . Tidal.fast . pure . toRational)
  ]

-- //////

verb :: Parser String
verb = choice [
  (reserved "hear" <|> reserved "hearing" <|> reserved "hears" <|> reserved "heard") >> return "oir",
  (reserved "write" <|> reserved "writing" <|> reserved "writes" <|> reserved "wrote" <|> reserved "written") >> return "escribir",
  (reserved "watch" <|> reserved "watching" <|> reserved "watches" <|> reserved "watched") >> return "observar",
  (reserved "read" <|> reserved "reading" <|> reserved "reads") >> return "leer",
  (reserved "see" <|> reserved "seeing" <|> reserved "sees" <|> reserved "saw" <|> reserved "seen") >> return "ver",
  (reserved "listen" <|> reserved "listening" <|> reserved "listens" <|> reserved "listened") >> return "escuchar",
  espacio,
  silencio
  ]

espacio :: Parser String
espacio = (reserved "do" <|> reserved "does" <|> reserved "am" <|> reserved "are" <|> reserved "is" <|> reserved "was" <|> reserved "were" <|> reserved "did" <|> reserved "not")  >> return "~"

silencio :: Parser String
silencio = reserved "silence" >> return "~"

-- //////

gerundio' :: Parser Tidal.ControlPattern
gerundio' = ((reserved "imagining" <|> reserved "multiplying" <|> reserved "swinging" <|> reserved "lying") >> return Tidal.n) <*> option (Tidal.irand 0) (int' >>= return . Tidal.irand)

gerundio :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
gerundio = do
  x <- gerundio'
  return (Tidal.# x)

-- //////

noun :: Parser Tidal.ControlPattern
noun = choice [
  ((reserved "sound" <|> reserved "sounds") >> return Tidal.up) <*> option 0 parentsdoublePattern,
  ((reserved "light" <|> reserved "lights") >> return Tidal.gain) <*> option 1 parentsdoublePattern,
  ((reserved "door" <|> reserved "doors") >> return Tidal.pan) <*> option 0.5 parentsdoublePattern,
  ((reserved "time" <|> reserved "times") >> return Tidal.delay) <*> option 0 parentsdoublePattern,
  ((reserved "space" <|> reserved "spaces") >> return Tidal.delayfeedback) <*> option 0 parentsdoublePattern,
  ((reserved "room" <|> reserved "rooms") >> return Tidal.delaytime) <*> option 0 parentsdoublePattern,
  ((reserved "word" <|> reserved "words") >> return Tidal.begin) <*> option 0.0 parentsdoublePattern,
  ((reserved "vowel" <|> reserved "vowels") >> return Tidal.end) <*> option 1.0 parentsdoublePattern,
  ((reserved "hall" <|> reserved "halls") >> return Tidal.room) <*> option 0 parentsdoublePattern,
  ((reserved "wall" <|> reserved "walls") >> return Tidal.size) <*> option 0 parentsdoublePattern
  ]

nouns' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
nouns' = do
  x <- noun
  return (Tidal.# x)

nouns :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
nouns = nouns'

variosNoun :: Parser Tidal.ControlPattern
variosNoun = choice [
  reserved "French" >> return (Tidal.vowel (parseBP' "a")),
  reserved "English" >> return (Tidal.vowel (parseBP' "e")),
  reserved "books" >> return (Tidal.vowel (parseBP' "i")),
  reserved "bus" >> return (Tidal.vowel (parseBP' "o")),
  reserved "swing" >> return (Tidal.vowel (parseBP' "u"))
  ]

variosNouns :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
variosNouns = do
  x <- variosNoun
  return (Tidal.# x)

-- ////////////////
-- Right not you can only do ([]) or (<>), not a pattern of ([][]) or (<><>)
parentsdoublePattern = choice [
   try (parens $ stringNegativeDoublePattern''),
   try (parens $ stringPattern)
   ]

stringNegativeDoublePattern'' = choice [
   try patternWithBrackets,
   try patternWithAngles
   ]

patternWithBrackets = do
  (symbol "[")
  p <-  (many muchosdoubles)
  (symbol "]")
  o <- option "/" operator
  n <- option 1 int
  return $ parseBP' $ "[" ++ (unwords p) ++ "]" ++ o ++ (show n)

-- muchosPatternWithAngles = do
--   p <- (many patternWithAngles)
--   return $ show p

patternWithAngles = do
  (symbol "<")
  p <-  (many muchosdoubles)
  (symbol ">")
  return $ parseBP' $ "<" ++ (unwords p) ++ ">"

muchosdoubles = do
  d <- double
  return $ show d

stringPattern = do
  p <-  (many muchosdoubles)
  return $ parseBP' $ (unwords p)

double' = do
  a <- parens $ float
  return a

double :: Parser Double
double = choice [
  try $ parens double,
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]

-- /////

operators :: Parser String
operators = choice [
         reserved "+" >> return "+",
         reserved "-" >> return "-",
         reserved "*" >> return "*",
         reserved "/" >> return "/"
         ]

-- /////

intPattern' = do
  a <- parens $ intPattern
  return a

intPattern :: Parser (Pattern Int)
intPattern = do
  p <- (many muchosint)
  return $ parseBP' $ (unwords p)

muchosint = do
  d <- int
  return $ show d

intOrNegativeInt = choice [ try negativeInt', try int']

int' = do
  a <- parens $ int
  return a

negativeInt' = parens $ negativeInt

negativeInt = do
  a <- symbol "-"
  b <- int
  return $ (-1) * b

int :: Parser Int
int = fromIntegral <$> integer


--Funciones de la librería TokenParser

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = []
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
