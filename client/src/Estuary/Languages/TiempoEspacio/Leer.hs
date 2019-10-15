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
  option () miscelanea
  option () miscelanea
  ns <- option id adverbs
  option () miscelanea
  option () miscelanea
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
  return $ ns $ a $ fn $ n $ n' $ n'' $ n''' $ n'''' $ n''''' $ Tidal.s $ parseBP' $ (unwords v)
--s "p ++ v ++ o ++ n ++ p"

verbs = choice [try verb''', try verb'', try verb', try verb ]--try simpleVerb, try simpleVerb']

verb''' = do
  v'' <- (brackets $ many verbOrVerb')
  o' <- option "/" operator
  n' <- option 1 int
  return $ "[" ++ (unwords v'') ++ "]" ++ o' ++ (show n')

verbOrVerb' = choice [try verb'', try verb', try verb]

verb'' = do
  v' <- verbOrVerb
  (symbol "*")
  n <- int
  return $ v' ++ "*" ++ (show n)

verbOrVerb = choice [try verb', try verb]

verb' = do
  v <- verb
  (symbol ":")
  s <- option 0 int
  return $ v ++ ":" ++ (show s)

-- simpleVerb = do
--   e' <- option "" espacio
--   v <- verb
--   e <- option "" espacio
--   return $ e' ++ v ++ e


miscelanea :: Parser ()
miscelanea = choice [
        reserved "I" >> return (),
        reserved "We" >> return (),
        reserved "we" >> return (),
        reserved "They" >> return (),
        reserved "they" >> return (),
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
        reserved "Words" >> return (),
        reserved "words" >> return (),
        reserved "without" >> return (),
        reserved "through" >> return (),
        reserved "Through" >> return (),
        reserved "down" >> return ()
      ]


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

noun :: Parser Tidal.ControlPattern
noun = choice [
  ((reserved "sound" <|> reserved "sounds") >> return Tidal.up) <*> option 0 parentsdoublePattern,
  ((reserved "light" <|> reserved "lights") >> return Tidal.gain) <*> option 1 parentsdoublePattern,
  ((reserved "door" <|> reserved "doors") >> return Tidal.pan) <*> option 0.5 parentsdoublePattern,
  ((reserved "time" <|> reserved "times") >> return Tidal.delay) <*> option 0 parentsdoublePattern,
  ((reserved "space" <|> reserved "spaces") >> return Tidal.delayfeedback) <*> option 0 parentsdoublePattern,
  ((reserved "room" <|> reserved "rooms") >> return Tidal.delaytime) <*> option 0 parentsdoublePattern
  ]

nouns' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
nouns' = do
  x <- noun
  return (Tidal.# x)

nouns :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
nouns = nouns'

fakeverb :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
fakeverb = (reserved "want" <|> reserved "wants" <|> reserved "wanted" <|> reserved "like" <|> reserved "likes" <|> reserved "liked") >> option (Tidal.slow 1) (double' >>= return . Tidal.slow . pure . toRational)

gerundio' :: Parser Tidal.ControlPattern
gerundio' = ((reserved "imagining" <|> reserved "multiplying" <|> reserved "swinging" <|> reserved "lying") >> return Tidal.n) <*> option (Tidal.irand 0) (int' >>= return . Tidal.irand)

gerundio :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
gerundio = do
  x <- gerundio'
  return (Tidal.# x)

adverbs = choice [try adverbs'', try adverbs']

adverbs' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
adverbs' = choice [
  reserved "always" >> return (Tidal.every 2) <*> (double' >>= return . Tidal.slow . pure . toRational),
  (reserved "Sometimes" <|> reserved "sometimes") >> return (Tidal.every 3) <*> (double' >>= return . Tidal.slow . pure . toRational),
  (reserved "Often" <|> reserved "often") >> return (Tidal.every 4) <*> (double' >>= return . Tidal.slow . pure . toRational),
  reserved "rarely" >> return (Tidal.every 5) <*> (double' >>= return . Tidal.slow . pure . toRational),
  reserved "never" >> return (Tidal.every 6) <*> (double' >>= return . Tidal.slow . pure . toRational)
  ]

adverbs'' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
adverbs'' = choice [
  (reserved "Palabra" <|> reserved "Palabras") >> return (Tidal.every 2) <*> fakeverb,
  (reserved "Dedo" <|> reserved "Dedos") >> return (Tidal.every 3) <*> fakeverb,
  (reserved "Idioma" <|> reserved "Idiomas") >> return (Tidal.every 4) <*> fakeverb,
  reserved "Ideas" >> return (Tidal.every 5) <*> fakeverb,
  reserved "Eggplant" >> return (Tidal.every 6) <*> fakeverb
  ]


-- ////////////////
parentsdoublePattern = choice [
   try (parens $ stringNegativeDoublePattern),
   try (parens $ stringPattern)
   ]

muchosdoubles = do
  d <- doublePattern
  return $ show d

stringPattern = do
  p <-  (many muchosdoubles)
  return $ parseBP' $ (unwords p)

stringNegativeDoublePattern = do
  (symbol "[")
  p <-  (many muchosdoubles)
  (symbol "]")
  o <- option "/" operator
  n <- option 1 int
  return $ parseBP' $ "[" ++ (unwords p) ++ "]" ++ o ++ (show n)


doublePattern = choice [
  negativeDouble,
  double
  ]
-- ////////////////

double = float

double' = do
  a <- parens $ float
  return a

negativeDouble = do
  a <- symbol "-"
  b <- float
  return $ (-1) * b

-- /////

operators :: Parser String
operators = choice [
         reserved "+" >> return "+",
         reserved "-" >> return "-",
         reserved "*" >> return "*",
         reserved "/" >> return "/"
         ]

intPattern :: Parser (Pattern Int)
intPattern = pure <$> int

int :: Parser Int
int = fromIntegral <$> integer

int' = do
  a <- parens $ int
  return a


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
