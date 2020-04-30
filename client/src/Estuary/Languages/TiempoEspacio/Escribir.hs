module Estuary.Languages.TiempoEspacio.Escribir (escribir) where

import Data.List

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim (many)
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal
--import Estuary.Tidal.ParamPatternable (parseBP')
import Control.Monad (forever)

parseBP' :: (Tidal.Enumerable a, Tidal.Parseable a) => String -> Tidal.Pattern a
parseBP' = (either (const Tidal.silence)  id). Tidal.parseBP

escribir :: String -> Either ParseError Tidal.ControlPattern
escribir s = parse escribirParser "escribir" s

escribirParser :: Parser Tidal.ControlPattern
escribirParser = whiteSpace >> choice [
  eof >> return Tidal.silence,
  do
    expr <- nuevaOracion escribirPattern
    eof
    return $ Tidal.stack expr
  ]

--Esta función divide oraciones
nuevaOracion s = sepBy s (comma)

escribirPattern :: Parser Tidal.ControlPattern
escribirPattern = oracion

oracion :: Parser Tidal.ControlPattern
oracion = do
  option () miscelanea
  option () miscelanea
  --s <- option id subject
  option () miscelanea
  option () miscelanea
  v <- option [" "] (many verbs)
  option () miscelanea
  option () miscelanea
  a <- option id adjective
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
  return $ a $ n $ n' $ n'' $ n''' $ n'''' $ n''''' $ Tidal.s $ parseBP' $ (unwords v)

verbs = choice [try verb'''', try verb''', try verb'', try verb', try verb ]

verb'''' = do
  v'' <- (brackets $ many verbOrVerb'')
  o' <- option "/" operator
  n' <- option 1 int
  return $ "[" ++ (unwords v'') ++ "]" ++ o' ++ (show n')

verbOrVerb'' = choice [try verb''', try verbOrVerb']

verb''' = do
  v'' <- (angles $ many verbOrVerb')
  return $ "<" ++ (unwords v'') ++ ">"

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


miscelanea :: Parser ()
miscelanea = choice [
        reserved "Yo" >> return (),
        -- reserved "La" >> return (),
        -- reserved "Las" >> return (),
        -- reserved "la" >> return (),
        -- reserved "las" >> return (),
        reserved "mi" >> return (),
        reserved "mis" >> return (),
        reserved "su" >> return (),
        reserved "sus" >> return (),
        reserved "Una" >> return (),
        reserved "una" >> return (),
        reserved "Un" >> return (),
        reserved "un" >> return (),
        reserved "palabras" >> return (),
        reserved "idioma" >> return (),
        reserved "idiomas" >> return (),
        reserved "ideas" >> return (),
        reserved "dedo" >> return (),
        reserved "Agosto" >> return (),
        reserved "con" >> return (),
        reserved "ajeno" >> return (),
        reserved "ajenos" >> return ()
      ]

--subject = choice [try articleAndSubject, try nounSubject]

nounSubject :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
nounSubject = choice [
  (reserved "Palabra" <|> reserved "Palabras" <|> reserved "Idioma" <|> reserved "Idiomas" <|> reserved "Ideas") >> option (Tidal.slow 1) (double' >>= return . Tidal.slow . pure . toRational),
  (reserved "Dedo" <|> reserved "Dedos" <|> reserved "Eggplant" <|> reserved "Eggplants") >> option (Tidal.fast 1) (double' >>= return . Tidal.fast . pure . toRational)
  ]

-- articleAndSubject' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern -> Tidal.ControlPattern)
-- articleAndSubject' =
--   (reserved "la" <|> reserved "las" <|> reserved "La" <|> reserved "Las" <|> reserved "los" <|> reserved "Los" <|> reserved "El" <|> reserved "el") >> return (int' >>= return . Tidal.every) <*> nounSubject

articleAndSubject :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
articleAndSubject =
  (reserved "la" <|> reserved "las" <|> reserved "La" <|> reserved "Las" <|> reserved "los" <|> reserved "Los" <|> reserved "El" <|> reserved "el") >> return (Tidal.every 1) <*> nounSubject

-- //////

verb :: Parser String
verb = choice [
  (reserved "oir" <|> reserved "oigo" <|> reserved "oyen" <|> reserved "oye" <|> reserved "oí") >> return "hear",
  (reserved "escribir" <|> reserved "escribo" <|> reserved "escriben" <|> reserved "escribe" <|> reserved "escribí") >> return "write",
  (reserved "observar" <|> reserved "observo" <|> reserved "observan" <|> reserved "observa" <|> reserved "observé") >> return "watch",
  (reserved "leer" <|> reserved "leo" <|> reserved "leen" <|> reserved "lee" <|> reserved "leí") >> return "read",
  (reserved "ver" <|> reserved "veo" <|> reserved "ven" <|> reserved "ve" <|> reserved "ví") >> return "see",
  (reserved "escuchar" <|> reserved "escucho" <|> reserved "escuchan" <|> reserved "escucha" <|> reserved "escuché") >> return "listen",
  espacio,
  silencio
  ]

espacio :: Parser String
espacio = (reserved "me" <|> reserved "que" <|> reserved "no" <|> reserved "se")  >> return "~"

silencio :: Parser String
silencio = reserved "silencio" >> return "~"

noun :: Parser Tidal.ControlPattern
noun = choice [
  ((reserved "sonido" <|> reserved "sonidos") >> return Tidal.up) <*> option 0 parentsdoublePattern,
  ((reserved "forma" <|> reserved "formas") >> return Tidal.gain) <*> option 1 parentsdoublePattern,
  ((reserved "textura" <|> reserved "texturas") >> return Tidal.pan) <*> option 0.5 parentsdoublePattern,
  (reserved "EGGPLANT" >> return Tidal.delay) <*> option 0 parentsdoublePattern,
  (reserved "eggPLANT" >> return Tidal.delayfeedback) <*> option 0 parentsdoublePattern,
  (reserved "EGGplant" >> return Tidal.delaytime) <*> option 0 parentsdoublePattern
  ]

nouns :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
nouns = do
  x <- noun
  return (Tidal.# x)

adjective' :: Parser Tidal.ControlPattern
adjective' = ((reserved "vívidas" <|> reserved "vívidos" <|> reserved "vívida" <|> reserved "vívido" <|> reserved "presurosas" <|> reserved "presurosos" <|> reserved "presurosa" <|> reserved "presuroso" <|> reserved "ansiosas" <|> reserved "ansiosos" <|> reserved "ansiosa" <|> reserved "ansioso") >> return Tidal.n) <*> option (Tidal.irand 0) (int' >>= return . Tidal.irand)

adjective :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
adjective = do
  x <- adjective'
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
--   return $ parseBP' $ (unwords p)

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


-- escribirIO :: Tidal.Stream -> String -> Either ParseError (IO ())
-- escribirIO tidal = parse (escribirIOParser tidal) "escribir"
--
-- escribirIOParser :: Tidal.Stream -> Parser (IO ())
-- escribirIOParser tidal = whiteSpace >> choice [
--   eof >> return (return ()),
--   dParser tidal <*> escribirPattern
--   ]
--
-- dParser :: Tidal.Stream -> Parser (ControlPattern -> IO ())
-- dParser tidal = choice [
--   return (Tidal.streamReplace tidal "1")
--   ]
--
-- main :: IO ()
-- main = do
--   tidal <- Tidal.startTidal Tidal.superdirtTarget Tidal.defaultConfig
--   forever $ do
--     cmd <- escribirIO tidal <$> getLine
--     either (\x -> putStrLn $ "error: " ++ show x) id cmd
--   return ()
