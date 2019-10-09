module Estuary.Languages.Morelia.Uno (uno) where

-- librerías que se tienen que importar

import Data.List

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim (many)
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal
import Control.Monad (forever)

-- ///////////////////
--Función importada de Tidal (permite hacer patrones)
parseBP' :: (Tidal.Enumerable a, Tidal.Parseable a) => String -> Tidal.Pattern a
parseBP' = (either (const Tidal.silence)  id). Tidal.parseBP


-- ///////////////////
--Mi Parser (Un String será error o será un patrón de Tidal)
uno :: String -> Either ParseError Tidal.ControlPattern
uno s = parse unoParser "uno" s

--Esta parte permite leer espacios en blanco o permite leer sin espacios en blanco
unoParser :: Parser Tidal.ControlPattern
unoParser = whiteSpace >> choice [
  eof >> return Tidal.silence,
  do
    expr <- nuevaOracion unoPattern
    eof
    return $ Tidal.stack expr
  ]

--Función que permite tener diferentes oraciones y definir cómo se dividen
--En este caso con un symbolo .
nuevaOracion s = sepBy s (symbol "/")

--Función de patron de nuestro parser que es un parseo del patrón de Tidal
unoPattern :: Parser Tidal.ControlPattern
unoPattern = oracion


-- //////////////
--OPCIONES DE ORACIONES

oracion :: Parser Tidal.ControlPattern
oracion = do  -- [sonido sonido] t t t
  option () miscelanea
  symbol "["
  a <- option [" "] (many audios)
  symbol "]"
  option () miscelanea
  t <- option id transformaciones
  option () miscelanea
  t' <- option id transformaciones
  option () miscelanea
  t'' <- option id transformaciones
  option () miscelanea
  t''' <- option id transformaciones
  option () miscelanea
  return $ t $ t' $ t'' $ t''' $ Tidal.s $ parseBP' $ (unwords a)


audios = choice [try audio', try audio]

audio' = do
  a <- audio
  (symbol ":")
  s <- option 0 int
  return $ a ++ ":" ++ (show s)

int :: Parser Int
int = fromIntegral <$> integer
-- ////////////////////
-- MIS FUNCIONES

miscelanea :: Parser ()
miscelanea = choice [
  reserved "monstruosas" >> return (),
  reserved "pidos" >> return (),
  reserved "invierno" >> return (),
  reserved "salvación" >> return ()
  ]

audio :: Parser String
audio = choice [
  reserved "vida" >> return "vida",
  reserved "infancia" >> return "infancia",
  reserved "perro" >> return "perro",
  (reserved "vidas" <|> reserved "infancias" <|> reserved "perros") >> return "palabra"
  ]

transformacion :: Parser Tidal.ControlPattern
transformacion = choice [
  ((reserved "anchos" <|> reserved "altos" <|> reserved "existir") >> return Tidal.up) <*> option 0 doublePattern,
  (reserved "sollozos" >> return Tidal.gain) <*> option 1 doublePattern,
  (reserved "miedo" >> return Tidal.delaytime) <*> option 0 doublePattern
  ]

transformaciones :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
transformaciones = do
  x <- transformacion
  return (Tidal.# x)


-- ////////////////
--PATRONES DE NÚMEROS

doublePattern = do
  p <- (many double)
  return $ parseBP' $ (unwords (fmap show p))

double :: Parser Double
double = choice [
  try $ parens double,
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]


-- ///////////////////////////////

--Funciones de la librería TokenParser
-- que se tienen que copiar

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
