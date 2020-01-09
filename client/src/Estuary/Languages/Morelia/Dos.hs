module Estuary.Languages.Morelia.Dos (dos) where

-- librerías que se tienen que importar

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as P
import Control.Monad.Identity (Identity)
import Control.Applicative

import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Program



-- ///////////////////
--Mi Parser (Un String será error o Expresión de Punctual)
dos :: Text -> Either ParseError Program
dos = parse empty ""

{-
--Función que te permite aceptar espacios y dividir oraciones
dosParser :: Parser [Expression]
dosParser = do
  whiteSpace
  x <- oracion `sepBy` symbol "."
  eof
  return x


-- //////////////
--OPCIONES DE ORACIONES

oracion :: Parser Expression
oracion = choice [
  try $ silencio,
  try $ filtroOnda
  ]

-- // Opciones de gramática
silencio :: Parser Expression
silencio = do
  f <- option DefaultCrossFade fade
  o <- out
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f EmptyGraph) o

filtroOnda :: Parser Expression
filtroOnda = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  fi <- filtro
  s <- onda
  option () miscelanea
  l <- parens $ level
  option () miscelanea
  o <- out
  let g = fi s
  let h = Product g (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f h) o


-- ////////////////////
-- MIS FUNCIONES

miscelanea :: Parser ()
miscelanea = choice [
  reservedOp "%" >> return (),
  reserved "mi" >> return (),
  reserved "amigos" >> return (),
  reserved "extrañas" >> return ()
  ]

-- // Fade in/out

fade :: Parser Transition
fade = choice [
  reserved "cinco" >> return (CrossFade (Seconds 5))
  ]

-- //  HPF / LPF

filtro :: Parser (Graph -> Graph)
filtro = choice [
  reserved "alto" >> return (\x -> HPF x (modulatedRangeGraph (Constant 100) (Constant 160) (Sine (Constant 1))) (Constant 1)),

  reserved "lento" >> return (\x -> LPF x (modulatedRangeGraph (Constant 100) (Constant 160) (Saw (Constant 1))) (Constant 1))
  ]

-- // ondas
onda :: Parser Graph
onda = choice [ -- sin [x, x]
  reserved "todo" >> return (Sine (Multi [Constant 250.148,Constant 164.42])),
  reserved "poliester" >> return (Square (Multi [Constant 184.14,Constant 342.2]))
  ]

-- // Salidas

level :: Parser Extent
level = do
  x <- doble
  return $ dbamp x

doble = choice [
  try $ reserved "algodón" >> return (-20.0),
  try $ reserved "monstruosas" >> return (-5.0)
  ]

out :: Parser Output
out = choice [
  try $ reservedOp "<=>" >> return (PannedOutput 0.5),
  try $ reservedOp "=>" >> return (PannedOutput 1.0),
  try $ reservedOp "<=" >> return (PannedOutput 0.0)
  ]


-- ////////////////
--DOBLES

double :: Parser Double
double = choice [
  try $ parens double,
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]


-- ///////////////////////////////
--Funciones de la librería TokenParser

tokenParser :: P.GenTokenParser Text () Identity
tokenParser = P.makeTokenParser $ P.LanguageDef {
  P.commentStart = "{-",
  P.commentEnd = "-}",
  P.commentLine = "--",
  P.nestedComments = False,
  P.identStart = letter <|> char '_',
  P.identLetter = alphaNum <|> char '_',
  P.opStart = oneOf "+*:@<>~=%",
  P.opLetter = oneOf "+*:@<>~=%",
  P.reservedNames = ["c","s","ms","db","sin","tri","saw","sqr","noise","pink","fx","fy",
  "lpf","hpf","mix","x","y","red","green","blue","alpha","clear","width","height",
  "left","right","centre","bipolar","unipolar","linlin","rect","px","py","m","abs","splay",
  "point","hline","vline","rgb"],
  P.reservedOpNames = ["+","*","/",":","@","<>","~","=","%",";","+-","..","=>","==","!=","<",">","<=",">="],
  P.caseSensitive = False
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
-}
