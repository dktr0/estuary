module Estuary.Languages.TiempoEspacio.Oir (oir) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Sound.Punctual.Extent
import Sound.Punctual.Graph
import Sound.Punctual.Types

oir :: Text -> Either ParseError [Expression]
oir = parse oirParser ""

oirParser :: Parser [Expression]
oirParser = do
  whiteSpace
  x <- sentence `sepBy` symbol "."
  eof
  return x

sentence :: Parser Expression
sentence = choice [
  try $ silence
  ]

-- // Opciones de gramática

silence :: Parser Expression
silence = do
  f <- option DefaultCrossFade fade
  o <- out
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f EmptyGraph) o



-- // Fade in/out

fade :: Parser Transition
fade = choice [
  reserved "cinco_" >> return (CrossFade (Seconds 5)),
  reserved "diez_" >> return (CrossFade (Seconds 10)),
  reserved "quince_" >> return (CrossFade (Seconds 15)),
  reserved "veinte_" >> return (CrossFade (Seconds 20))
  ]


-- // Salidas

out :: Parser Output
out = choice [
  try $ reservedOp "<<>>" >> return (NamedOutput (T.pack "rgb")),
  try $ reservedOp "<<" >> return (NamedOutput (T.pack "red")),
  try $ reservedOp ">>" >> return (NamedOutput (T.pack "green")),
  try $ reservedOp "<>" >> return (NamedOutput (T.pack "blue")),
  try $ reservedOp "<<_>>" >> return (NamedOutput (T.pack "alpha"))
  ]





-- //////////////////////////////////

average :: Graph -> Graph -> Graph
average x y = Product (Sum x y) (Constant 0.5)

difference :: Graph -> Graph -> Graph
difference x y = Sum x (Product y (Constant (-1)))

modulatedRangeGraph :: Graph -> Graph -> Graph -> Graph
modulatedRangeGraph low high mod = Sum (average low high) (Product (Product (difference high low) (Constant 0.5)) mod)

seconds :: Parser Duration
seconds = do
  x <- double
  reserved "s"
  return $ Seconds x

double :: Parser Double
double = choice [
  try $ parens double,
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]

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
