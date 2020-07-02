{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Hydra.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text

import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Monad.Identity (Identity)

import Estuary.Languages.Hydra.Types
import Estuary.Languages.Hydra.Test

----

hydra :: Text -> Either ParseError Statement
hydra s = parse hydraStatement "hydra" s

hydraStatement :: Parser Statement
hydraStatement = choice [
  sourceOut,
  renderOut
  ]

-- Source Output Statement
sourceOut :: Parser Statement
sourceOut = do
  s <- parserSource
  symbol "."
  o <- parserOut
  return $ sourceOutputToStatement s o

sourceOutputToStatement :: Source -> Output -> Statement
sourceOutputToStatement s o = Out s o

-- Render Statement
-- render()
renderOut :: Parser Statement
renderOut = do
  reserved "render"
  o <- parens $ parserOutput
  return $ outputToStatement o

outputToStatement :: Output -> Statement
outputToStatement o = Render o


--------- Adding sources

sourceAsArgument :: Parser Source
sourceAsArgument = choice [
  parserSource,
  list,
  constantDouble,
  constantInt
  ]

parserSource :: Parser Source
parserSource = choice [
  osc,
  solid,
  gradient,
  noise,
  shape,
  voronoi
  ]

--------- parserSource

-- osc() -- osc(0.3) -- osc(0.3,0.5) -- osc(0,10,0.5) -- osc([0.4,0.5],1.0,0.2)
osc :: Parser Source
osc = do
  reserved "osc"
  p <- parens $ sepBy sourceAsArgument (comma)
  case p of
      [] -> return $ Osc Nothing Nothing Nothing
      (x:[]) -> return $ Osc (Just x) Nothing Nothing
      (x:y:[]) -> return $ Osc (Just x ) (Just y) Nothing
      (x:y:z:_) -> return $ Osc (Just x) (Just y) (Just z)

-- solid() -- solid(0.5) -- solid(0.2,[0.1,0.2,0.3]) -- solid(1,0.5,1) -- solid(1,0.5,0.2,0.7)
solid :: Parser Source
solid = do
  reserved "solid"
  p <- parens $ sepBy sourceAsArgument (comma)
  case p of
      [] -> return $ Solid Nothing Nothing Nothing Nothing
      (x:[]) -> return $ Solid (Just x) Nothing Nothing Nothing
      (x:y:[]) -> return $ Solid (Just x) (Just y) Nothing Nothing
      (x:y:z:[]) -> return $ Solid (Just x) (Just y) (Just z) Nothing
      (x:y:z:v:_) -> return $ Solid (Just x) (Just y) (Just z) (Just v)

-- gradient() -- gradient(0.4) -- gradient(osc())
gradient :: Parser Source
gradient = do
  reserved "gradient"
  p <- parens $ sepBy sourceAsArgument (comma)
  case p of
      [] -> return $ Gradient Nothing
      (x:_) -> return $ Gradient (Just x)

-- noise() -- noise([5,10]) -- noise(0.5,0.7)
noise :: Parser Source
noise = do
  reserved "noise"
  p <- parens $ sepBy sourceAsArgument (comma)
  case p of
      [] -> return $ Noise Nothing Nothing
      (x:[]) -> return $ Noise (Just x) Nothing
      (x:y:_) -> return $ Noise (Just x) (Just y)

-- shape() -- shape(osc()) -- shape(0.5,noise(),gradient())
shape :: Parser Source
shape = do
  reserved "shape"
  p <- parens $ sepBy sourceAsArgument (comma)
  case p of
      [] -> return $ Shape Nothing Nothing Nothing
      (x:[]) -> return $ Shape (Just x) Nothing Nothing
      (x:y:[]) -> return $ Shape (Just x) (Just y) Nothing
      (x:y:z:_) -> return $ Shape (Just x) (Just y) (Just z)

-- voronoi() -- voronoi([0.5,0.8,0.3]) -- voronoi(10,0.5,0.1)
voronoi :: Parser Source
voronoi = do
  reserved "voronoi"
  p <- parens $ sepBy sourceAsArgument (comma)
  case p of
      [] -> return $ Shape Nothing Nothing Nothing
      (x:[]) -> return $ Shape (Just x) Nothing Nothing
      (x:y:[]) -> return $ Shape (Just x) (Just y) Nothing
      (x:y:z:_) -> return $ Shape (Just x) (Just y) (Just z)

--------- sourceAsArgument

-- [0.2,0.4] -- [0.3,0.4,1.0]
list :: Parser Source
list = do
  n <- brackets $ sepBy sourceAsArgument (comma)
  return $ List n

-- [].fast() -- [].fast(0.5) -- [].fast([0.5,0.2])
-- fast :: Parser Source
-- fast = do
--   symbol "."
--   reserved "fast"
--   p <- parens $ sepBy sourceAsArgument (comma)
--   case p of
--       [] -> return $ Fast Nothing
--       (x:_) -> return $ Fast (Just x)

-- 0.2 -- 4.0
constantDouble :: Parser Source
constantDouble = do
  n <- double
  return $ ConstantDouble n

constantInt :: Parser Source
constantInt = do
  n <- int
  return $ ConstantInt n


---------

-- Adding outputs
-- out() -- out(O1) -- out(O2) -- out(O3)
parserOut :: Parser Output
parserOut = do
  reserved "out"
  o <- parens $ parserOutput
  return $ o

--
parserOutput :: Parser Output
parserOutput = choice [
  reserved "" >> return O0,
  reserved "O0" >> return O0,
  reserved "O1" >> return O1,
  reserved "O2" >> return O2,
  reserved "O3" >> return O3
  ]


---------
int :: Parser Int
int = fromIntegral <$> integer

double :: Parser Double
double = choice [
  symbol "-" >> double >>= return . (* (-1)),
  try float,
  try $ fromIntegral <$> integer
  ]

---------

tokenParser :: P.GenTokenParser Text () Identity
tokenParser = P.makeTokenParser $ P.LanguageDef {
  P.commentStart = "{-",
  P.commentEnd = "-}",
  P.commentLine = "--",
  P.nestedComments = False,
  P.identStart = letter <|> char '_',
  P.identLetter = alphaNum <|> char '_',
  P.opStart = oneOf "+*.",
  P.opLetter = oneOf "+*.",
  P.reservedNames = [
    "out"
    ],
  P.reservedOpNames = [],
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
