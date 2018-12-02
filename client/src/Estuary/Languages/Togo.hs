module Estuary.Languages.Togo (togo) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
-- import Estuary.Tidal.ParamPatternable (parseBP')
import Text.ParserCombinators.Parsec.Number

import Text.Parsec.Language (haskellDef)
import           Control.Monad (forever)



import Sound.Tidal.MiniTidal (miniTidal,miniTidalIO,main)
import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Parseable,Time,Arc,TPat)
import qualified Sound.Tidal.Context as Tidal

--lima
-- <nombre sonido> <transf1> <parametros>

togo :: String -> Either ParseError ControlPattern
togo = parse lengExpr "togo"

lengExpr :: Parser ControlPattern
lengExpr = whiteSpace >> choice [
  eof >> return Tidal.silence,
  do
    p <- pattern
    eof --m ()
    return $ p
    ]

pattern :: Parser ControlPattern
pattern = do
  char '\"'
  x <- pattern'
  char '\"'
  return x

pattern' :: Parser ControlPattern
pattern' =  choice [
   pattern''
      ]

pattern'' :: Parser ControlPattern
pattern'' = do
     whiteSpace
     x <- parseBP'
     y <- nPattern
     whiteSpace
     return $ Tidal.s x

nPattern :: Parser ControlPattern
nPattern = do
     reserved "n" >> whiteSpace
     x' <- parseBP'
     whiteSpace
     return $ Tidal.n x'

parseBP' :: (Enumerable a, Parseable a) => Parser (Pattern a)
parseBP' = parseTPat' >>= return . Tidal.toPat

parseBP'' :: (Enumerable a, Parseable a) => Parser (Pattern a)
parseBP'' = parseTPat'' >>= return . Tidal.toPat

parseTPat'' :: Parseable a => Parser (TPat a)
parseTPat'' = Tidal.tPatParser

parseTPat' :: Parseable a => Parser (TPat a)
parseTPat' = Tidal.pSequence Tidal.tPatParser
  --parseRhythm' Tidal.tPatParser

-- parseRhythm' :: Parseable a => Parser (TPat a) -> Parser (TPat a)
-- parseRhythm' f = do
--   x <-  f' --Tidal.pSequence f'
--   return x
--   where f' = f
--              <|> do _ <- symbol "~" <?> "rest"
--                     return Tidal.TPat_Silence
--
-- parseRhythmWPattern :: Parseable a => Parser (TPat a) -> Parser (TPat a)
-- parseRhythmWPattern f = do
--   char '\"' >> whiteSpace
--   f' <- f
--   char '\"' >> whiteSpace
--   return f'

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = [],
  P.reservedOpNames = ["s", "n"]
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

main :: IO ()
main = do
  putStrLn "laCalle"
  tidal <- Tidal.startTidal Tidal.superdirtTarget Tidal.defaultConfig
  forever $ do
    cmd <- miniTidalIO tidal <$> getLine
    either (\x -> putStrLn $ "error: " ++ show x) id cmd
