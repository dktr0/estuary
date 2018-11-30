module Estuary.Languages.LaCalle (laCalle) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
-- import Estuary.Tidal.ParamPatternable (parseBP')
import Text.Parsec.Language (haskellDef)
import           Control.Monad (forever)



import Sound.Tidal.MiniTidal (miniTidal,miniTidalIO,main)
import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Parseable,Time,Arc,TPat,StreamparseBP')
import qualified Sound.Tidal.Context as Tidal

--lima
-- <nombre sonido> <transf1> <parametros>

laCalle :: String -> Either ParseError ControlPattern
laCalle = parse lengExpr "LaCalle"

exprStack :: Parser ControlPattern
exprStack = do
   expr <- many lengExpr
   return $ Tidal.stack expr

lengExpr :: Parser ControlPattern
lengExpr = whiteSpace >> choice [
  eof >> return Tidal.silence,
  do
    s <- sonidos
    eof --m ()
    return $ nuestroTextoATidal s
    ]

nuestroTextoATidal ::  String  -> ControlPattern
nuestroTextoATidal s = Tidal.s $ parseBP' s

sonidos :: Parser String
sonidos = (reserved "hola" >> return "sitar" )

trans :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
trans = choice [
              --coloca aqui los nombres de tus transformaciones
         try (string "tu manyas" >> spaces >> fractional3 False  >>= return . Tidal.slow),
         try (string "bien helenas" >> spaces >> fractional3 False  >>= return . Tidal.fast),
         try (string "palta con el">> spaces >> int >>= return . Tidal.iter),
         try (string "mi cerro" >> spaces >> int >>= return . Tidal.chop),
         try (descartarTexto >> return id)
                ]

--descartar espacios
espacios :: Parser String
espacios = many (oneOf " ")

--descartar texto
descartarTexto :: Parser String
descartarTexto = many (oneOf "\n")


tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["hola"],
  P.reservedOpNames = []
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
