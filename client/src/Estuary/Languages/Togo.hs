module Estuary.Languages.Togo (togo) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (ControlPattern)
import qualified Sound.Tidal.Context as Tidal

import Estuary.Tidal.ParamPatternable

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
  c <- togoPatternAsArg
  d <- togoPatternAsArg
  let n1 = pure $ fromIntegral a
  let n2 = pure $ fromIntegral b
  let onPattern = Tidal.fast (pure $ fromIntegral b) c
  let offPattern = Tidal.fast (pure $ fromIntegral b) d
  return $ Tidal.euclidFull n1 n2 onPattern offPattern

euclidPattern :: Parser ControlPattern
euclidPattern = do
  a <- natural
  reservedOp "x"
  b <- natural
  c <- togoPatternAsArg
  let n1 = pure $ fromIntegral a
  let n2 = pure $ fromIntegral b
  let onPattern = Tidal.fast (pure $ fromIntegral b) c
  return $ Tidal.euclid n1 n2 onPattern

euclidInvPattern :: Parser ControlPattern
euclidInvPattern = do
  a <- natural
  reservedOp "_"
  b <- natural
  c <- togoPatternAsArg
  let n1 = pure $ fromIntegral a
  let n2 = pure $ fromIntegral b
  let offPattern = Tidal.fast (pure $ fromIntegral b) c
  return $ Tidal.euclidInv n1 n2 offPattern

samplePattern :: Parser ControlPattern
samplePattern = do
  s <- stringLiteral
  return $ Tidal.s $ parseBP' s

silence :: Parser ControlPattern
silence = choice [ reserved "silence", reserved "~" ] >> return Tidal.silence

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["silence","~"],
  P.reservedOpNames = ["x"]
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
