module Estuary.Languages.BlackBox (blackBox) where

import           Control.Monad (forever)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim (many)
-- import qualified Text.Parsec.Combinator ()
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal
import Data.List

--blackChair
--whiteChair
--floor
--stool

--blackChair!arm N
--blackChair!floor

--blackChair>arm.tap
--blackChair>arm.slide W 0.75
--blackChair.stack.slide
--blackChair.stack.strum
--blackChair<>floor.contact
--blackChair.sit

--stool.dragging Room
--stool.tilt

--whiteChair.stacking
--whiteChair.unstacking

--N = pan 0
--E
--w
--S
--Room



blackBox :: String -> Either ParseError Tidal.ControlPattern
blackBox s = parse existenceParser "Existence" s

existenceParser :: Parser Tidal.ControlPattern
existenceParser = choice [
    eof >> return Tidal.silence,
    do
      expr <- sepEmpty existencePattern
      eof
      return $  Tidal.stack expr
      ]

sepEmpty p = sepBy p sep
sep = choice [try (symbol "|")]

joinExistence = choice [try (symbol "<>"), try (symbol "."), try (symbol ">")]

existencePattern :: Parser Tidal.ControlPattern
existencePattern = existenceOrNonExistence


existenceOrNonExistence :: Parser Tidal.ControlPattern
existenceOrNonExistence = do
  o <- many objectOrDescription
  d <- option id distance
  return $ d $ Tidal.s $ parseBP' $ (unwords o)

objectOrDescription :: Parser String
objectOrDescription = choice [
                        try (description),
                        try (description'),
                        try (object)
                        ]

object :: Parser String
object = choice [
        (reserved "blackChair" >> return "blackChair"),
        (reserved "arm" >> return "blackChair:1"),
        (reserved "whiteChair" >> return "whiteChair"),
        (reserved "stool" >> return "stool"),
        (reserved "floor" >> return "blackChair:2")
        ]


description :: Parser String
description = do
        o <- object
        joinExistence
        o2 <- object
        joinExistence
        d <- descriptor
        return d

descriptor :: Parser String
descriptor = choice[
              (reserved "slide" >> return "blackChair:0"),
              (reserved "arm.tap" >> return "blackChair:1"), --necesita joinExistence
              (reserved "contact" >> return "blackChair:2"),
              (reserved "sit" >> return "blackChair:4"),
              (reserved "strum.five" >> return "blackChair:5"),
              (reserved "strum.four" >> return "blackChair:7"),
              (reserved "tap" >> return "blackChair:8"),
              (reserved "stacking" >> return "whiteChair:0"),
              (reserved "unstacking" >> return "whiteChair:1"),
              (reserved "tilt" >> return "stool:1"),
              (reserved "dragging" >> return "stool:0")
              ]

description' :: Parser String
description' = do
        o <- object
        symbol "."
        d <- descriptor
        return d

distance' :: Parser Tidal.ControlPattern
distance' = choice [
            (reserved "N" >> return Tidal.pan) <*> option 0 doublePattern,
            (reserved "E" >> return Tidal.pan) <*> option 0.375 doublePattern,
            (reserved "S" >> return Tidal.pan) <*> option 0.5 doublePattern,
            (reserved "W" >> return Tidal.pan) <*> option 0.75 doublePattern,
            -- (reserved "N>NE" >> return Tidal.pan) <*> option (Tidal.fromList [0,1]) doublePattern,
            (reserved "room" >> return Tidal.pan) <*> option 0 (return Tidal.sine),
           --(reserved "place" >> return Tidal.pan) <*> option 0 (return Tidal.?),
 (reserved "Room" >> return Tidal.pan) <*> option 0 (return Tidal.rand)
                 ]


distance :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
distance = do
  x <- distance'
  return (Tidal.# x)


doublePattern :: Parser (Pattern Double)
doublePattern = choice [
    pure <$> double,
    -- reserved "N" >> return (Tidal.rand*10),
    reserved "rand'" >> return Tidal.rand,
    reserved "saw" >> return Tidal.saw,
    reserved "isaw" >> return Tidal.isaw,
    reserved "tri" >> return Tidal.tri,
    reserved "square" >> return Tidal.square,
    reserved "cosine" >> return Tidal.cosine,
    reserved "sine" >> return Tidal.sine
    ]

double = choice [
   float,
   fromInteger <$> integer --TO DO: this doesnt work with ints like 1 only if they have decimals 1.0
   ]

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
   P.reservedNames = [ ]
   }

parseBP' :: (Tidal.Enumerable a, Tidal.Parseable a) => String -> Tidal.Pattern a
parseBP' = (either (const Tidal.silence)  id). Tidal.parseBP

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
