module Estuary.Languages.LaCalle (laCalle) where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim (many)
-- import qualified Text.Parsec.Combinator ()
import Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P
import Sound.Tidal.Context (Pattern,ControlPattern)
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.ParamPatternable (parseBP')
import Data.List

--laCalle
-- <nombre sonido> <transf1> <parametros>
laCalle :: String -> Either ParseError Tidal.ControlPattern
laCalle s = parse laCalleParser "LaCalle" s

laCalleParser :: Parser Tidal.ControlPattern
laCalleParser = whiteSpace >> choice [
    eof >> return Tidal.silence,
    do
      expr <- sepEmpty laCallePattern
      eof
      return $  Tidal.stack expr
      ]

sepEmpty p = sepBy p (symbol "_")

-- endOfDoc :: m Char
endOfDoc = choice [comma, dot, try (symbol "?")]
joinSentence = choice [comma, try (symbol "y"), try (symbol "pá")]


laCallePattern :: Parser Tidal.ControlPattern
laCallePattern = choice [
                         -- try simpleNoun,
                         -- try simpleVerb,
                         try simpleSentenceWGreeting
                         ,try simpleSentenceWPronoun
                         -- ,try simpleSentenceWQuantity
                         -- ,try simpleQuestion
                         ,simpleAndCompundQuestion
                         ,try simpleSentenceStartingWNoun
                        ]

simpleSentenceWGreeting :: Parser Tidal.ControlPattern
simpleSentenceWGreeting = do
  c <- greeting
  n <- option [" "] (many noun)
  v <- option id verb
  e <- option "," endOfDoc
  return $ v $ Tidal.s $ parseBP' (unwords n)

simpleSentenceWPronoun :: Parser Tidal.ControlPattern
simpleSentenceWPronoun = do
  c <- pronoun
  n <- option [" "] (many noun)
  v <- option id verb
  e <- option "," endOfDoc
  return $ v $ Tidal.s $ parseBP' (unwords n)

-- simpleSentenceWQuantity:: Parser Tidal.ControlPattern
-- simpleSentenceWQuantity = do
--   c <- pronoun
--   n <- option [" "] (many noun)
--   v <- option id taypa
--   e <- option "," endOfDoc
--   return $ v $ Tidal.s $ parseBP' (unwords n)

simpleSentenceStartingWNoun :: Parser Tidal.ControlPattern
simpleSentenceStartingWNoun = do
  n <- many noun
  v <- option id verb
  e <- option "," endOfDoc
  return $ v $ Tidal.s $ parseBP' (unwords n)


simpleNoun :: Parser Tidal.ControlPattern
simpleNoun = do
  n <- many noun
  e <- option "," endOfDoc
  return $ Tidal.s $ parseBP' (unwords n)

simpleVerb :: Parser Tidal.ControlPattern
simpleVerb = do
  n <- verb
  e <- option "," endOfDoc
  return $ Tidal.silence


simpleQuestion:: Parser Tidal.ControlPattern
simpleQuestion = do
  i <- symbol "¿"
  -- n <- option [" "] (many noun)
  c <- option " " question
  v <- option id verb
  n <- option [" "] (many noun)
  e <- option "?" endOfDoc
  return $ v $ Tidal.s $ parseBP' (unwords n)


simpleAndCompundQuestion:: Parser Tidal.ControlPattern
simpleAndCompundQuestion = do
  i <- symbol "¿"
  -- n <- option [" "] (many noun)
  c <- option [" "] (many connector)
  v <- option id verb
  n <- option [" "] (many noun)
  j <- option " " joinSentence
  c' <- option [" "] (many connector)
  v' <- option id verb
  n' <- option [" "] (many noun)
  v'' <- option id verb
  e <- option "?" endOfDoc
  return $ v'' $ v' $ v $ Tidal.s $ parseBP' $ (unwords n) ++ " " ++  (unwords n')


verb :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
verb = choice [verb', verb''', try taypa, try helenas]

connector :: Parser String
connector = choice [try question, try connector', try pronoun]

question :: Parser String
question = choice [
         (reserved "qué" >> return ""),
         (reserved "Qué" >> return "")
       ]

possesive :: Parser String
possesive = choice [
        (reserved "mi" >> return ""),
        (reserved "Mi" >> return "")
        ]

connector' :: Parser String
connector' = choice [
        (reserved "asu" >> return ""),
        (reserved "mare" >> return ""),
        (reserved "en" >> return "")
        ]

greeting :: Parser String
greeting = choice [
      (reserved "habla" >> return ""),
      (reserved "Habla" >> return "")

      ]

pronoun :: Parser String
pronoun = choice [
        (reserved "el" >> return ""),
        (reserved "el" >> return ""),
        (reserved "El" >> return ""),
        (reserved "un" >> return ""),
        (reserved "Un" >> return ""),
        (reserved "la" >> return ""),
        (reserved "La" >> return ""),
        (reserved "Una" >> return ""),
        (reserved "una" >> return ""),
        (reserved "unas" >> return ""),
        (reserved "Unas" >> return "")
      ]

noun :: Parser String
noun = choice [
        (reserved "Causa" >> return "kurt"),
        (reserved "causa" >> return "kurt"),
        (reserved "negocio" >> return "kurt:5"),
        (reserved "Negocio" >> return "kurt:5"),
        (reserved "Señito" >> return "kurt:2"),
        (reserved "señito" >> return "kurt:2"),
        (reserved "Cevillano" >> return "kurt:2"),
        (reserved "cevillano" >> return "kurt:2"),
        (reserved "Batería" >> return "kurt:3"),
        (reserved "batería" >> return "kurt:3"),
        (reserved "Chancha" >> return "arpy"),
        (reserved "chancha" >> return "arpy"),
        (reserved "chelas" >> return "arpy"),
        (reserved "Cholo" >> return "arpy"),
        (reserved "cholo" >> return "arpy"),
        (reserved "viejita" >> return "arpy"),
        (reserved "Viejita" >> return "arpy"),
        (reserved "jato" >> return "arpy"),
        (reserved "Tombo" >> return "arpy"),
        (reserved "tombo" >> return "arpy"),
        (reserved "zampado" >> return "arpy"),
        (reserved "Tío" >> return "arpy"),
        (reserved "tío" >> return "arpy"),
        (reserved "Tía" >> return "arpy"),
        (reserved "tía" >> return "arpy"),
        (reserved "Brother" >> return "arpy"),
        (reserved "~" >> return "~")

        ]

verb' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
verb' = choice [
         (reserved "manyas" >> option (Tidal.slow 1) (double >>= return . Tidal.slow . pure . toRational)),
         (reserved "mare" >> option (Tidal.iter 0) (int >>= return . Tidal.iter . pure))

         ]

verb'' :: Parser Tidal.ControlPattern
verb'' = choice [
         (reserved "corto" >> return Tidal.end) <*> option 1 doublePattern,
         (reserved "salir" >> return Tidal.begin) <*> option 0 doublePattern,
         (reserved "acompáñame" >> return Tidal.up) <*> option 0 doublePattern,
         (reserved "alucina" >> return Tidal.loop) <*> option 0 doublePattern,
         (reserved "gané" >> return Tidal.pan) <*> option 0 (return Tidal.rand),
         (reserved "quito" >> return Tidal.pan) <*> option 0 (return $Tidal.rand*10),
         (reserved "saca" >> return Tidal.pan) <*> option 0 doublePattern,
         (reserved "fué" >> return Tidal.delaytime) <*> option 0 doublePattern,
         (reserved "sabes" >> return Tidal.hresonance) <*> option 0 doublePattern,
         (reserved "estas" >> return Tidal.accelerate) <*> option 0 doublePattern,
         (reserved "está" >> return Tidal.delay) <*> option 0 doublePattern,
        (reserved "sírvame" >> return Tidal.crush) <*> option 0 doublePattern
        ]

verb''' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
verb''' = do
  x <- verb''
  return (Tidal.# x)


taypa :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
taypa = do
  t <- option (Tidal.fast 1) (double >>= return . Tidal.fast . pure . toRational)
  s <- string "taypá"
  return t

helenas :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
helenas = do
  t <- option (Tidal.chop 0) (int >>= return . Tidal.chop . pure)
  s <- string "helenas"
  return t

--descartar texto
descartarTexto :: GenParser Char a String
descartarTexto = many (oneOf "\n")

-- randPatternTimesTen :: Parser (Pattern Double)
-- randPatternTimesTen = return (Tidal.rand*10)
--
--
-- randPattern :: Parser (Pattern Double)
-- randPattern = reserved "rand" >> return (Tidal.rand*10)

doublePattern :: Parser (Pattern Double)
doublePattern = choice [
    pure <$> double,
    reserved "rand" >> return (Tidal.rand*10),
    reserved "rand'" >> return Tidal.rand,
    reserved "saw" >> return Tidal.saw,
    reserved "isaw" >> return Tidal.isaw,
    reserved "tri" >> return Tidal.tri,
    reserved "square" >> return Tidal.square,
    reserved "cosine" >> return Tidal.cosine
    ]

-- pan' :: Parser (Tidal.ControlPattern -> Tidal.ControlPattern)
-- pan' = do
--   s <- string "saca'"
--   t <- return Tidal.pan <*> option 0 (many doublePattern)
--   return t

double = choice [
   float,
   fromInteger <$> integer, --TO DO: his doesnt work with ints like 1 only if they have decimals 1.0
   adjectiveFloat,
   fromInteger <$> adjectiveInteger
   ]

int :: Parser Int
int = choice [
   fromIntegral <$> integer,
   fromIntegral <$> adjectiveInteger
   ]

adjectiveFloat :: Parser Double
adjectiveFloat = choice [
       (reserved "bien" >> return 0.9),
       (reserved "misio" >> return 0.8),
       (reserved "heladas" >> return 0.7),
       (reserved "más" >> return 0.6),
       (reserved "frikeado" >> return 0.5),
       (reserved "full" >> return 0.4),
       (reserved "zampado" >> return 0.3),
       (reserved "chill" >> return 0.2),
       (reserved "cerrado" >> return 0.1)
       ]

adjectiveInteger :: Parser Integer
adjectiveInteger = choice [
       (reserved "aguja" >> return 9),
       (reserved "Bien" >> return 8),
       (reserved "Misio" >> return 7),
       (reserved "Heladas" >> return 6),
       (reserved "Más" >> return 5),
       (reserved "Frikeado" >> return 4),
       (reserved "Full" >> return 6),
       (reserved "Zampado" >> return 3),
       (reserved "Chill" >> return 2),
       (reserved "Cerrado" >> return 1)
       ]

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = [ "el", "un", "la", "unas"]
  -- P.reservedOpNames = ["X","x","_","/",">>","<<"]
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
