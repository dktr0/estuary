module Estuary.Languages.TiempoEspacio.Ver (ver) where

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


ver :: Text -> Either ParseError Program
ver = parse empty ""

{-
verParser :: Parser [Expression]
verParser = do
  whiteSpace
  x <- sentence `sepBy` reservedOp ","
  eof
  return x

sentence :: Parser Expression
sentence = choice [
  try $ silence,
  try $ sound,
  try $ soundOscilate,
  try $ soundAddSound,
  try $ soundAddSoundOscilate,
  try $ filterverb,
  try $ filterverbOscilate,
  try $ filterverbAndverb,
  try $ filterverbAndverbOscilate
  ]

-- // Opciones de gramatica

silence :: Parser Expression
silence = do
  f <- option DefaultCrossFade fade
  o <- out
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f EmptyGraph) o

sound :: Parser Expression
sound = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  v <- verb
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = Product v (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

soundOscilate :: Parser Expression
soundOscilate = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  v <- verb
  option () miscelanea
  option () miscelanea
  n <- noun
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = Product (Product v n) (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

soundAddSound :: Parser Expression
soundAddSound = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  v1 <- verb
  reserved "and"
  v2 <- verb
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = Product (Sum v1 v2) (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

soundAddSoundOscilate :: Parser Expression
soundAddSoundOscilate = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  v1 <- verb
  reserved "and"
  v2 <- verb
  option () miscelanea
  option () miscelanea
  n <- noun
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = Product (Product (Sum v1 v2) n) (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

filterverb :: Parser Expression
filterverb = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  fv <- firstVerb
  option () miscelanea
  v <- verb
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = fv v
  let h = Product g (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0)) f h) o

filterverbOscilate :: Parser Expression
filterverbOscilate = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  fv <- firstVerb
  option () miscelanea
  option () miscelanea
  v <- verb
  option () miscelanea
  option () miscelanea
  n <- noun
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = fv (Product v n)
  let h = Product g (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0)) f h) o

filterverbAndverb :: Parser Expression
filterverbAndverb = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  fv <- firstVerb
  option () miscelanea
  option () miscelanea
  v1 <- verb
  reserved "and"
  v2 <- verb
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let a = fv (Sum v1 v2)
  let b = Product a (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0)) f b) o

filterverbAndverbOscilate :: Parser Expression
filterverbAndverbOscilate = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  fv <- firstVerb
  option () miscelanea
  option () miscelanea
  v1 <- verb
  reserved "and"
  v2 <- verb
  option () miscelanea
  option () miscelanea
  n <- noun
  option () miscelanea
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let a = fv (Product (Sum v1 v2) n)
  let b = Product a (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0)) f b) o


-- // miscelánea

miscelanea :: Parser ()
miscelanea = choice [
  reserved "I" >> return (),
  reserved "it" >> return (),
  reserved "They" >> return (),
  reserved "they" >> return (),
  reserved "not" >> return (),
  reserved "no" >> return (),
  reserved "to" >> return (),
  reserved "the" >> return (),
  reserved "The" >> return (),
  reserved "tired" >> return (),
  reserved "Tired" >> return (),
  reserved "exhausted" >> return (),
  reserved "Place" >> return (),
  reserved "Places" >> return (),
  reserved "Sound" >> return (),
  reserved "Sounds" >> return (),
  reserved "City" >> return (),
  reserved "Cities" >> return (),
  reserved "There" >> return (),
  reserved "there" >> return (),
  reserved "another" >> return (),
  reserved "everything" >> return (),
  reserved "and" >> return (),
  reserved "or" >> return (),
  reserved "been" >> return (),
  reserved "those" >> return (),
  reserved "that" >> return (),
  reserved "of" >> return (),
  reserved "nothing" >> return (),
  reserved "Nothing" >> return ()
  ]

-- // Fade in/out

fade :: Parser Transition
fade = choice [
  reserved "cinco_" >> return (CrossFade (Seconds 5)),
  reserved "diez_" >> return (CrossFade (Seconds 10)),
  reserved "quince_" >> return (CrossFade (Seconds 15)),
  reserved "veinte_" >> return (CrossFade (Seconds 20))
  ]

-- // filters

firstVerb :: Parser (Graph -> Graph)
firstVerb = choice [
  (reserved "am" <|> reserved "are" <|> reserved "is") >> return (\x -> HPF x (modulatedRangeGraph (Constant 80) (Constant 120) (Sine (Constant 1))) (Constant 1)),
  (reserved "have" <|> reserved "has") >> return (\x -> HPF x (modulatedRangeGraph (Constant 150) (Constant 160) (Sine (Constant 0.5))) (Constant 1)),
  reserved "can" >> return (\x -> HPF x (modulatedRangeGraph (Constant 200) (Constant 250) (Sine (Constant 1))) (Constant 1)),
  (reserved "want" <|> reserved "wants") >> return (\x -> HPF x (modulatedRangeGraph (Constant 100) (Constant 120) (Sine (Constant 0.5))) (Constant 1)),
  (reserved "like" <|> reserved "likes") >> return (\x -> HPF x (modulatedRangeGraph (Constant 180) (Constant 200) (Sine (Constant 1))) (Constant 1)),
  (reserved "hate" <|> reserved "hates") >> return (\x -> HPF x (modulatedRangeGraph (Constant 250) (Constant 300) (Sine (Constant 0.5))) (Constant 1)),

  (reserved "was" <|> reserved "were") >> return (\x -> LPF x (modulatedRangeGraph (Constant 100) (Constant 120) (Sine (Constant 1))) (Constant 1)),
  reserved "had" >> return (\x -> LPF x (modulatedRangeGraph (Constant 150) (Constant 160) (Sine (Constant 0.5))) (Constant 1)),
  reserved "could" >> return (\x -> LPF x (modulatedRangeGraph (Constant 200) (Constant 250) (Sine (Constant 1))) (Constant 1)),
  reserved "wanted" >> return (\x -> LPF x (modulatedRangeGraph (Constant 180) (Constant 200) (Sine (Constant 0.5))) (Constant 1)),
  reserved "liked" >> return (\x -> LPF x (modulatedRangeGraph (Constant 120) (Constant 150) (Sine (Constant 1))) (Constant 1)),
  reserved "hated" >> return (\x -> LPF x (modulatedRangeGraph (Constant 250) (Constant 300) (Sine (Constant 0.5))) (Constant 1))
  ]


-- // verbs


verb :: Parser Graph
verb = choice [
  (reserved "travel" <|> reserved "travels") >> return (Sine (Multi [Constant 261.626,Constant 262.079])),
  reserved "traveled" >> return (Sine (Multi [Constant 241.626,Constant 242.079])),
  reserved "travelling" >> return (Sine (Multi [Constant 231.626,Constant 232.079])),
  (reserved "go" <|> reserved "goes") >> return (Sine (Multi [Constant 227.652,Constant 208.012, Constant 228.072])),
  reserved "went" >> return (Sine (Multi [Constant 217.652,Constant 198.012, Constant 218.072])),
  reserved "going" >> return (Sine (Multi [Constant 218.652,Constant 298.012, Constant 218.072])),
  reserved "gone" >> return (Sine (Multi [Constant 207.652,Constant 188.012, Constant 218.072])),
  (reserved "move" <|> reserved "moves") >> return (Sine (Multi [Constant 349.228,Constant 349.833, Constant 466.163])),
  reserved "moved" >> return (Sine (Multi [Constant 339.228,Constant 339.833, Constant 366.163])),
  reserved "moving" >> return (Sine (Multi [Constant 329.228,Constant 329.833, Constant 400.163])),
  (reserved "sit" <|> reserved "sits") >> return (Sine (Multi [Constant 110,Constant 110.190, Constant 195.997])),
  reserved "sat" >> return (Sine (Multi [Constant 100,Constant 100.190, Constant 145.997])),
  reserved "sitting" >> return (Sine (Multi [Constant 100,Constant 100.190, Constant 125.997])),

  (reserved "happen" <|> reserved "happens") >> return (Saw (Multi [Constant 70,Constant 70.190, Constant 80.997])),
  reserved "happened" >> return (Saw (Multi [Constant 60,Constant 60.190, Constant 65.997])),
  reserved "happening" >> return (Saw (Multi [Constant 65,Constant 65.190, Constant 70.997])),
  (reserved "quit" <|> reserved "quits") >> return (Saw (Multi [Constant 161.626,Constant 162.079])),
  reserved "quitting" >> return (Saw (Multi [Constant 121.626,Constant 122.079])),

  (reserved "leave" <|> reserved "leaves") >> return (Tri (Multi [Constant 107.652,Constant 188.012, Constant 108.072])),
  reserved "left" >> return (Tri (Multi [Constant 117.652,Constant 168.012, Constant 118.072])),
  reserved "leaving" >> return (Tri (Multi [Constant 100.652,Constant 110.012, Constant 100.072])),
  (reserved "scape" <|> reserved "scapes") >> return (Tri (Multi [Constant 100.626,Constant 110.079])),
  reserved "scaped" >> return (Tri (Multi [Constant 90.626,Constant 102.079])),
  reserved "scaping" >> return (Tri (Multi [Constant 120.626,Constant 180.079]))
  ]
-- sin [60, 60.03]
-- sin [56, 56.03, 56.035]
-- sin [65, 65.03, 70]
-- sin [45, 45.03, 55]

-- // oscillators

noun :: Parser Graph
noun = choice [
  (reserved "place" <|> reserved "places") >> return (Sine (Constant 0.2)),
  (reserved "sound" <|> reserved "sounds") >> return (Sine (Constant 0.5)),
  (reserved "noise" <|> reserved "noises") >> return (Sine (Constant 1.0)),
  (reserved "city" <|> reserved "cities") >> return (Tri (Constant 0.2)),
  (reserved "light" <|> reserved "lights") >> return (Tri (Constant 0.5)),
  (reserved "image" <|> reserved "images") >> return (Tri (Constant 0.8)),
  (reserved "country" <|> reserved "countries") >> return (Saw (Constant 0.2)),
  (reserved "texture" <|> reserved "textures") >> return (Saw (Constant 0.5)),
  (reserved "feeling" <|> reserved "feelings") >> return (Saw (Constant 0.8))
  ]


-- // Salidas

level :: Parser Extent
level = do
  x <- double
  return $ dbamp x

out :: Parser Output
out = choice [
  try $ reservedOp "<>" >> return (PannedOutput 0.5),
  try $ reservedOp ">" >> return (PannedOutput 1.0),
  try $ reservedOp "<" >> return (PannedOutput 0.0)
  ]

-- //


average :: Graph -> Graph -> Graph
average = mean

difference :: Graph -> Graph -> Graph
difference x y = x - y

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
-}
