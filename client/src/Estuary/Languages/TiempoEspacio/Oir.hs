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
  try $ silence,
  try $ graphic,
  try $ graphicPlusConstrain,
  try $ polarPlusGraphic
  ]

-- // Opciones de gramática .

silence :: Parser Expression
silence = do
  f <- option DefaultCrossFade fade
  o <- out
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f EmptyGraph) o

graphic :: Parser Expression
graphic = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  v <- verb
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = Product v (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

graphicPlusConstrain :: Parser Expression
graphicPlusConstrain = do
  f <- option DefaultCrossFade fade
  option () miscelanea
  option () miscelanea
  v <- verb
  option () miscelanea
  option () miscelanea
  option () miscelanea
  n <- noun
  option () miscelanea
  option () miscelanea
  option () miscelanea
  l <- parens $ level
  o <- out
  let g = Product (Product v n) (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

polarPlusGraphic :: Parser Expression
polarPlusGraphic = do
  f <- option DefaultCrossFade fade
  a <- auxiliar
  v <- verb
  l <- parens $ level
  o <- out
  let g = Product (a v) (Constant l)
  return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f g) o

-- transformationPlusGraphicTransformation :: Parser Expression
-- transformationPlusGraphicTransformation = do
--   f <- option DefaultCrossFade fade
--   option () miscelanea
--   option () miscelanea
--   av <- auxiliarPlusVerb
--   option () miscelanea
--   option () miscelanea
--   option () miscelanea
--   n <- noun
--   option () miscelanea
--   option () miscelanea
--   option () miscelanea
--   l <- parens $ level
--   o <- out
--   let g = Product (Product av n) (Constant l)
--   return $ Expression (Definition Anonymous (Quant 1 (Seconds 0.0)) f h) o

-- [Expression {definition = Definition {target = Anonymous, defTime = Quant 1.0 (Seconds 0.0), transition = DefaultCrossFade, graph = Product (Sine (Constant 0.5)) (Constant 10.0)}, output = NamedOutput "rgb"}]

-- // miscelánea

miscelanea :: Parser ()
miscelanea = choice [
  reserved "I" >> return (),
  reserved "They" >> return (),
  reserved "a" >> return (),
  reserved "an" >> return (),
  reserved "on" >> return (),
  reserved "in" >> return (),
  reserved "my" >> return (),
  reserved "their" >> return (),
  reserved "her" >> return (),
  reserved "golden" >> return (),
  reserved "the" >> return (),
  reserved "whale" >> return (),
  reserved "whales" >> return (),
  reserved "over" >> return (),
  reserved "off" >> return (),
  reserved "with" >> return (),
  reserved "big" >> return (),
  reserved "Big" >> return (),
  reserved "blue" >> return (),
  reserved "next" >> return (),
  reserved "to" >> return (),
  reserved "me" >> return (),
  reserved "them" >> return (),
  reserved "under" >> return (),
  reserved "Whale" >> return (),
  reserved "Whales" >> return (),
  reserved "Lights" >> return (),
  reserved "Tale" >> return (),
  reserved "bed" >> return (),
  reserved "Dreams" >> return ()
  ]


-- // Fade in/out

fade :: Parser Transition
fade = choice [
  reserved "cinco_" >> return (CrossFade (Seconds 5)),
  reserved "diez_" >> return (CrossFade (Seconds 10)),
  reserved "quince_" >> return (CrossFade (Seconds 15)),
  reserved "veinte_" >> return (CrossFade (Seconds 20))
  ]

-- // bipolar / unipolar

auxiliar :: Parser (Graph -> Graph)
auxiliar = choice [
  (reserved "do" <|> reserved "dont" <|> reserved "does" <|> reserved "doesnt" reserved "am" <|> reserved "is" <|> reserved "are" <|> reserved "isnt" <|> reserved "arent") >> return unipolar,
  (reserved "did" <|> reserved "didnt" <|> reserved "was" <|> reserved "wasnt" <|> reserved "were" <|> reserved "werent") >> return bipolar
  ]

-- // sound waves y fx / fy

verb :: Parser Graph
verb = choice [ --saw (fx*fy* [10, 10.05, 11])
  reserved "dream" >> return Fx,
  reserved "dreams" >> return Fy,
  reserved "dreaming" >> return (Product Fx Fy),
  (reserved "crush" <|> reserved "crushes") >> return (Sine (Product Fx (Multi [Constant 10,Constant 10.05,Constant 11]))),
  reserved "crushing" >> return (Sine (Product Fy (Multi [Constant 10,Constant 10.05,Constant 11]))),
  (reserved "scream" <|> reserved "screams") >> return (Product Fx (Product Fx (Sine (Multi [Constant 0.1, Constant 0.2])))),
  reserved "screaming" >> return (Product Fy (Product Fy (Sine (Multi [Constant 0.1, Constant 0.2])))),
  (reserved "open" <|> reserved "opens") >> return (Sine (Constant 0.1)),
  reserved "opening" >> return (Sine (Constant 0.2)),
  (reserved "swallow" <|> reserved "swallows") >> return (Saw (Product Fx (Multi [Constant 1.0, Constant 2.0, Constant 3.0]))),
  reserved "swallowing" >> return (Sine (Product Fy (Multi [Constant 1.0, Constant 2.0, Constant 3.0]))),
  (reserved "run" <|> reserved "runs") >> return (Saw (Product Fx (Product Fy (Multi [Constant 0.1,Constant 0.2,Constant 0.3])))),
  reserved "running" >> return (Saw (Product Fy (Product Fx (Multi [Constant 5,Constant 6,Constant 7])))),
  (reserved "vanish" <|> reserved "vanishes") >> return (Sine (Product Fy (Constant 0.2))),
  reserved "vanishing" >> return (Sine (Product Fy (Multi [Constant 0.1, Constant 0.2, Constant 0.3]))),
  (reserved "disappear" <|> reserved "disappears") >> return (Saw (Product Fx (Product Fy (Multi [Constant 10,Constant 10.05,Constant 11])))),
  reserved "disappearing" >> return (Sine (Product Fx (Product Fy (Multi [Constant 20,Constant 12.05,Constant 21])))),
  (reserved "play" <|> reserved "plays") >> return (Sine Fx),
  reserved "playing" >> return (Sine Fy),
  (reserved "have" <|> reserved "has") >> return (Sine (Product Fy Fy)),
  (reserved "shine" <|> reserved "shines") >> return (Sine (Product Fx (Multi [Constant 2, Constant 3, Constant 4]))),
  reserved "shining" >> return (Saw (Product Fy (Multi [Constant 2, Constant 3, Constant 4]))),
  (reserved "miss" <|> reserved "misses") >> return (Sine (Product Fx (Multi [Constant 0.01, Constant 0.02, Constant 0.03, Constant 0.04, Constant 0.05]))),
  reserved "missing" >> return (Sine (Product Fy (Multi [Constant 0.01, Constant 0.02, Constant 0.03, Constant 0.04, Constant 0.05])))
  ]

-- // constrains rect / circle

noun :: Parser Graph
noun = choice [ --(sin (fx *[0.01, 0.02])) (sin (fx* 0.1)) 0.5
  (reserved "bed" <|> reserved "beds") >> return (circle (Sine (Product Fx (Multi [Constant 0.01, Constant 0.02]))) (Sine (Product Fx (Constant 0.1))) (Constant 0.5)),
  (reserved "ocean" <|> reserved "oceans") >> return (circle (Product (Sine Fy) (Product Fx (Multi [Constant 0.6, Constant 0.7, Constant 0.8]))) (Product (Sine Fy) (Product Fx (Multi [Constant 0.02, Constant 0.03, Constant 0.04]))) (Constant 0.7) ),
  (reserved "mouth" <|> reserved "mouths") >> return (circle (Product (Sine Fy) (Product Fx (Multi [Constant 0.5, Constant 0.6, Constant 0.7]))) (Product (Sine Fy) (Product Fx (Multi [Constant 0.01, Constant 0.02, Constant 0.03]))) (Constant 0.7) ),
  (reserved "fish" <|> reserved "fishes") >> return (circle (Product (Sine Fy) (Product Fx (Multi [Constant 0.5, Constant 0.6, Constant 0.7]))) (Product (Sine Fy) (Product Fx (Multi [Constant 0.01, Constant 0.02, Constant 0.03]))) (Constant 0.7) ),
  reserved "voice" >> return (circle (Product (Sine Fy) (Product Fx (Multi [Constant (-0.3), Constant (-0.4), Constant (-0.5)]))) (Product (Sine Fy) (Product Fx (Multi [Constant (-1), Constant (-1.1), Constant (-1.2)]))) (Constant 0.3) ),
  (reserved "tale" <|> reserved "tales") >> return (circle (Product (Sine Fx) (Product Fx (Constant (-0.4)))) (Product (Sine Fy) (Product Fx (Constant (-1)))) (Constant 0.3) ),
  (reserved "light" <|> reserved "lights") >> return (circle (Product (Sine Fx) (Multi [Constant 0.01, Constant 0.02, Constant 0.03])) (Product (Sine Fy) (Multi [Constant 0.1, Constant 0.2, Constant 0.3])) (Constant 0.5)),

  reserved "sand" >> return (rect (Saw Fx) (Saw Fy) (Constant 0.5) (Constant 0.5)),
  (reserved "noise" <|> reserved "noises") >> return (rect (Product (Saw Fx) (Multi [Constant 1, Constant 2, Constant 3])) (Product (Saw Fy) (Multi [Constant 0.1, Constant 0.2, Constant 0.3])) (Constant 0.5) (Constant 0.5)),
  (reserved "song" <|> reserved "songs") >> return (rect (Product (Saw Fy) (Product Fy (Multi [Constant 1, Constant 2, Constant 3]))) (Product (Saw Fy) (Multi [Constant 0.1, Constant 0.2, Constant 0.3])) (Constant 0.8) (Constant 0.8)),
  reserved "sky" >> return (rect (Product (Sine Fy) (Multi [Constant (-0.3), Constant (-0.4), Constant (-0.5)])) (Product (Sine Fy) (Product Fx (Multi [Constant (-1), Constant (-1.1), Constant (-1.2)]))) (Constant 0.5) (Constant 0.5)),
  reserved "death" >> return (rect (Product (Saw Fy) (Product Fy (Multi [Constant 0.5, Constant 0.6, Constant 0.7]))) (Product (Saw Fx) (Product Fy (Multi [Constant 0.1, Constant 0.2, Constant 0.4, Constant 0.5]))) (Constant 0.7) (Constant 0.7))
  ]

-- // Salidas

level :: Parser Extent
level = do
  x <- double
  return $ dbamp x

out :: Parser Output
out = choice [
  try $ reserved "<<>>" >> return (NamedOutput (T.pack "rgb")),
  try $ reserved "<<" >> return (NamedOutput (T.pack "red")),
  try $ reserved ">>" >> return (NamedOutput (T.pack "green")),
  try $ reserved "<>" >> return (NamedOutput (T.pack "blue")),
  try $ reserved "<<_>>" >> return (NamedOutput (T.pack "alpha"))
  ]



-- //////////////////////////////////

difference :: Graph -> Graph -> Graph
difference x y = Sum x (Product y (Constant (-1)))

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
