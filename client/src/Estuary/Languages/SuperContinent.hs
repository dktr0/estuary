module Estuary.Languages.SuperContinent {- (parseSuperContinent,emptyState,runProgram,stateToSvgOps) -} where

{-

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

parseSuperContinent :: String -> Either ParseError Program
parseSuperContinent = parse programParser "SuperContinent"


type Program = [Statement]

programParser :: Parser Program
programParser = semiSep statementParser


data Statement =
  With Selector [Delta]
  -- Assignment String Expression -- for later...
  deriving (Show)

statementParser :: Parser Statement
statementParser = With <$> selectorParser <*> deltasParser


data Selector = NumberedObjects [Int] | AllObjects deriving (Show)

selectorParser :: Parser Selector
selectorParser = choice [
  try $ brackets $ do
    x <- fromIntegral <$> integer
    reservedOp ".."
    y <- fromIntegral <$> integer
    return $ NumberedObjects [x..y],
  try $ brackets $ do
    xs <- commaSep (fromIntegral <$> integer)
    return $ NumberedObjects xs,
  try $ do
    x <- fromIntegral <$> integer
    return $ NumberedObjects [x],
  reservedOp "*" >> return AllObjects
  ]


data Delta = Delta Property ValueGraph deriving (Show)

deltasParser :: Parser [Delta]
deltasParser = commaSep deltaParser

deltaParser :: Parser Delta
deltaParser = do
  p <- propertyParser
  reservedOp "="
  v <- valueGraphParser
  return $ Delta p v

data Property =
  Type |
  X0 | Y0 |
  X1 | Y1 |
  X2 | Y2
  deriving (Show)

propertyParser :: Parser Property
propertyParser = choice [
  reserved "type" >> return Type,
  reserved "x0" >> return X0,
  reserved "y0" >> return Y0,
  reserved "x1" >> return X1,
  reserved "y1" >> return Y1,
  reserved "x2" >> return X2,
  reserved "y2" >> return Y2
  ]

-- ie. a graph that will eventually be reduced to a loosely-typed value
data ValueGraph =
  Constant Value |
  Sum ValueGraph ValueGraph |
  Product ValueGraph ValueGraph |
  AudioProperty | -- for now there is only one audio property... but later this type can have more values
  Random -- a random value between -1 and 1
  deriving (Show)

valueGraphParser :: Parser ValueGraph
valueGraphParser = chainl1 productOfGraphs (reservedOp "+" >> return Sum)

productOfGraphs :: Parser ValueGraph
productOfGraphs = chainl1 valueGraphComponent (reservedOp "*" >> return Product)

valueGraphComponent :: Parser ValueGraph
valueGraphComponent = choice [
  parens valueGraphParser,
  reserved "audio" >> return AudioProperty,
  reserved "random" >> return Random,
  valueParser >>= return . Constant
  ]

-- a loosely typed value, with semantics similar to those in javascript
-- ie. the value contains a value of a specific type but we support
-- automatic casting to each of the other types as well.
data Value =
  ValueType ObjectType |
  ValueInt Int |
  ValueDouble Double
  deriving (Show)

data ObjectType =
  Nil |
  Triangle
  deriving (Show)

valueAsType :: Value -> ObjectType
valueAsType (ValueType x) = x
valueAsType (ValueInt 0) = Nil
valueAsType (ValueInt 1) = Triangle
valueAsType (ValueInt _) = Nil
valueAsType (ValueDouble x) = valueAsType (ValueInt (floor x))

valueAsInt :: Value -> Int
valueAsInt (ValueType Triangle) = 1
valueAsInt (ValueType _) = 0
valueAsInt (ValueInt x) = x
valueAsInt (ValueDouble x) = floor x

valueAsDouble :: Value -> Double
valueAsDouble (ValueType x) = fromIntegral $ valueAsInt (ValueType x)
valueAsDouble (ValueInt x) = fromIntegral x
valueAsDouble (ValueDouble x) = x

valueParser :: Parser Value
valueParser = choice [
  try $ float >>= return . ValueDouble,
  try $ (fromIntegral <$> integer) >>= return . ValueInt,
  try $ reserved "nil" >> return (ValueType Nil),
  try $ reserved "triangle" >> return (ValueType Triangle)
  ]

data SuperContinentState = SuperContinentState {
  objects :: IntMap Object
  -- placeholder: will also soon need a random number generator here
  }

emptyState :: IO SuperContinentState
emptyState = return $ SuperContinentState { objects = empty }

stateToSvgOps :: SuperContinentState -> [SvgOp]
stateToSvgOps s = concat $ fmap objectToSvgOps $ elems (objects s)

type Object = Map Property Value

objectToSvgOps :: Object -> [SvgOp]
objectToSvgOps x | lookup Type x == Just Triangle = .... *** working here ***
objectToSvgOps x | otherwise = []


runSuperContinentProgram :: Double -> SuperContinentState -> Program -> IO SuperContinentState
runSuperContinentProgram audio prevState program = foldM (runSuperContinentStatement audio) prevState program

runSuperContinentStatement :: Double -> SuperContinentState -> Statement -> IO SuperContinentState
runSuperContinentStatement audio prevState (With sel deltas) = do
  -- *** working here also ***
  -- get the objects (make new ones if necessary)
  -- run the delta on each object to form modified objects
  -- (note that running the delta involves io because of randomness)
  -- insert the modified objects back in the state

-- below this line all there is is our Parsec tokenized parsing definitions

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["type","x0","y0","x1","y1","x2","y2","nil","triangle","audio","random"],
  P.reservedOpNames = ["..","=","*","+"]
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
