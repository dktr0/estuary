module Estuary.Languages.SuperContinent (parseSuperContinent,Program,SuperContinentState,emptyState,runProgram,stateToSvgOps) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.IntMap.Strict
import qualified Data.Map.Strict as Map
import Control.Monad.State
import System.Random.MWC

import qualified Estuary.Types.SvgOp as SvgOp
import Estuary.Types.Color
import Estuary.Types.Transform
import Estuary.Types.Stroke

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
  X2 | Y2 |
  R | G | B | A
  deriving (Show,Ord,Eq)

propertyParser :: Parser Property
propertyParser = choice [
  reserved "type" >> return Type,
  reserved "x0" >> return X0,
  reserved "y0" >> return Y0,
  reserved "x1" >> return X1,
  reserved "y1" >> return Y1,
  reserved "x2" >> return X2,
  reserved "y2" >> return Y2,
  reserved "r" >> return R,
  reserved "g" >> return G,
  reserved "b" >> return B,
  reserved "a" >> return A
  ]

-- ie. a graph that will eventually be reduced to a loosely-typed value
data ValueGraph =
  Constant Value |
  Sum ValueGraph ValueGraph |
  Product ValueGraph ValueGraph |
  AudioProperty | -- for now there is only one audio property... but later this type can have more values
  Random | -- a random value between -1 and 1
  ObjectN -- the number of the currently relevant object
  deriving (Show)
  -- *** should add a property reflecting current position in meter as well!

valueGraphParser :: Parser ValueGraph
valueGraphParser = chainl1 productOfGraphs (reservedOp "+" >> return Sum)

productOfGraphs :: Parser ValueGraph
productOfGraphs = chainl1 valueGraphComponent (reservedOp "*" >> return Product)

valueGraphComponent :: Parser ValueGraph
valueGraphComponent = choice [
  parens valueGraphParser,
  reserved "audio" >> return AudioProperty,
  reserved "random" >> return Random,
  reserved "n" >> return ObjectN,
  valueParser >>= return . Constant
  ]

-- a loosely typed value, with semantics similar to those in javascript
-- ie. the value contains a value of a specific type but we support
-- automatic casting to each of the other types as well.
data Value =
  ValueType ObjectType |
  ValueInt Int |
  ValueDouble Double
  deriving (Show,Eq)

data ObjectType =
  Nil |
  Triangle
  deriving (Show,Eq)

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

sumOfValues :: Value -> Value -> Value
sumOfValues (ValueDouble x) y = ValueDouble (x + valueAsDouble y)
sumOfValues x (ValueDouble y) = ValueDouble (y + valueAsDouble x)
sumOfValues x y = ValueInt (valueAsInt x + valueAsInt y)

productOfValues :: Value -> Value -> Value
productOfValues (ValueDouble x) y = ValueDouble (x * valueAsDouble y)
productOfValues x (ValueDouble y) = ValueDouble (y * valueAsDouble x)
productOfValues x y = ValueInt (valueAsInt x * valueAsInt y)

valueParser :: Parser Value
valueParser = choice [
  try $ float >>= return . ValueDouble,
  try $ (fromIntegral <$> integer) >>= return . ValueInt,
  try $ reserved "nil" >> return (ValueType Nil),
  try $ reserved "triangle" >> return (ValueType Triangle)
  ]

data SuperContinentState = SuperContinentState {
  objects :: IntMap Object,
  randomGen :: GenIO
  }

emptyState :: IO SuperContinentState
emptyState = do
  rg <- create
  return $ SuperContinentState {
    objects = empty,
    randomGen = rg
  }

stateToSvgOps :: SuperContinentState -> [SvgOp.SvgOp]
stateToSvgOps s = concat $ fmap objectToSvgOps $ elems (objects s)

type Object = Map.Map Property Value

objectToSvgOps :: Object -> [SvgOp.SvgOp]
objectToSvgOps x | Map.lookup Type x == Just (ValueType Triangle) = [objectToTriangle x] -- *** note: this doesn't actually do the loose typing thing...
objectToSvgOps x | otherwise = []

objectToTriangle :: Object -> SvgOp.SvgOp
objectToTriangle obj = SvgOp.Triangle x0 y0 x1 y1 x2 y2 c s
  where
    x0 = valueAsDouble $ Map.findWithDefault (ValueDouble 0) X0 obj
    y0 = valueAsDouble $ Map.findWithDefault (ValueDouble 0) Y0 obj
    x1 = valueAsDouble $ Map.findWithDefault (ValueDouble 0) X1 obj
    y1 = valueAsDouble $ Map.findWithDefault (ValueDouble 0) Y1 obj
    x2 = valueAsDouble $ Map.findWithDefault (ValueDouble 0) X2 obj
    y2 = valueAsDouble $ Map.findWithDefault (ValueDouble 0) Y2 obj
    r = valueAsDouble $ Map.findWithDefault (ValueDouble 0) R obj
    g = valueAsDouble $ Map.findWithDefault (ValueDouble 0) G obj
    b = valueAsDouble $ Map.findWithDefault (ValueDouble 0) B obj
    a = valueAsDouble $ Map.findWithDefault (ValueDouble 100) A obj
    c = RGBA r g b a
    s = defaultStroke { strokeColor = c }

runProgram :: Double -> Program -> SuperContinentState -> IO SuperContinentState
runProgram audio program prevState = execStateT (mapM (runSuperContinentStatement audio) program) prevState

type ST = StateT SuperContinentState IO

runSuperContinentStatement :: Double -> Statement -> ST ()
runSuperContinentStatement audio (With sel deltas) = do
  prevState <- get
  let objs = selectOrInstantiateObjects sel $ objects prevState
  objs' <- runDeltasOnObjects audio objs deltas
  modify' $ \s -> s { objects = union objs' $ objects s }

selectOrInstantiateObjects :: Selector -> IntMap Object -> IntMap Object
selectOrInstantiateObjects (NumberedObjects ns) m = differenceWith (\_ b -> Just b) newObjects m
  where newObjects = fromList $ fmap (\x -> (x,Map.empty)) ns
selectOrInstantiateObjects AllObjects m = m

runDeltasOnObjects :: Double -> IntMap Object -> [Delta] -> ST (IntMap Object)
runDeltasOnObjects audio objs deltas = foldM (runDeltaOnObjects audio) objs deltas

runDeltaOnObjects :: Double -> IntMap Object -> Delta -> ST (IntMap Object)
runDeltaOnObjects audio objs delta = sequence $ mapWithKey (\k v -> runDeltaOnObject k audio delta v) objs
  
runDeltaOnObject :: Int -> Double -> Delta -> Object -> ST Object
runDeltaOnObject objectN audio (Delta prop graph) obj = do
  val <- getValueFromGraph objectN audio graph
  return $ Map.insert prop val obj

getValueFromGraph :: Int -> Double -> ValueGraph -> ST Value
getValueFromGraph _ _ (Constant v) = return v
getValueFromGraph objectN audio (Sum x y) = do
  x' <- getValueFromGraph objectN audio x
  y' <- getValueFromGraph objectN audio y
  return $ sumOfValues x' y'
getValueFromGraph objectN audio (Product x y) = do
  x' <- getValueFromGraph objectN audio x
  y' <- getValueFromGraph objectN audio y
  return $ productOfValues x' y'
getValueFromGraph _ audio (AudioProperty) = return $ ValueDouble audio
getValueFromGraph _ _ (Random) = do
  x <- gets randomGen
  y <- liftIO $ uniform x
  return $ ValueDouble y
getValueFromGraph objectN _ ObjectN = return $ ValueInt objectN

-- below this line all there is is our Parsec tokenized parsing definitions

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["type","x0","y0","x1","y1","x2","y2","nil","triangle","audio","random","r","g","b","a","n"],
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
