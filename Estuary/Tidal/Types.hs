module Estuary.Tidal.Types where

import Data.List as List (intercalate, zip)
import Data.Map as Map
import qualified Sound.Tidal.Context as Tidal

-- This module defines types that model elements of the notation employed in the Tidal language
-- including both notations within Tidal's pattern notation (i.e. within "quotation marks") as well
-- as a range of Haskell notations and definitions provided by the Tidal modules.
-- Each type implements the class Show in a way that produces valid Tidal/Haskell code.
-- And each type implements the class ParamPatternable which specifies the function
-- toParamPattern :: a -> ParamPattern (where ParamPattern is the type commonly given
-- as an argument to stream-generating computations in Tidal).

class ParamPatternable a where
  toParamPattern :: a -> Tidal.ParamPattern

data RepOrDiv = Once | Rep Int | Div Int deriving (Eq)

instance Show RepOrDiv where
  show Once = ""
  show (Rep n) = "*" ++ (show n)
  show (Div n) = "/" ++ (show n)

data GeneralPattern a = Atom a RepOrDiv | Blank | Group [GeneralPattern a] RepOrDiv | Layers [GeneralPattern a] RepOrDiv deriving (Eq)

instance Show a => Show (GeneralPattern a) where
  show (Atom x r) = (show x) ++ (show r)
  show (Blank) = "~"
  show (Group xs r) = "[" ++ (intercalate " " $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (Layers xs r) = "[" ++ (intercalate "," $ Prelude.map (show) xs)  ++ "]" ++ (show r)

type SampleName = String

newtype Sample = Sample (SampleName,Int) deriving (Eq)

instance Show Sample where
  show (Sample (x,0)) = show x
  show (Sample (x,y)) = (show x) ++ ":" ++ (show y)

data SpecificPattern = S (GeneralPattern SampleName) | N (GeneralPattern Int) | Sound (GeneralPattern Sample) | Pan (GeneralPattern Double) deriving (Eq)

instance Show SpecificPattern where
  show (S x) = "s \"" ++ (show x) ++ "\""
  show (N x) = "n \"" ++ (show x) ++ "\""
  show (Sound x) = "sound \"" ++ (show x) ++ "\""
  show (Pan x) = "pan \"" ++ (show x) ++ "\""

instance ParamPatternable SpecificPattern where
  toParamPattern (S x) = Tidal.s $ Tidal.p $ show x
  toParamPattern (N x) = Tidal.n $ Tidal.p $ show x
  toParamPattern (Sound x) = Tidal.sound $ Tidal.p $ show x
  toParamPattern (Pan x) = Tidal.pan $ Tidal.p $ show x


data PatternTransformer = NoTransformer | Rev | Slow Rational | Density Rational | Degrade | DegradeBy Double | Every Int PatternTransformer | Brak | Jux PatternTransformer deriving (Ord,Eq)

instance Show PatternTransformer where
  show NoTransformer = ""
  show Rev = "rev"
  show (Slow f) = "slow " ++ (show f)
  show (Density f) = "density " ++ (show f)
  show Degrade = "degrade"
  show (DegradeBy f) = "degradeBy " ++ (show f)
  show (Every n t) = "every " ++ (show n) ++ "(" ++ show t ++ ")"
  show (Brak) = "brak"
  show (Jux f) = "jux (" ++ (show f) ++ ")"

applyPatternTransformer :: PatternTransformer -> (Tidal.ParamPattern -> Tidal.ParamPattern)
applyPatternTransformer NoTransformer = id
applyPatternTransformer Rev = Tidal.rev
applyPatternTransformer (Slow f) = Tidal.slow f
applyPatternTransformer (Density f) = Tidal.density f
applyPatternTransformer Degrade = Tidal.degrade
applyPatternTransformer (DegradeBy d) = Tidal.degradeBy d
applyPatternTransformer (Every n t) = Tidal.every n (applyPatternTransformer t)
applyPatternTransformer (Brak) = Tidal.brak
applyPatternTransformer (Jux t) = Tidal.jux (applyPatternTransformer t)


data TransformedPattern = TransformedPattern [PatternTransformer] SpecificPattern deriving (Eq)

instance Show TransformedPattern where
  show (TransformedPattern ts x) = (intercalate " $ " (Prelude.map show ts))  ++ " $ " ++ (show x)

instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern ts x) = Prelude.foldr (\a b -> (applyPatternTransformer a) b) (toParamPattern x) ts



data PatternCombinator = Merge | Add | Subtract | Multiply | Divide deriving (Eq,Show)

data PatternChain = PatternChain TransformedPattern | PatternChain' TransformedPattern PatternCombinator PatternChain deriving (Eq)

instance Show PatternChain where
  show (PatternChain x) = show x
  show (PatternChain' x Merge y) = (show x) ++ " |=| " ++ (show y)
  show (PatternChain' x Add y) = (show x) ++ " |=| " ++ (show y)
  show (PatternChain' x Subtract y) = (show x) ++ " |=| " ++ (show y)
  show (PatternChain' x Multiply y) = (show x) ++ " |=| " ++ (show y)
  show (PatternChain' x Divide y) = (show x) ++ " |=| " ++ (show y)

instance ParamPatternable PatternChain where
  toParamPattern (PatternChain x) = toParamPattern x
  toParamPattern (PatternChain' x Merge y) = (Tidal.|=|) (toParamPattern x) (toParamPattern y)
  toParamPattern (PatternChain' x Add y) =  (Tidal.|+|) (toParamPattern x) (toParamPattern y)
  toParamPattern (PatternChain' x Subtract y) =  (Tidal.|-|) (toParamPattern x) (toParamPattern y)
  toParamPattern (PatternChain' x Multiply y) =  (Tidal.|*|) (toParamPattern x) (toParamPattern y)
  toParamPattern (PatternChain' x Divide y) =  (Tidal.|/|) (toParamPattern x) (toParamPattern y)



data StackedPatterns = StackedPatterns [PatternChain]

instance Show StackedPatterns where
  show (StackedPatterns xs) = "stack [" ++ (intercalate "," (Prelude.map show xs)) ++ "]"

instance ParamPatternable StackedPatterns where
  toParamPattern (StackedPatterns xs) = Tidal.stack (Prelude.map toParamPattern xs)
