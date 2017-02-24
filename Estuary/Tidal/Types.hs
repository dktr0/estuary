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
  isEmpty:: a -> Bool

data RepOrDiv = Once | Rep Int | Div Int deriving (Eq)


instance Show RepOrDiv where
  show Once = ""
  show (Rep 1) = ""
  show (Rep n) = "*" ++ (show n)
  show (Div 1) = ""
  show (Div n) = "/" ++ (show n)


data GeneralPattern a = Atom a RepOrDiv | Blank | Group [GeneralPattern a] RepOrDiv | Layers [GeneralPattern a] RepOrDiv | TextPattern String deriving (Eq)

generalPatternIsEmpty::GeneralPattern a -> Bool
generalPatternIsEmpty (Atom _ _) = False
generalPatternIsEmpty Blank = True
generalPatternIsEmpty (Group [] _) = True
generalPatternIsEmpty (Group a _) = and $ fmap generalPatternIsEmpty a
generalPatternIsEmpty (Layers a _) = and $ fmap generalPatternIsEmpty a
generalPatternIsEmpty (TextPattern x) = length x == 0

showNoQuotes::(Show a)=> a->String
showNoQuotes x= if ((head x'=='"' && (last x')=='"') || (head x'== '\'' && last x'=='\'')) then if x''=="" then "~" else x'' else show x
  where x' = show x
        x''=(tail (init x'))

-- || (head x' =='\'' && last x'=='\'')
instance Show a => Show (GeneralPattern a) where
  show (Atom x r) = (showNoQuotes x) ++ (show r)
  show (Blank) = "~"
  show (Group [] _) = ""
  show (Group xs r) = "[" ++ (intercalate " " $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (Layers xs r) = "[" ++ (intercalate "," $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (TextPattern x) = x

type SampleName = String

newtype Sample = Sample (SampleName,Int) deriving (Eq)

instance Show Sample where
  show (Sample (x,0)) = showNoQuotes x
  show (Sample (x,y)) = (showNoQuotes x) ++ ":" ++ (show y)

data SpecificPattern =  Accelerate (GeneralPattern Double) | Bandf (GeneralPattern Int)
  | Bandq (GeneralPattern Double) | Begin (GeneralPattern Double) | Coarse (GeneralPattern Int)
  | Crush (GeneralPattern Int) | Cut (GeneralPattern Int) | Cutoff (GeneralPattern Int)
  | Delay (GeneralPattern Double) | Delayfeedback (GeneralPattern Double) | Delaytime (GeneralPattern Double)
  | End (GeneralPattern Double) | Gain (GeneralPattern Double) | Hcutoff (GeneralPattern Int)
  | Hresonance (GeneralPattern Double) | Loop (GeneralPattern Int) | N (GeneralPattern Int)
  | Pan (GeneralPattern Double)
  | Resonance (GeneralPattern Double) |S (GeneralPattern SampleName) | Shape (GeneralPattern Double)
  | Sound (GeneralPattern Sample) | Speed (GeneralPattern Double) | Unit (GeneralPattern Char)
  | Up (GeneralPattern Double) | Vowel (GeneralPattern Char) deriving (Eq)


instance Show SpecificPattern where
  show (Accelerate x) = "accelerate \"" ++ (show x) ++ "\""
  show (Bandf x) = "bandf \"" ++ (show x) ++ "\""
  show (Bandq x) = "bandq \"" ++ (show x) ++ "\""
  show (Begin x) = "begin \"" ++ (show x) ++ "\""
  show (Coarse x) = "coarse \"" ++ (show x) ++ "\""
  show (Crush x) = "crush \"" ++ (show x) ++ "\""
  show (Cut x) = "cut \"" ++ (show x) ++ "\""
  show (Cutoff x) = "cutoff \"" ++ (show x) ++ "\""
  show (Delay x) = "delay \"" ++ (show x) ++ "\""
  show (Delayfeedback x) = "delayfeedback \"" ++ (show x) ++ "\""
  show (Delaytime x) = "delaytime \"" ++ (show x) ++ "\""
  show (End x) = "end \"" ++ (show x) ++ "\""
  show (Gain x) = "gain \"" ++ (show x) ++ "\""
  show (Hcutoff x) = "hcutoff \"" ++ (show x) ++ "\""
  show (Hresonance x) = "hresonane \"" ++ (show x) ++ "\""
  show (Loop x) = "loop \"" ++ (show x) ++ "\""
  show (N x) = "n \"" ++ (show x) ++ "\""
  show (Pan x) = "pan \"" ++ (show x) ++ "\""
  show (Resonance x) = "resonance \"" ++ (show x) ++ "\""
  show (S x) = "s \"" ++ (show x) ++ "\""
  show (Shape x) = "shape \"" ++ (show x) ++ "\""
  show (Sound x) = "sound \"" ++ (show x) ++ "\""
  show (Speed x) = "speed \"" ++ (show x) ++ "\""
  show (Unit x) = "unit \"" ++ (show x) ++ "\""
  show (Up x) = "up \"" ++ (show x) ++ "\""
  show (Vowel x) = "vowel \"" ++ (show x) ++ "\""


instance ParamPatternable SpecificPattern where
  toParamPattern (Accelerate x) = Tidal.accelerate $ Tidal.p $ show x
  toParamPattern (Bandf x) = Tidal.bandf $ Tidal.p $ show x
  toParamPattern (Bandq x) = Tidal.bandq $ Tidal.p $ show x
  toParamPattern (Begin x) = Tidal.begin $ Tidal.p $ show x
  toParamPattern (Coarse x) = Tidal.coarse $ Tidal.p $ show x
  toParamPattern (Crush x) = Tidal.crush $ Tidal.p $ show x
  toParamPattern (Cut x) = Tidal.cut $ Tidal.p $ show x
  toParamPattern (Cutoff x) = Tidal.cutoff $ Tidal.p $ show x
  toParamPattern (Delay x) = Tidal.delay $ Tidal.p $ show x
  toParamPattern (Delaytime x) = Tidal.delaytime $ Tidal.p $ show x
  toParamPattern (Delayfeedback x) = Tidal.delayfeedback $ Tidal.p $ show x
  toParamPattern (End x) = if isEmpty $ End x then Tidal.end $ Tidal.p "1" else Tidal.end $ Tidal.p $ show x
  toParamPattern (Gain x) = Tidal.gain $ Tidal.p $ show x
  toParamPattern (Hcutoff x) = Tidal.hcutoff $ Tidal.p $ show x
  toParamPattern (Hresonance x) = Tidal.hresonance $ Tidal.p $ show x
  toParamPattern (Loop x) = Tidal.loop $ Tidal.p $ show x
  toParamPattern (N x) = Tidal.n $ Tidal.p $ show x
  toParamPattern (Pan x) = Tidal.pan $ Tidal.p $ show x
  toParamPattern (Resonance x) = Tidal.resonance $ Tidal.p $ show x
  toParamPattern (S x) = Tidal.s $ Tidal.p $ show x
  toParamPattern (Shape x) = Tidal.shape $ Tidal.p $ show x
  toParamPattern (Sound x) = Tidal.sound $ Tidal.p $ show x
  toParamPattern (Speed x) = Tidal.speed $ Tidal.p $ show x
  toParamPattern (Up x) = if isEmpty $ Up x then Tidal.up $ Tidal.p "0" else Tidal.up $ Tidal.p $ show x
  toParamPattern (Unit x) = Tidal.unit $ Tidal.p $ show x
  --toParamPattern (Vowel x) = Tidal.vowel $ Tidal.p $ show x
  toParamPattern (Vowel x) = if isEmpty $ Vowel x then Tidal.vowel $ Tidal.p $ "t" else Tidal.vowel $ Tidal.p $ show x
  isEmpty (Accelerate x) = generalPatternIsEmpty x
  isEmpty (Bandf x) = generalPatternIsEmpty x
  isEmpty (Bandq x) = generalPatternIsEmpty x
  isEmpty (Begin x) = generalPatternIsEmpty x
  isEmpty (Coarse x) = generalPatternIsEmpty x
  isEmpty (Crush x) = generalPatternIsEmpty x
  isEmpty (Cut x) = generalPatternIsEmpty x
  isEmpty (Cutoff x) = generalPatternIsEmpty x
  isEmpty (Delay x) = generalPatternIsEmpty x
  isEmpty (Delaytime x) = generalPatternIsEmpty x
  isEmpty (Delayfeedback x) = generalPatternIsEmpty x
  isEmpty (End x) = generalPatternIsEmpty x
  isEmpty (Gain x) = generalPatternIsEmpty x
  isEmpty (Hcutoff x) = generalPatternIsEmpty x
  isEmpty (Hresonance x) = generalPatternIsEmpty x
  isEmpty (Loop x) = generalPatternIsEmpty x
  isEmpty (N x) = generalPatternIsEmpty x
  isEmpty (Pan x) = generalPatternIsEmpty x
  isEmpty (Resonance x) = generalPatternIsEmpty x
  isEmpty (S x) = generalPatternIsEmpty x
  isEmpty (Shape x) = generalPatternIsEmpty x
  isEmpty (Sound x) = generalPatternIsEmpty x
  isEmpty (Speed x) = generalPatternIsEmpty x
  isEmpty (Up x) = generalPatternIsEmpty x
  isEmpty (Unit x) = generalPatternIsEmpty x
  isEmpty (End x) = generalPatternIsEmpty x
  isEmpty (Vowel x) = generalPatternIsEmpty x



emptySPattern :: SpecificPattern
emptySPattern = S Blank

sPatternFromList :: [String] -> SpecificPattern
sPatternFromList xs = S (Group (Prelude.map (\x -> Atom x Once) xs) Once)

emptyNPattern :: SpecificPattern
emptyNPattern = N Blank

nPatternFromList :: [Int] -> SpecificPattern
nPatternFromList xs = N (Group (Prelude.map (\x -> Atom x Once) xs) Once)

emptySoundPattern :: SpecificPattern
emptySoundPattern = Sound Blank


data PatternTransformer = NoTransformer | Rev | Slow Rational | Density Rational | Degrade | DegradeBy Double | Every Int PatternTransformer | Brak | Jux PatternTransformer | Chop Int  deriving (Ord,Eq)

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
  show (Chop i) = "chop (" ++ (show i) ++ ")"

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
applyPatternTransformer (Chop t) = Tidal.chop t


data TransformedPattern = TransformedPattern [PatternTransformer] SpecificPattern deriving (Eq)

instance Show TransformedPattern where
  show (TransformedPattern [NoTransformer] (Sound x)) = " $ " ++show (Sound x)
  show (TransformedPattern [NoTransformer] x) = show x
  show (TransformedPattern [] x) = show x
  show (TransformedPattern (NoTransformer:[]) x) = show x
  show (TransformedPattern ts x) = (intercalate " $ " (Prelude.map show ts))  ++ " $ " ++ (show x)


-- @what if the parameter is an empty pattern? -> do we need to guard for that?
-- what happens in tidal with s "a b c d" |=|
instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern ts x) = Prelude.foldr (\a b -> (applyPatternTransformer a) b) (toParamPattern x) ts
  isEmpty (TransformedPattern _ x) = isEmpty x

data PatternCombinator = Merge | Add | Subtract | Multiply | Divide deriving (Eq,Show,Read,Ord)

toTidalCombinator :: PatternCombinator -> (Tidal.ParamPattern -> Tidal.ParamPattern -> Tidal.ParamPattern)
toTidalCombinator Merge = (Tidal.|=|)
toTidalCombinator Add = (Tidal.|+|)
toTidalCombinator Subtract = (Tidal.|-|)
toTidalCombinator Multiply = (Tidal.|*|)
toTidalCombinator Divide = (Tidal.|/|)

data PatternChain = EmptyPatternChain | PatternChain TransformedPattern | PatternChain' TransformedPattern PatternCombinator PatternChain deriving (Eq)


instance Show PatternChain where
  show (EmptyPatternChain) = "silence"
  show (PatternChain x) = show x
  -- show (PatternChain' x _ EmptyPatternChain) = show x
  show (PatternChain' x Merge y) = if isEmpty x then if isEmpty y then "" else show y else if isEmpty y then show x else (show x) ++ " |=| " ++ (show y)
  show (PatternChain' x Add y) = (show x) ++ " |+| " ++ (show y)
  show (PatternChain' x Subtract y) = (show x) ++ " |-| " ++ (show y)
  show (PatternChain' x Multiply y) = (show x) ++ " |*| " ++ (show y)
  show (PatternChain' x Divide y) = (show x) ++ " |/| " ++ (show y)

instance ParamPatternable PatternChain where
  toParamPattern (EmptyPatternChain) = Tidal.silence
  toParamPattern (PatternChain' transPat pCombinator (PatternChain' transPat2 pCombinator2 pChain)) | isEmpty transPat = toParamPattern (PatternChain' transPat2 pCombinator2 pChain)
                                                                                                    | otherwise = (toTidalCombinator pCombinator2) ((toTidalCombinator pCombinator) (toParamPattern transPat) (toParamPattern transPat2)) (toParamPattern pChain)
  toParamPattern (PatternChain' transPat pCombinator patChain) | isEmpty transPat = toParamPattern patChain
                                                               | isEmpty patChain = toParamPattern transPat
                                                               | otherwise = (toTidalCombinator pCombinator) (toParamPattern transPat) (toParamPattern patChain)

  --toParamPattern (PatternChain' x c (PatternChain y)) | isEmpty x = toParamPattern y
  --                                                    | otherwise = (toTidalCombinator c) (toParamPattern x) (toParamPattern y)
  --toParamPattern (PatternChain' x c y) | isEmpty x = toParamPattern y
  --                                     | otherwise = (toTidalCombinator c) (toParamPattern x) (toParamPattern y)
  toParamPattern (PatternChain x) = toParamPattern x
  isEmpty (EmptyPatternChain) = True
  isEmpty (PatternChain x) = isEmpty x
  isEmpty (PatternChain' x _ y) = (isEmpty x) && (isEmpty y)


data StackedPatterns = StackedPatterns [PatternChain]

instance Show StackedPatterns where
  show (StackedPatterns xs) = "stack [" ++ (intercalate ", " (Prelude.map show xs)) ++ "]"

instance ParamPatternable StackedPatterns where
  toParamPattern (StackedPatterns xs) = Tidal.stack $ Prelude.map toParamPattern $ Prelude.filter (not . isEmpty) xs
  isEmpty (StackedPatterns xs) = and $ fmap isEmpty xs
