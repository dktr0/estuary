{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Tidal.Types where

import Text.JSON
import Text.JSON.Generic
import Data.List as List (intercalate, zip)
import Data.Map as Map
import Data.Ratio
import qualified Sound.Tidal.Context as Tidal

-- import Estuary.Languages.TidalParser
import Estuary.Utility
import Estuary.Types.Live

-- This module defines types that model elements of the notation employed in the Tidal language
-- including both notations within Tidal's pattern notation (i.e. within "quotation marks") as well
-- as a range of Haskell notations and definitions provided by the Tidal modules.
-- Each type implements the class Show in a way that produces valid Tidal/Haskell code.
-- And each type implements the class ParamPatternable which specifies the function
-- toParamPattern :: a -> ParamPattern (where ParamPattern is the type commonly given
-- as an argument to stream-generating computations in Tidal).

class ParamPatternable a where
  toParamPattern :: a -> Tidal.ParamPattern
  isEmptyFuture :: a -> Bool
  isEmptyPast :: a -> Bool


data RepOrDiv = Once | Rep Int | Div Int deriving (Eq,Data,Typeable)

instance JSON RepOrDiv where
  showJSON = toJSON
  readJSON = fromJSON

instance Show RepOrDiv where
  show Once = ""
  show (Rep n) = "*" ++ (show n)
  show (Div n) = "/" ++ (show n)


data Potential a = Potential a | PotentialDelete
    | PotentialMakeGroup | PotentialMakeLayer | PotentialLiveness Liveness
    | Inert | PotentialRepOrDiv| Potentials [Potential a] deriving (Eq,Show,Data,Typeable) --show just for testing

instance Data a => JSON (Potential a) where
  showJSON = toJSON
  readJSON = fromJSON


data GeneralPattern a =
  Atom a (Potential a) RepOrDiv |
  Blank (Potential a) |
  Group (Live  ([GeneralPattern a],RepOrDiv)) (Potential a) |
  Layers (Live ([GeneralPattern a],RepOrDiv)) (Potential a) |
  TextPattern String
  deriving (Eq,Data,Typeable)

instance Data a => JSON (GeneralPattern a) where
  showJSON = toJSON
  readJSON = fromJSON

isGroup (Group _ _)= True
isGroup _ = False
isLayers (Layers _ _) = True
isLayers _ = False
isAtom (Atom _ _ _) = True
isAtom _ = False
isBlank (Blank _) = True
isBlank _ = False

generalPatternIsEmptyFuture :: GeneralPattern a -> Bool
generalPatternIsEmptyFuture (Atom _ _ _) = False
generalPatternIsEmptyFuture (Blank _) = True
generalPatternIsEmptyFuture (Group (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (Group (Edited _ (xs,_)) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (Layers (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (Layers (Edited _ (xs,_)) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (TextPattern x) = length x == 0

generalPatternIsEmptyPast :: GeneralPattern a -> Bool
generalPatternIsEmptyPast (Atom _ _ _) = False
generalPatternIsEmptyPast (Blank _) = True
generalPatternIsEmptyPast (Group (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (Group (Edited (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (Layers (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (Layers (Edited (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (TextPattern x) = length x == 0

showNoQuotes :: (Show a) => a -> String
showNoQuotes x = if ((head x'=='"' && (last x')=='"') || (head x'== '\'' && last x'=='\'')) then if x''=="" then "~" else x'' else show x
  where x' = show x
        x''=(tail (init x'))

instance Show a => Show (GeneralPattern a) where
  show (Atom a _ r) = (showNoQuotes a) ++ (show r)
  show (Blank _) = "~"
  show (Group (Live ([],r) _) _) = ""
  show (Group (Live (xs,r) _) _) = "[" ++ (intercalate " " $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (Group (Edited ([],r) _) _) = ""
  show (Group (Edited (xs,r) _) _) = "[" ++ (intercalate " " $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (Layers (Live ([],r) _) _) = ""
  show (Layers (Live (xs,r) _) _) = "[" ++ (intercalate ", " $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (Layers (Edited ([],r) _) _) = ""
  show (Layers (Edited (xs,r) _) _) = "[" ++ (intercalate ", " $ Prelude.map (show) xs)  ++ "]" ++ (show r)
  show (TextPattern x) = Prelude.filter (not . (`elem` "?")) x


type SampleName = String

newtype Sample = Sample (SampleName,Int) deriving (Eq,Data,Typeable)

instance JSON Sample where
  showJSON = toJSON
  readJSON = fromJSON

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
  | Resonance (GeneralPattern Double) | S (GeneralPattern SampleName) | Shape (GeneralPattern Double)
  | Sound (GeneralPattern Sample) | Speed (GeneralPattern Double) | Unit (GeneralPattern Char)
  | Up (GeneralPattern Double) | Vowel (GeneralPattern Char) deriving (Eq,Data,Typeable)


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

-- ??? should all of the definitions below test for emptiness ???

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
  toParamPattern (End x) = if isEmptyPast $ End x then Tidal.end $ Tidal.p "1" else Tidal.end $ Tidal.p $ show x
  toParamPattern (Gain x) = Tidal.gain $ Tidal.p $ show x
  toParamPattern (Hcutoff x) = Tidal.hcutoff $ Tidal.p $ show x
  toParamPattern (Hresonance x) = Tidal.hresonance $ Tidal.p $ show x
  toParamPattern (Loop x) = Tidal.loop $ Tidal.p $ show x
  toParamPattern (N x) = if isEmptyPast $ N x then Tidal.n $ Tidal.p "0" else Tidal.n $ Tidal.p $ show x
  toParamPattern (Pan x) = Tidal.pan $ Tidal.p $ show x
  toParamPattern (Resonance x) = Tidal.resonance $ Tidal.p $ show x
  toParamPattern (S x) = Tidal.s $ Tidal.p $ show x
  toParamPattern (Shape x) = Tidal.shape $ Tidal.p $ show x
  toParamPattern (Sound x) = Tidal.sound $ Tidal.p $ show x
  toParamPattern (Speed x) = Tidal.speed $ Tidal.p $ show x
  toParamPattern (Up x) = if isEmptyPast $ Up x then Tidal.up $ Tidal.p "0" else Tidal.up $ Tidal.p $ show x
  toParamPattern (Unit x) = Tidal.unit $ Tidal.p $ show x
  toParamPattern (Vowel x) = if isEmptyPast $ Vowel x then Tidal.vowel $ Tidal.p $ "t" else Tidal.vowel $ Tidal.p $ show x
  isEmptyPast (Accelerate x) = generalPatternIsEmptyPast x
  isEmptyPast (Bandf x) = generalPatternIsEmptyPast x
  isEmptyPast (Bandq x) = generalPatternIsEmptyPast x
  isEmptyPast (Begin x) = generalPatternIsEmptyPast x
  isEmptyPast (Coarse x) = generalPatternIsEmptyPast x
  isEmptyPast (Crush x) = generalPatternIsEmptyPast x
  isEmptyPast (Cut x) = generalPatternIsEmptyPast x
  isEmptyPast (Cutoff x) = generalPatternIsEmptyPast x
  isEmptyPast (Delay x) = generalPatternIsEmptyPast x
  isEmptyPast (Delaytime x) = generalPatternIsEmptyPast x
  isEmptyPast (Delayfeedback x) = generalPatternIsEmptyPast x
  isEmptyPast (End x) = generalPatternIsEmptyPast x
  isEmptyPast (Gain x) = generalPatternIsEmptyPast x
  isEmptyPast (Hcutoff x) = generalPatternIsEmptyPast x
  isEmptyPast (Hresonance x) = generalPatternIsEmptyPast x
  isEmptyPast (Loop x) = generalPatternIsEmptyPast x
  isEmptyPast (N x) = generalPatternIsEmptyPast x
  isEmptyPast (Pan x) = generalPatternIsEmptyPast x
  isEmptyPast (Resonance x) = generalPatternIsEmptyPast x
  isEmptyPast (S x) = generalPatternIsEmptyPast x
  isEmptyPast (Shape x) = generalPatternIsEmptyPast x
  isEmptyPast (Sound x) = generalPatternIsEmptyPast x
  isEmptyPast (Speed x) = generalPatternIsEmptyPast x
  isEmptyPast (Up x) = generalPatternIsEmptyPast x
  isEmptyPast (Unit x) = generalPatternIsEmptyPast x
  isEmptyPast (Vowel x) = generalPatternIsEmptyPast x
  isEmptyFuture (Accelerate x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Bandf x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Bandq x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Begin x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Coarse x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Crush x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Cut x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Cutoff x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Delay x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Delaytime x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Delayfeedback x) = generalPatternIsEmptyFuture x
  isEmptyFuture (End x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Gain x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Hcutoff x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Hresonance x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Loop x) = generalPatternIsEmptyFuture x
  isEmptyFuture (N x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Pan x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Resonance x) = generalPatternIsEmptyFuture x
  isEmptyFuture (S x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Shape x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Sound x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Speed x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Up x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Unit x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Vowel x) = generalPatternIsEmptyFuture x

instance JSON SpecificPattern where
  showJSON = toJSON
  readJSON = fromJSON

emptySPattern :: SpecificPattern
emptySPattern = S (Blank Inert)


data PatternCombinator = Merge | Add | Subtract | Multiply | Divide deriving (Eq,Read,Data,Typeable)

instance JSON PatternCombinator where
  showJSON = toJSON
  readJSON = fromJSON

instance Show PatternCombinator where
  show (Merge) = "|=|"
  show (Add) = "|+|"
  show (Subtract) = "|-|"
  show (Multiply) = "|*|"
  show (Divide) = "|/|"

toTidalCombinator :: PatternCombinator -> (Tidal.ParamPattern -> Tidal.ParamPattern -> Tidal.ParamPattern)
toTidalCombinator Merge = (Tidal.|=|)
toTidalCombinator Add = (Tidal.|+|)
toTidalCombinator Subtract = (Tidal.|-|)
toTidalCombinator Multiply = (Tidal.|*|)
toTidalCombinator Divide = (Tidal.|/|)


data PatternTransformer =
  NoTransformer |
  Rev |
  Slow Rational |
  Density Rational |
  Degrade |
  DegradeBy Double |
  Every Int PatternTransformer |
  Brak |
  Jux PatternTransformer |
  Chop Int |
  Combine SpecificPattern PatternCombinator
  deriving (Eq,Data,Typeable)

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
  show (Combine p c) = (show p) ++ " " ++ (show c) ++ " "

instance JSON PatternTransformer where
  showJSON = toJSON
  readJSON = fromJSON

applyPatternTransformer :: PatternTransformer -> (Tidal.ParamPattern -> Tidal.ParamPattern)
applyPatternTransformer NoTransformer = id
applyPatternTransformer Rev = Tidal.rev
applyPatternTransformer (Slow f) = Tidal.slow $ pure f
applyPatternTransformer (Density f) = Tidal.density $ pure f
applyPatternTransformer Degrade = Tidal.degrade
applyPatternTransformer (DegradeBy d) = Tidal.degradeBy $ pure d
applyPatternTransformer (Every n t) = Tidal.every (pure n) (applyPatternTransformer t)
applyPatternTransformer (Brak) = Tidal.brak
applyPatternTransformer (Jux t) = Tidal.jux (applyPatternTransformer t)
applyPatternTransformer (Chop t) = Tidal.chop $ pure t
applyPatternTransformer (Combine p c) =  (toTidalCombinator c) $ toParamPattern p


data TransformedPattern =
  TransformedPattern PatternTransformer TransformedPattern |
  UntransformedPattern SpecificPattern |
  EmptyTransformedPattern
  deriving (Eq,Data,Typeable)

instance Show TransformedPattern where
  show (TransformedPattern t p) = (show t) ++ " " ++ (show p)
  show (UntransformedPattern u) = (show u)
  show (EmptyTransformedPattern) = ""

instance JSON TransformedPattern where
  showJSON = toJSON
  readJSON = fromJSON

instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern (Combine sPat comb) EmptyTransformedPattern) = toParamPattern sPat
  toParamPattern (TransformedPattern t p) = applyPatternTransformer t (toParamPattern p)
  toParamPattern (UntransformedPattern u) = toParamPattern u
  toParamPattern (EmptyTransformedPattern) = Tidal.silence
--  toParamPattern (TidalTextPattern x) = uncurry tidalParser $ forRendering x
  isEmptyFuture (UntransformedPattern u) = isEmptyFuture u
  isEmptyFuture (TransformedPattern t p) = isEmptyFuture p
  isEmptyFuture (EmptyTransformedPattern) = True
  isEmptyPast (TransformedPattern t p) = isEmptyPast p
  isEmptyPast (UntransformedPattern u) = isEmptyPast u
  isEmptyPast (EmptyTransformedPattern) = True


data StackedPatterns = StackedPatterns [TransformedPattern] deriving (Eq,Data,Typeable)

instance JSON StackedPatterns where
  showJSON = toJSON
  readJSON = fromJSON

instance Show StackedPatterns where
  show (StackedPatterns xs) = "stack [" ++ (intercalate ", " (Prelude.map show xs)) ++ "]"

instance ParamPatternable StackedPatterns where
  toParamPattern (StackedPatterns xs) = Tidal.stack $ Prelude.map toParamPattern $ Prelude.filter (not . isEmptyPast) xs
  isEmptyPast (StackedPatterns xs) = and $ fmap isEmptyPast xs
  isEmptyFuture (StackedPatterns xs) = and $ fmap isEmptyFuture xs
