{-# LANGUAGE DeriveGeneric #-}

module Estuary.Tidal.Types where

import Data.List as List (intercalate, zip)
import Data.Map.Strict as Map
import Data.Ratio
import GHC.Generics
import Data.Aeson

import Estuary.Types.Live

data RepOrDiv = Once | Rep Int | Div Int deriving (Eq,Generic)

instance ToJSON RepOrDiv
instance FromJSON RepOrDiv

instance Show RepOrDiv where
  show Once = ""
  show (Rep n) = "*" ++ (show n)
  show (Div n) = "/" ++ (show n)


data Potential a = Potential a | PotentialDelete
    | PotentialMakeGroup | PotentialMakeLayer | PotentialLiveness Liveness
    | Inert | PotentialRepOrDiv| Potentials [Potential a] deriving (Eq,Show,Generic)

instance ToJSON a => ToJSON (Potential a)
instance FromJSON a => FromJSON (Potential a)


data GeneralPattern a =
  Atom a (Potential a) RepOrDiv |
  Blank (Potential a) |
  Group (Live  ([GeneralPattern a],RepOrDiv)) (Potential a) |
  Layers (Live ([GeneralPattern a],RepOrDiv)) (Potential a) |
  TextPattern String
  deriving (Eq,Generic)

instance ToJSON a => ToJSON (GeneralPattern a)
instance FromJSON a => FromJSON (GeneralPattern a)

isGroup :: GeneralPattern a -> Bool
isGroup (Group _ _)= True
isGroup _ = False

isLayers :: GeneralPattern a -> Bool
isLayers (Layers _ _) = True
isLayers _ = False

isAtom :: GeneralPattern a -> Bool
isAtom (Atom _ _ _) = True
isAtom _ = False

isBlank :: GeneralPattern a -> Bool
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

newtype Sample = Sample (SampleName,Int) deriving (Eq,Generic)

instance ToJSON Sample
instance FromJSON Sample

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
  | Up (GeneralPattern Double) | Vowel (GeneralPattern Char) deriving (Eq,Generic)


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

instance ToJSON SpecificPattern
instance FromJSON SpecificPattern

emptySPattern :: SpecificPattern
emptySPattern = S (Blank Inert)


data PatternCombinator = Merge | Add | Subtract | Multiply | Divide deriving (Eq,Read,Generic)

instance ToJSON PatternCombinator
instance FromJSON PatternCombinator

instance Show PatternCombinator where
  show (Merge) = "|=|"
  show (Add) = "|+|"
  show (Subtract) = "|-|"
  show (Multiply) = "|*|"
  show (Divide) = "|/|"

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
  deriving (Eq,Generic)

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

instance ToJSON PatternTransformer
instance FromJSON PatternTransformer


data TransformedPattern =
  TransformedPattern PatternTransformer TransformedPattern |
  UntransformedPattern SpecificPattern |
  EmptyTransformedPattern
  deriving (Eq,Generic)

instance Show TransformedPattern where
  show (TransformedPattern t p) = (show t) ++ " " ++ (show p)
  show (UntransformedPattern u) = (show u)
  show (EmptyTransformedPattern) = ""

instance ToJSON TransformedPattern
instance FromJSON TransformedPattern


data StackedPatterns = StackedPatterns [TransformedPattern] deriving (Eq,Generic)

instance ToJSON StackedPatterns
instance FromJSON StackedPatterns

instance Show StackedPatterns where
  show (StackedPatterns xs) = "stack [" ++ (intercalate ", " (Prelude.map show xs)) ++ "]"
