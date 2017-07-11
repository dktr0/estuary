 module Estuary.Tidal.Types where

import Text.JSON
import Data.List as List (intercalate, zip)
import Data.Map as Map
import Data.Ratio
import qualified Sound.Tidal.Context as Tidal

import Estuary.Utility

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


data RepOrDiv = Once | Rep Int | Div Int deriving (Eq)

instance Show RepOrDiv where
  show Once = ""
  --show (Rep 1) = ""
  show (Rep n) = "*" ++ (show n)
  --show (Div 1) = ""
  show (Div n) = "/" ++ (show n)

instance JSON RepOrDiv where
  showJSON Once = showJSON "o"
  showJSON (Rep n) = encJSDict [("r",n)]
  showJSON (Div n) = encJSDict [("d",n)]
  readJSON (JSString x) | fromJSString x == "o" = Ok (Once)
                        | otherwise = Error $ "can't parse JSString as RepOrDiv: " ++ (show x)
  readJSON (JSObject x) | firstKey x == "r" = Rep <$> valFromObj "r" x
                        | firstKey x == "d" = Div <$> valFromObj "d" x
                        | otherwise = Error $ "can't parse JSObject as RepOrDiv: " ++ (show x)
  readJSON _ = Error "can't parse as RepOrDiv (not JSString nor JSObject)"

data Liveness = L3 | L4 deriving (Eq,Show)

instance JSON Liveness where
  showJSON L3 = showJSON "L3"
  showJSON L4 = showJSON "L4"
  readJSON (JSString x) | fromJSString x == "L3" = Ok L3
  readJSON (JSString x) | fromJSString x == "L4" = Ok L4
  readJSON _ = Error "can't parse as Liveness"


data Potential a = Potential a | PotentialDelete
    | PotentialMakeGroup | PotentialMakeLayer | PotentialLiveness Liveness
    | Inert | PotentialRepOrDiv| Potentials [Potential a] deriving (Eq,Show) --show just for testing

instance JSON a => JSON (Potential a) where
  showJSON (Potential a) = encJSDict [("P",a)]
  showJSON PotentialDelete = showJSON "Pd"
  showJSON PotentialMakeGroup = showJSON "Pmg"
  showJSON PotentialMakeLayer = showJSON "Pml"
  showJSON (PotentialLiveness x) = encJSDict [("Pl",showJSON x)]
  showJSON Inert = showJSON "I"
  showJSON PotentialRepOrDiv = showJSON "Prd"
  showJSON (Potentials xs) = encJSDict [("Ps",xs)]
  readJSON (JSObject x) | firstKey x == "P" = Potential <$> valFromObj "P" x
  readJSON (JSString x) | fromJSString x == "Pd" = Ok PotentialDelete
  readJSON (JSString x) | fromJSString x == "Pmg" = Ok PotentialMakeGroup
  readJSON (JSString x) | fromJSString x == "Pml" = Ok PotentialMakeLayer
  readJSON (JSObject x) | firstKey x == "Pl" = PotentialLiveness <$> valFromObj "Pl" x
  readJSON (JSString x) | fromJSString x == "I" = Ok Inert
  readJSON (JSString x) | fromJSString x == "Prd" = Ok PotentialRepOrDiv
  readJSON (JSObject x) | firstKey x == "Ps" = Potentials <$> valFromObj "Ps" x
  readJSON _ = Error "can't parse as Potential"


data Live a = Live a Liveness | Edited a a deriving(Eq)

instance JSON a => JSON (Live a) where
  showJSON (Live a liveness) = encJSDict [("Live",showJSON a),("l",showJSON liveness)]
  showJSON (Edited past future) = encJSDict [("Edited",past),("f",future)]
  readJSON (JSObject x) | firstKey x == "Live" = Live <$> valFromObj "Live" x <*> valFromObj "l" x
  readJSON (JSObject x) | firstKey x == "Edited" = Edited <$> valFromObj "Edited" x <*> valFromObj "f" x
  readJSON _ = Error "can't parse as Live"

isEdited :: Live a -> Bool
isEdited (Edited _ _) = True
isEdited _ = False

data GeneralPattern a =
  Atom a (Potential a) RepOrDiv |
  Blank (Potential a) |
  Group (Live  ([GeneralPattern a],RepOrDiv)) (Potential a) |
  Layers (Live ([GeneralPattern a],RepOrDiv)) (Potential a) |
  TextPattern String
  deriving (Eq)

isGroup (Group _ _)= True
isGroup _ = False
isLayers (Layers _ _) = True
isLayers _ = False
isAtom (Atom _ _ _) = True
isAtom _ = False
isBlank (Blank _) = True
isBlank _ = False

generalPatternIsEmptyFuture::GeneralPattern a -> Bool
generalPatternIsEmptyFuture (Atom _ _ _) = False
generalPatternIsEmptyFuture (Blank _) = True
generalPatternIsEmptyFuture (Group (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (Group (Edited _ (xs,_)) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (Layers (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (Layers (Edited _ (xs,_)) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyFuture (TextPattern x) = length x == 0

generalPatternIsEmptyPast::GeneralPattern a -> Bool
generalPatternIsEmptyPast (Atom _ _ _) = False
generalPatternIsEmptyPast (Blank _) = True
generalPatternIsEmptyPast (Group (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (Group (Edited (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (Layers (Live (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (Layers (Edited (xs,_) _) _) = and $ fmap generalPatternIsEmptyFuture xs
generalPatternIsEmptyPast (TextPattern x) = length x == 0

showNoQuotes::(Show a)=> a->String
showNoQuotes x= if ((head x'=='"' && (last x')=='"') || (head x'== '\'' && last x'=='\'')) then if x''=="" then "~" else x'' else show x
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

instance JSON a => JSON (GeneralPattern a) where
  showJSON (Atom a p r) = encJSDict [("a",showJSON a),("p",showJSON p),("r",showJSON r)]
  showJSON (Blank p) = encJSDict [("b",showJSON p)]
  showJSON (Group x p) = encJSDict [("g",showJSON x),("p",showJSON p)]
  showJSON (Layers x p) = encJSDict [("l",showJSON x),("p",showJSON p)]
  showJSON (TextPattern t) = encJSDict [("t",showJSON t)]
  readJSON (JSObject x) | firstKey x == "a" = Atom <$> valFromObj "a" x <*> valFromObj "p" x <*> valFromObj "r" x
  readJSON (JSObject x) | firstKey x == "b" = Blank <$> valFromObj "b" x
  readJSON (JSObject x) | firstKey x == "g" = Group <$> valFromObj "g" x <*> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "l" = Layers <$> valFromObj "l" x <*> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "t" = TextPattern <$> valFromObj "t" x


type SampleName = String

newtype Sample = Sample (SampleName,Int) deriving (Eq)

instance JSON Sample where
  showJSON (Sample (x,y)) = encJSDict [("s",showJSON x),("n",showJSON y)]
  readJSON (JSObject x) | firstKey x == "s" = (\a b -> Sample (a,b)) <$> c <*> d
    where c = valFromObj "s" x
          d = valFromObj "n" x
  readJSON _ = Error "can't parsed as Sample"

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
  showJSON (Accelerate x) = encJSDict [("a",x)]
  showJSON (Bandf x) = encJSDict [("bf",x)]
  showJSON (Bandq x) = encJSDict [("bq",x)]
  showJSON (Begin x) = encJSDict [("b",x)]
  showJSON (Coarse x) = encJSDict [("co",x)]
  showJSON (Crush x) = encJSDict [("cr",x)]
  showJSON (Cut x) = encJSDict [("cut",x)]
  showJSON (Cutoff x) = encJSDict [("cu",x)]
  showJSON (Delay x) = encJSDict [("de",x)]
  showJSON (Delayfeedback x) = encJSDict [("df",x)]
  showJSON (Delaytime x) = encJSDict [("dt",x)]
  showJSON (End x) = encJSDict [("e",x)]
  showJSON (Gain x) = encJSDict [("g",x)]
  showJSON (Hcutoff x) = encJSDict [("hc",x)]
  showJSON (Hresonance x) = encJSDict [("hr",x)]
  showJSON (Loop x) = encJSDict [("l",x)]
  showJSON (N x) = encJSDict [("n",x)]
  showJSON (Pan x) = encJSDict [("p",x)]
  showJSON (Resonance x) = encJSDict [("r",x)]
  showJSON (S x) = encJSDict [("s",x)]
  showJSON (Shape x) = encJSDict [("sh",x)]
  showJSON (Sound x) = encJSDict [("so",x)]
  showJSON (Speed x) = encJSDict [("sp",x)]
  showJSON (Unit x) = encJSDict [("un",x)]
  showJSON (Up x) = encJSDict [("u",x)]
  showJSON (Vowel x) = encJSDict [("v",x)]
  readJSON (JSObject x) | firstKey x == "a" = Accelerate <$> valFromObj "a" x
  readJSON (JSObject x) | firstKey x == "bf" = Bandf <$> valFromObj "bf" x
  readJSON (JSObject x) | firstKey x == "bq" = Bandq <$> valFromObj "bq" x
  readJSON (JSObject x) | firstKey x == "b" = Begin <$> valFromObj "b" x
  readJSON (JSObject x) | firstKey x == "co" = Coarse <$> valFromObj "co" x
  readJSON (JSObject x) | firstKey x == "cr" = Crush <$> valFromObj "cr" x
  readJSON (JSObject x) | firstKey x == "cut" = Cut <$> valFromObj "cut" x
  readJSON (JSObject x) | firstKey x == "cu" = Cutoff <$> valFromObj "cu" x
  readJSON (JSObject x) | firstKey x == "de" = Delay <$> valFromObj "de" x
  readJSON (JSObject x) | firstKey x == "df" = Delayfeedback <$> valFromObj "df" x
  readJSON (JSObject x) | firstKey x == "dt" = Delaytime <$> valFromObj "dt" x
  readJSON (JSObject x) | firstKey x == "e" = End <$> valFromObj "e" x
  readJSON (JSObject x) | firstKey x == "g" = Gain <$> valFromObj "g" x
  readJSON (JSObject x) | firstKey x == "hc" = Hcutoff <$> valFromObj "hc" x
  readJSON (JSObject x) | firstKey x == "hr" = Hresonance <$> valFromObj "hr" x
  readJSON (JSObject x) | firstKey x == "l" = Loop <$> valFromObj "l" x
  readJSON (JSObject x) | firstKey x == "n" = N <$> valFromObj "n" x
  readJSON (JSObject x) | firstKey x == "p" = Pan <$> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "r" = Resonance <$> valFromObj "r" x
  readJSON (JSObject x) | firstKey x == "s" = S <$> valFromObj "s" x
  readJSON (JSObject x) | firstKey x == "sh" = Shape <$> valFromObj "sh" x
  readJSON (JSObject x) | firstKey x == "so" = Sound <$> valFromObj "so" x
  readJSON (JSObject x) | firstKey x == "sp" = Speed <$> valFromObj "sp" x
  readJSON (JSObject x) | firstKey x == "un" = Unit <$> valFromObj "un" x
  readJSON (JSObject x) | firstKey x == "u" = Up <$> valFromObj "u" x
  readJSON (JSObject x) | firstKey x == "v" = Vowel <$> valFromObj "v" x
  readJSON _ = Error "can't parse as SpecificPattern"


emptySPattern :: SpecificPattern
emptySPattern = S (Blank Inert)


data PatternCombinator = Merge | Add | Subtract | Multiply | Divide deriving (Eq,Read)

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

instance JSON PatternCombinator where
  showJSON Merge = showJSON "M"
  showJSON Add = showJSON "A"
  showJSON Subtract = showJSON "S"
  showJSON Multiply = showJSON "Mu"
  showJSON Divide = showJSON "D"
  readJSON (JSString x) | fromJSString x == "M" = Ok Merge
  readJSON (JSString x) | fromJSString x == "A" = Ok Add
  readJSON (JSString x) | fromJSString x == "S" = Ok Subtract
  readJSON (JSString x) | fromJSString x == "Mu" = Ok Multiply
  readJSON (JSString x) | fromJSString x == "D" = Ok Divide
  readJSON _ = Error "can't parse as PatternCombinator"


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
  deriving (Eq)

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
  showJSON NoTransformer = showJSON "No"
  showJSON Rev = showJSON "Rev"
  showJSON (Slow f) = encJSDict [("Slow",numerator f),("d",denominator f)]
  showJSON (Density f) = encJSDict [("Density",numerator f),("d",denominator f)]
  showJSON Degrade = showJSON "Degrade"
  showJSON (DegradeBy f) = encJSDict [("DegradeBy",f)]
  showJSON (Every n t) = encJSDict [("Every",showJSON n),("t",showJSON t)]
  showJSON (Brak) = showJSON "Brak"
  showJSON (Jux f) = encJSDict [("Jux",f)]
  showJSON (Chop i) = encJSDict [("Chop",i)]
  showJSON (Combine p c) = encJSDict [("Combine",showJSON p),("c",showJSON c)]
  readJSON (JSString x) | fromJSString x == "No" = Ok NoTransformer
  readJSON (JSString x) | fromJSString x == "Rev" = Ok Rev
  readJSON (JSObject x) | firstKey x == "Slow" = (\a b -> Slow (a%b)) <$> valFromObj "Slow" x <*> valFromObj "d" x
  readJSON (JSObject x) | firstKey x == "Density" = (\a b -> Density (a%b)) <$> valFromObj "Density" x <*> valFromObj "d" x
  readJSON (JSString x) | fromJSString x == "Degrade" = Ok Degrade
  readJSON (JSObject x) | firstKey x == "DegradeBy" = DegradeBy <$> valFromObj "DegradeBy" x
  readJSON (JSObject x) | firstKey x == "Every" = Every <$> valFromObj "Every" x <*> valFromObj "t" x
  readJSON (JSString x) | fromJSString x == "Brak" = Ok Brak
  readJSON (JSObject x) | firstKey x == "Jux" = Jux <$> valFromObj "Jux" x
  readJSON (JSObject x) | firstKey x == "Chop" = Chop <$> valFromObj "Chop" x
  readJSON (JSObject x) | firstKey x == "Combine" = Combine <$> valFromObj "Combine" x <*> valFromObj "c" x
  readJSON _ = Error "can't parse as PatternTransformer"


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



data TransformedPattern = TransformedPattern PatternTransformer TransformedPattern | UntransformedPattern SpecificPattern | EmptyTransformedPattern | TextPatternChain String String String deriving (Eq)

instance Show TransformedPattern where
  show (TransformedPattern t p) = (show t) ++ " " ++ (show p)
  show (UntransformedPattern u) = (show u)
  show (EmptyTransformedPattern) = ""
  show (TextPatternChain a b c) = (show a) ++ " " ++ (show b) ++ " " ++ (show c)

instance JSON TransformedPattern where
  showJSON (TransformedPattern t p) = encJSDict [("TP",showJSON t),("p",showJSON p)]
  showJSON (UntransformedPattern s) = encJSDict [("UP",showJSON s)]
  showJSON (EmptyTransformedPattern) = showJSON "E"
  showJSON (TextPatternChain a b c) = encJSDict [("Text",a),("b",b),("c",c)]
  readJSON (JSObject x) | firstKey x == "TP" = TransformedPattern <$> valFromObj "TP" x <*>  valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "UP" = UntransformedPattern <$> valFromObj "UP" x
  readJSON (JSString x) | fromJSString x == "E" = Ok EmptyTransformedPattern
  readJSON (JSObject x) | firstKey x == "Text" = TextPatternChain <$> valFromObj "Text" x <*> valFromObj "b" x <*> valFromObj "c" x
  readJSON _ = Error "can't parse as TransformedPattern"

instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern (Combine sPat comb) EmptyTransformedPattern) = toParamPattern sPat
  toParamPattern (TransformedPattern t p) = applyPatternTransformer t (toParamPattern p)
  toParamPattern (UntransformedPattern u) = toParamPattern u
  toParamPattern (EmptyTransformedPattern) = Tidal.silence -- @ is this correct?
  toParamPattern (TextPatternChain a b c) = toParamPattern $ TransformedPattern (Combine a' Merge) $ TransformedPattern (Combine b' Merge) $ UntransformedPattern c'
    where a' = Sound (TextPattern a)
          b' = Up (TextPattern b)
          c' = Vowel (TextPattern c)
  isEmptyFuture (UntransformedPattern u) = isEmptyFuture u
  isEmptyFuture (TransformedPattern t p) = isEmptyFuture p
  isEmptyFuture (EmptyTransformedPattern) = True
  isEmptyFuture (TextPatternChain _ _ _) = False
  isEmptyPast (TransformedPattern t p) = isEmptyPast p
  isEmptyPast (UntransformedPattern u) = isEmptyPast u
  isEmptyPast (EmptyTransformedPattern) = True
  isEmptyPast (TextPatternChain _ _ _) = False

data StackedPatterns = StackedPatterns [TransformedPattern]

instance Show StackedPatterns where
  show (StackedPatterns xs) = "stack [" ++ (intercalate ", " (Prelude.map show xs)) ++ "]"

instance JSON StackedPatterns where
  showJSON (StackedPatterns xs) = encJSDict [("StackedPatterns",xs)]
  readJSON (JSObject x) | firstKey x == "StackedPatterns" = StackedPatterns <$> valFromObj "StackedPatterns" x
  readJSON _ = Error "can't parse as StackedPatterns"

instance ParamPatternable StackedPatterns where
  toParamPattern (StackedPatterns xs) = Tidal.stack $ Prelude.map toParamPattern $ Prelude.filter (not . isEmptyPast) xs
  isEmptyPast (StackedPatterns xs) = and $ fmap isEmptyPast xs
  isEmptyFuture (StackedPatterns xs) = and $ fmap isEmptyFuture xs
