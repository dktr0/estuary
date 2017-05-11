 module Estuary.Tidal.Types where

import Text.JSON
import Data.List as List (intercalate, zip)
import Data.Map as Map
import Data.Ratio
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
  isEmptyFuture :: a -> Bool
  isEmptyPast :: a -> Bool


data RepOrDiv = Once | Rep Int | Div Int deriving (Eq)

instance Show RepOrDiv where
  show Once = ""
  show (Rep 1) = ""
  show (Rep n) = "*" ++ (show n)
  show (Div 1) = ""
  show (Div n) = "/" ++ (show n)

instance JSON RepOrDiv where
  showJSON Once = JSNull
  showJSON (Rep n) = encJSDict [("Rep",n)]
  showJSON (Div n) = encJSDict [("Div",n)]
  readJSON (JSNull) = Ok (Once)
  readJSON (JSObject x) | firstKey x == "Rep" = valFromObj "Rep" x
  readJSON (JSObject x) | firstKey x == "Div" = valFromObj "Rep" x
  readJSON _ = Error "can't parse as RepOrDiv"

firstKey :: JSObject JSValue -> String
firstKey = fst . head . fromJSObject

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
  showJSON (Potential a) = encJSDict [("Potential",a)]
  showJSON PotentialDelete = showJSON "PotentialDelete"
  showJSON PotentialMakeGroup = showJSON "PotentialMakeGroup"
  showJSON PotentialMakeLayer = showJSON "PotentialMakeLayer"
  showJSON (PotentialLiveness x) = encJSDict [("PotentialLiveness",showJSON x)]
  showJSON Inert = showJSON "Inert"
  showJSON PotentialRepOrDiv = showJSON "PotentialRepOrDiv"
  showJSON (Potentials xs) = encJSDict [("Potentials",xs)]
  readJSON (JSObject x) | firstKey x == "Potential" = Potential <$> valFromObj "Potential" x
  readJSON (JSString x) | fromJSString x == "PotentialDelete" = Ok PotentialDelete
  readJSON (JSString x) | fromJSString x == "PotentialMakeGroup" = Ok PotentialMakeGroup
  readJSON (JSString x) | fromJSString x == "PotentialMakeLayer" = Ok PotentialMakeLayer
  readJSON (JSObject x) | firstKey x == "PotentialLiveness" = PotentialLiveness <$> valFromObj "PotentialLiveness" x
  readJSON (JSString x) | fromJSString x == "Inert" = Ok Inert
  readJSON (JSString x) | fromJSString x == "PotentialRepOrDiv" = Ok PotentialRepOrDiv
  readJSON (JSObject x) | firstKey x == "Potentials" = Potentials <$> valFromObj "Potentials" x
  readJSON _ = Error "can't parse as Potential"


data Live a = Live a Liveness | Edited a a deriving(Eq)

instance JSON a => JSON (Live a) where
  showJSON (Live a liveness) = encJSDict [("Live",showJSON a),("Liveness",showJSON liveness)]
  showJSON (Edited past future) = encJSDict [("Edited",past),("future",future)]
  readJSON (JSObject x) | firstKey x == "Live" = Live <$> valFromObj "Live" x <*> valFromObj "Liveness" x
  readJSON (JSObject x) | firstKey x == "Edited" = Edited <$> valFromObj "Edited" x <*> valFromObj "future" x
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

-- example: an initial pattern...
-- Group (Live ([Atom "bd" Inert,Atom "sn" Inert,Atom "bd" Inert],Once) L4)
-- user clicks in whitespace to bring up a potential change (bring ups pop up menu in whitespace)...
-- Group (Live ([Atom "bd" Inert,Blank (Potentials [Potential "~",PotentialLiveness L3,Inert]), Atom "sn" Inert,Atom "bd" Inert],Once) L4)
-- user selects change to L3...
-- Group (Live ([Atom "bd" Inert,Atom "sn" Inert,Atom "bd" Inert],Once) L3)
-- user clicks on "sn" intending to delete it (brings up pop up menu on sn)...
-- Group (Edited ([Atom "bd" Inert,Atom "sn" Inert,Atom "bd" Inert],Once) ([Atom "bd" Inert,Atom "sn" (Potentials [Potential "arpy",DeleteMe,Inert]), Atom "bd" Inert],Once))
-- after clicking on delete button
-- Group (Edited ([Atom "bd" Inert,Atom "sn" Inert,Atom "bd" Inert],Once) ([Atom "bd" Inert,Atom "bd" Inert],Once))
-- after clicking eval button a moment later...
-- Group (Live [Atom "bd",Atom "bd"] L3)

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
  show (TextPattern x) = x

instance JSON a => JSON (GeneralPattern a) where
  showJSON (Atom a p r) = encJSDict [("Atom",showJSON a),("p",showJSON p),("r",showJSON r)]
  showJSON (Blank p) = encJSDict [("Blank",showJSON p)]
  showJSON (Group x p) = encJSDict [("Group",showJSON x),("p",showJSON p)]
  showJSON (Layers x p) = encJSDict [("Layers",showJSON x),("p",showJSON p)]
  showJSON (TextPattern t) = encJSDict [("TextPattern",showJSON t)]
  readJSON (JSObject x) | firstKey x == "Atom" = Atom <$> valFromObj "Atom" x <*> valFromObj "p" x <*> valFromObj "r" x
  readJSON (JSObject x) | firstKey x == "Blank" = Blank <$> valFromObj "Blank" x
  readJSON (JSObject x) | firstKey x == "Group" = Group <$> valFromObj "Group" x <*> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "Layers" = Layers <$> valFromObj "Layers" x <*> valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "TextPattern" = TextPattern <$> valFromObj "TextPattern" x


type SampleName = String

newtype Sample = Sample (SampleName,Int) deriving (Eq)

instance JSON Sample where
  showJSON (Sample (x,y)) = encJSDict [("Sample",showJSON x),("n",showJSON y)]
  readJSON (JSObject x) | firstKey x == "Sample" = (\a b -> Sample (a,b)) <$> c <*> d
    where c = valFromObj "Sample" x
          d = valFromObj "n" x
  readJSON _ = Error "can't parse as Sample"

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
  showJSON (Accelerate x) = encJSDict [("Accelerate",x)]
  showJSON (Bandf x) = encJSDict [("Bandf",x)]
  showJSON (Bandq x) = encJSDict [("Bandq",x)]
  showJSON (Begin x) = encJSDict [("Begin",x)]
  showJSON (Coarse x) = encJSDict [("Coarse",x)]
  showJSON (Crush x) = encJSDict [("Crush",x)]
  showJSON (Cut x) = encJSDict [("Cut",x)]
  showJSON (Cutoff x) = encJSDict [("Cutoff",x)]
  showJSON (Delay x) = encJSDict [("Delay",x)]
  showJSON (Delayfeedback x) = encJSDict [("Delayfeedback",x)]
  showJSON (Delaytime x) = encJSDict [("Delaytime",x)]
  showJSON (End x) = encJSDict [("End",x)]
  showJSON (Gain x) = encJSDict [("Gain",x)]
  showJSON (Hcutoff x) = encJSDict [("Hcutoff",x)]
  showJSON (Hresonance x) = encJSDict [("Hresonance",x)]
  showJSON (Loop x) = encJSDict [("Loop",x)]
  showJSON (N x) = encJSDict [("N",x)]
  showJSON (Pan x) = encJSDict [("Pan",x)]
  showJSON (Resonance x) = encJSDict [("Resonance",x)]
  showJSON (S x) = encJSDict [("S",x)]
  showJSON (Shape x) = encJSDict [("Shape",x)]
  showJSON (Sound x) = encJSDict [("Sound",x)]
  showJSON (Speed x) = encJSDict [("Speed",x)]
  showJSON (Unit x) = encJSDict [("Unit",x)]
  showJSON (Up x) = encJSDict [("Up",x)]
  showJSON (Vowel x) = encJSDict [("Vowel",x)]
  readJSON (JSObject x) | firstKey x == "Accelerate" = Accelerate <$> valFromObj "Accelerate" x
  readJSON (JSObject x) | firstKey x == "Bandf" = Bandf <$> valFromObj "Bandf" x
  readJSON (JSObject x) | firstKey x == "Bandq" = Bandq <$> valFromObj "Bandq" x
  readJSON (JSObject x) | firstKey x == "Begin" = Begin <$> valFromObj "Begin" x
  readJSON (JSObject x) | firstKey x == "Coarse" = Coarse <$> valFromObj "Coarse" x
  readJSON (JSObject x) | firstKey x == "Crush" = Crush <$> valFromObj "Crush" x
  readJSON (JSObject x) | firstKey x == "Cut" = Cut <$> valFromObj "Cut" x
  readJSON (JSObject x) | firstKey x == "Cutoff" = Cutoff <$> valFromObj "Cutoff" x
  readJSON (JSObject x) | firstKey x == "Delay" = Delay <$> valFromObj "Delay" x
  readJSON (JSObject x) | firstKey x == "Delayfeedback" = Delayfeedback <$> valFromObj "Delayfeedback" x
  readJSON (JSObject x) | firstKey x == "Delaytime" = Delaytime <$> valFromObj "Delaytime" x
  readJSON (JSObject x) | firstKey x == "End" = End <$> valFromObj "End" x
  readJSON (JSObject x) | firstKey x == "Gain" = Gain <$> valFromObj "Gain" x
  readJSON (JSObject x) | firstKey x == "Hcutoff" = Hcutoff <$> valFromObj "Hcutoff" x
  readJSON (JSObject x) | firstKey x == "Hresonance" = Hresonance <$> valFromObj "Hresonance" x
  readJSON (JSObject x) | firstKey x == "Loop" = Loop <$> valFromObj "Loop" x
  readJSON (JSObject x) | firstKey x == "N" = N <$> valFromObj "N" x
  readJSON (JSObject x) | firstKey x == "Pan" = Pan <$> valFromObj "Pan" x
  readJSON (JSObject x) | firstKey x == "Resonance" = Resonance <$> valFromObj "Resonance" x
  readJSON (JSObject x) | firstKey x == "S" = S <$> valFromObj "S" x
  readJSON (JSObject x) | firstKey x == "Shape" = Shape <$> valFromObj "Shape" x
  readJSON (JSObject x) | firstKey x == "Sound" = Sound <$> valFromObj "Sound" x
  readJSON (JSObject x) | firstKey x == "Speed" = Speed <$> valFromObj "Speed" x
  readJSON (JSObject x) | firstKey x == "Unit" = Unit <$> valFromObj "Unit" x
  readJSON (JSObject x) | firstKey x == "Up" = Up <$> valFromObj "Up" x
  readJSON (JSObject x) | firstKey x == "Vowel" = Vowel <$> valFromObj "Vowel" x
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
  showJSON Merge = showJSON "Merge"
  showJSON Add = showJSON "Add"
  showJSON Subtract = showJSON "Subtract"
  showJSON Multiply = showJSON "Multiply"
  showJSON Divide = showJSON "Divide"
  readJSON (JSString x) | fromJSString x == "Merge" = Ok Merge
  readJSON (JSString x) | fromJSString x == "Add" = Ok Add
  readJSON (JSString x) | fromJSString x == "Subtract" = Ok Subtract
  readJSON (JSString x) | fromJSString x == "Multiply" = Ok Multiply
  readJSON (JSString x) | fromJSString x == "Divid" = Ok Divide
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
  showJSON NoTransformer = showJSON "NoTransformer"
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
  readJSON (JSString x) | fromJSString x == "NoTransformer" = Ok NoTransformer
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
applyPatternTransformer (Slow f) = Tidal.slow f
applyPatternTransformer (Density f) = Tidal.density f
applyPatternTransformer Degrade = Tidal.degrade
applyPatternTransformer (DegradeBy d) = Tidal.degradeBy d
applyPatternTransformer (Every n t) = Tidal.every n (applyPatternTransformer t)
applyPatternTransformer (Brak) = Tidal.brak
applyPatternTransformer (Jux t) = Tidal.jux (applyPatternTransformer t)
applyPatternTransformer (Chop t) = Tidal.chop t
applyPatternTransformer (Combine p c) =  (toTidalCombinator c) $ toParamPattern p


data TransformedPattern = TransformedPattern PatternTransformer TransformedPattern | UntransformedPattern SpecificPattern | EmptyTransformedPattern deriving (Eq)

--deleteHeadByReplacingWithChildren :: TransformedPattern -> TransformedPattern
--deleteHeadByReplacingWithChildren (TransformedPattern p t) = t
--deleteHeadByReplacingWithChildren (UntransformedPattern s) = EmptyTransformedPattern


instance Show TransformedPattern where
  show (TransformedPattern t p) = (show t) ++ " " ++ (show p)
  show (UntransformedPattern u) = (show u)
  show (EmptyTransformedPattern) = ""

instance JSON TransformedPattern where
  showJSON (TransformedPattern t p) = encJSDict [("TransformedPattern",showJSON t),("p",showJSON p)]
  showJSON (UntransformedPattern s) = encJSDict [("UntransformedPattern",showJSON s)]
  showJSON (EmptyTransformedPattern) = showJSON "EmptyTransformedPattern"
  readJSON (JSObject x) | firstKey x == "TransformedPattern" = TransformedPattern <$> valFromObj "TransformedPattern" x <*>  valFromObj "p" x
  readJSON (JSObject x) | firstKey x == "UntransformedPattern" = UntransformedPattern <$> valFromObj "UntransformedPattern" x
  readJSON (JSString x) | fromJSString x == "EmptyTransformedPattern" = Ok EmptyTransformedPattern
  readJSON _ = Error "can't parse as TransformedPattern"

instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern (Combine sPat comb) EmptyTransformedPattern) = toParamPattern sPat
  toParamPattern (TransformedPattern t p) = applyPatternTransformer t (toParamPattern p)
  toParamPattern (UntransformedPattern u) = toParamPattern u
  toParamPattern (EmptyTransformedPattern) = Tidal.silence -- @ is this correct?
  isEmptyFuture (UntransformedPattern u) = isEmptyFuture u
  isEmptyFuture (TransformedPattern t p) = isEmptyFuture p
  isEmptyFuture (EmptyTransformedPattern) = True
  isEmptyPast (TransformedPattern t p) = isEmptyPast p
  isEmptyPast (UntransformedPattern u) = isEmptyPast u
  isEmptyPast (EmptyTransformedPattern) = True


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
