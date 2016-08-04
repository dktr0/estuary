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

data Sample = Sample {
  sampleName::String,
  sampleN::Int,
  sampleRepeats::Int,
  sampleDegrade::Bool
  } deriving (Eq)

instance Show Sample where
  show (Sample x 0 1 False) = x
  show (Sample x y 1 False) = x++":"++(show y)
  show (Sample x y r False) = x++":"++(show y)++"*"++(show r)
  show (Sample x y 1 True)  = x++":"++(show y)++"?"
  show (Sample x y r True)  = x++":"++(show y)++"*"++(show r)++"?"

instance ParamPatternable Sample where
  toParamPattern = Tidal.sound . Tidal.p . show



newtype Sound = Sound (Maybe Sample) deriving (Eq)

instance Show Sound where
  show (Sound (Just s)) = show s
  show (Sound (Nothing)) = "~"

instance ParamPatternable Sound where
  toParamPattern (Sound (Just s)) = Tidal.sound (Tidal.p (show s))
  toParamPattern (Sound Nothing) = Tidal.sound (Tidal.p "")

silentSound = Sound (Nothing)

simpleSound :: String -> Sound
simpleSound x = Sound (Just (Sample x 0 1 False))



newtype SoundPattern = SoundPattern [Sound] deriving (Eq)

showSoundPattern :: SoundPattern -> String
showSoundPattern (SoundPattern xs) = intercalate " " (Prelude.map show xs)

instance Show SoundPattern where
  show x = "sound \"" ++ (showSoundPattern x) ++ "\""

instance ParamPatternable SoundPattern where
  toParamPattern = Tidal.sound . Tidal.p . showSoundPattern



data PatternTransformer = Rev | Slow Rational | Density Rational | Degrade | DegradeBy Double | Every Int PatternTransformer | Brak

instance Show PatternTransformer where
  show Rev = "rev"
  show (Slow f) = "slow " ++ (show f)
  show (Density f) = "density " ++ (show f)
  show Degrade = "degrade"
  show (DegradeBy f) = "degradeBy " ++ (show f)
  show (Every n t) = "every " ++ (show n) ++ "(" ++ show t ++ ")"
  show (Brak) = "brak"

applyPatternTransform :: PatternTransformer -> (Tidal.ParamPattern -> Tidal.ParamPattern)
applyPatternTransform Rev = Tidal.rev
applyPatternTransform (Slow f) = Tidal.slow f
applyPatternTransform (Density f) = Tidal.density f
applyPatternTransform Degrade = Tidal.degrade
applyPatternTransform (DegradeBy d) = Tidal.degradeBy d
applyPatternTransform (Every n t) = Tidal.every n (applyPatternTransform t)
applyPatternTransform (Brak) = Tidal.brak

-- PatternTransformer is not an instance of ParamPatternable because a pattern transformer is not sufficient to make a ParamPattern



data TransformedPattern = TransformedPattern [PatternTransformer] SoundPattern

instance Show TransformedPattern where
  show (TransformedPattern ts x) = (intercalate " $ " (Prelude.map show ts))  ++ " $ " ++ (show x)

instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern ts x) = Prelude.foldr (\a b -> (applyPatternTransform a) b) (toParamPattern x) ts
