module Types.Tidal where

import Data.List as List (intercalate, zip)
import Data.Map as Map

-- This module defines types that model elements of the notation employed in the Tidal language
-- including both notations within Tidal's pattern notation (i.e. within "quotation marks") as well
-- as a range of Haskell notations and definitions provided by the Tidal modules.
-- Each type implements the class Show in a way that produces valid Tidal/Haskell code.
-- And each type implements the class ParamPatternable which specifies the function
-- toParamPattern :: a -> ParamPattern (where ParamPattern is the type commonly given
-- as an argument to stream-generating computations in Tidal).

class ParamPatternable a where
  toParamPattern :: a -> ParamPattern

data Sample = Sample {
  sampleName::String,
  sampleN::Int,
  sampleRepeats::Int,
  sampleDegrade::Bool
  } deriving (Eq)

instance Show Sample where
  show (Sample (x,y,1,False))) = x++":"++(show y)
  show (Sample (x,y,r,False))) = x++":"++(show y)++"*"++(show r)
  show (Sample (x,y,1,True)))  = x++":"++(show y)++"?"
  show (Sample (x,y,r,True)))  = x++":"++(show y)++"*"++(show r)++"?"

instance ParamPatternable Sample where
  toParamPattern = p . show



newtype Sound = Sound (Maybe Sample) deriving (Eq)

instance Show Sound where
  show (Sound (Just s)) = show s
  show (Sound (Nothing)) = "~"

instance ParamPatternable Sound where
  toParamPattern (Sound (Just s)) = p (show s)
  toParamPattern (Sound Nothing) = p ""

silentSound = Sound (Nothing)

simpleSound :: String -> Sound
simpleSound x = Sound (Just (x,0,1,False))



newtype SoundPattern = SoundPattern [Sound] deriving (Eq)

instance Show SoundPattern where
  show (SoundPattern xs) = intercalate " " (Prelude.map show xs)

instance ParamPatternable where
  toParamPattern = p . show



data PatternTransformer = Degrade | Slow Float

instance Show PatternTransformer where
  show Degrade = "degrade"
  show (Slow f) = "slow " ++ (show f)

applyPatternTransform :: PatternTransformer -> (ParamPattern -> ParamPattern)
applyPatternTransform Degrade = Tidal.degrade
applyPatternTransform (Slow f) = Tidal.slow f


data TransformedPattern = TransformedPattern [PatternTransformer] SoundPattern

foldr :: (a -> b -> b) -> b -> [a] -> b

instance Show TransformedPattern where
  show (TransformedPattern ts x) = (intercalate " $ " (Prelude.map show ts))  ++ " $ " ++ (show x)
