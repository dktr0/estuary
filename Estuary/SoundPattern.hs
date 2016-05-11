module SoundPattern where

import Sound
import Data.List (intercalate)

data SoundPattern = SoundPattern [Sound] deriving (Eq)

instance Show SoundPattern where
  show (SoundPattern xs) = intercalate " " (map show xs)

emptyPattern :: SoundPattern
emptyPattern = SoundPattern []

insertIntoPatternAt :: Sound -> SoundPattern -> Int -> SoundPattern
insertIntoPatternAt sound (SoundPattern pattern) position =  SoundPattern ((take position pattern) ++ [sound] ++ (drop position pattern))
