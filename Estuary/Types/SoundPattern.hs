module SoundPattern where

import Sound
import Data.List (intercalate)

data SoundPattern = SoundPattern [Sound] deriving (Eq)

instance Show SoundPattern where
  show (SoundPattern xs) = intercalate " " (map show xs)
