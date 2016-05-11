module SoundPattern where

import Sound
import Data.List (intercalate)

data SoundPattern = SoundPattern [Sound] deriving (Eq)

instance Show SoundPattern where
  show (SoundPattern xs) = intercalate " " (map show xs)

insert :: SoundPattern -> Sound -> Int -> SoundPattern
insert (SoundPattern pattern) sound position =  SoundPattern ((take position pattern) ++ [sound] ++ (drop position pattern))

delete :: SoundPattern -> Int -> SoundPattern
delete (SoundPattern pattern) position = SoundPattern ((take position pattern) ++ (drop (position+1) pattern))


-- convenience functions

empty :: SoundPattern
empty = SoundPattern []

simple :: String -> SoundPattern
simple x = SoundPattern [simpleSound x]
