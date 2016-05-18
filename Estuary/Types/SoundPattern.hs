module Types.SoundPattern where

import Types.Sound
import Data.List (intercalate)

data SoundPattern = SoundPattern [Sound] deriving (Eq)

instance Show SoundPattern where
  show (SoundPattern xs) = intercalate " " (map show xs)

initialContainer :: SoundPattern
initialContainer = simple "sn"

insert :: (Sound,Int) -> SoundPattern -> SoundPattern
insert (sound,position) (SoundPattern pattern) =  SoundPattern ((take position pattern) ++ [sound] ++ (drop position pattern))

delete :: Int -> SoundPattern -> SoundPattern
delete position (SoundPattern pattern) = SoundPattern ((take position pattern) ++ (drop (position+1) pattern))

-- convenience functions

empty :: SoundPattern
empty = SoundPattern []

simple :: String -> SoundPattern
simple x = SoundPattern [simpleSound x]
