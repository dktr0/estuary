module Types.SoundPattern where

import Types.Sound
import Data.List as List (intercalate, zip)
import Data.Map as Map

data SoundPattern = SoundPattern [Sound] deriving (Eq)

instance Show SoundPattern where
  show (SoundPattern xs) = intercalate " " (Prelude.map show xs)

initialPattern :: SoundPattern
initialPattern = simple "sn"

-- Get SoundEvent as well as new position
-- When it is an insert event, insert the element, and change the sound at the
-- Current position to Nothing/silentSound
insert :: Maybe (Sound,Int) -> SoundPattern -> SoundPattern
insert (Just(sound,position)) (SoundPattern pattern) =  SoundPattern ((take position pattern) ++ [sound] ++ (drop position pattern))
insert (Nothing) (SoundPattern pattern) = (SoundPattern pattern)

delete :: Int -> SoundPattern -> SoundPattern
delete position (SoundPattern pattern) = SoundPattern ((take position pattern) ++ (drop (position+1) pattern))

-- convenience functions

empty :: SoundPattern
empty = SoundPattern []

simple :: String -> SoundPattern
simple x = SoundPattern [simpleSound x]

update :: Maybe (Sound,Int) -> SoundPattern -> SoundPattern
update (Just(sound,position)) (SoundPattern pattern) = SoundPattern ((take position pattern) ++ [sound] ++ (drop (position+1) pattern))
update (Nothing) (SoundPattern pattern) = SoundPattern (pattern)

convertToMapMaybe :: SoundPattern -> Map Int (Maybe Sound)
convertToMapMaybe (SoundPattern pattern) = Map.fromList (List.zip [0..] $ (Prelude.map Just) pattern)

convertToMap :: SoundPattern -> Map Int Sound
convertToMap (SoundPattern pattern) = Map.fromList (List.zip [0..] pattern)
