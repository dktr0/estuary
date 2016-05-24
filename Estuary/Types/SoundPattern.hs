module Types.SoundPattern where

import Types.Sound
import Data.Map as Map

data SoundPattern = SoundPattern (Map Int Sound) deriving (Eq)

-- Rewrite
--instance Show SoundPattern where
  --show (SoundPattern xs) = intercalate " " (map show xs)

initialContainer :: SoundPattern
initialContainer = simple "sn"

insert :: (Int,Int,Sound) -> SoundPattern -> SoundPattern
insert (dkey,ikey,sound) (SoundPattern pattern) = if Map.member (ikey+1) pattern
                                                  then do
                                                       let upattern = if (dkey == ikey) then pattern else Map.delete dkey pattern
                                                       let (lp,gp) = Map.partitionWithKey (\ k _ -> k > (ikey+1)) upattern
                                                       let gpp = Map.mapKeys (+1) gp
                                                       let lpp = Map.insert ((+1) $ fst $ Map.findMax lp) sound pattern
                                                       SoundPattern (Map.union lpp gpp)
                                                  else SoundPattern (Map.insert (ikey+1) sound pattern)

delete :: Int -> SoundPattern -> SoundPattern
delete key (SoundPattern pattern) = SoundPattern (Map.delete key pattern)

-- convenience functions
empty :: SoundPattern
empty = SoundPattern Map.empty

simple :: String -> SoundPattern
simple x = SoundPattern (Map.singleton 1 (simpleSound x))
