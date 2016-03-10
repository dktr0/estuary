module Tidal.Utils where

import Sound.Tidal.Context
import Sound.Tidal.Utils (fst',snd',thd')
import Data.Map (Map, lookup)
import Sound.OSC.Type

showEventArc :: Event a -> String
showEventArc x = (show (fst' x)) ++ " " ++ (show (snd' x))

extractSample :: Event OscMap -> String
extractSample x = f mm
 where f (Just (Just y)) = ascii_to_string $ d_ascii_string y
       mm = Data.Map.lookup (S "sound" Nothing) (thd' x)

takeArc :: String -> [Event OscMap]
takeArc x = arc (sound (p x)) (0, 1)

showSoundPattern :: String -> String
showSoundPattern x = (intercalate "," (Prelude.map extractSample $ takeArc x) )

extractArcs :: String -> [String]
extractArcs pat = (Prelude.map showEventArc $ takeArc pat)
