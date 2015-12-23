module Estuary.Tidal.Utils where

import Sound.Tidal.Context
import Sound.Tidal.Utils (fst',snd',thd')
import Data.Map
import Sound.OSC.Type

showEventArc :: Parseable a => Event a -> String
showEventArc x = (show (fst' x)) ++ " " ++ (show (snd' x))

extractSample :: Event OscMap -> String
extractSample x = f mm
 where f (Just (Just y)) = ascii_to_string $ d_ascii_string y
       mm = Data.Map.lookup (S "sound" Nothing) (thd' x)
