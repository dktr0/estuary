module Main where

import Reflex
import Reflex.Dom
import Sound.Tidal.Context as Tidal
import Sound.Tidal.Utils (fst',snd',thd')
import Data.Map
import Sound.OSC.Type

showEventArc :: Parseable a => Tidal.Event a -> String
showEventArc x = (show (fst' x)) ++ " " ++ (show (snd' x))

extractSample :: Tidal.Event OscMap -> String
extractSample x = f mm
 where f (Just (Just y)) = ascii_to_string $ d_ascii_string y
       mm = Data.Map.lookup (S "sound" Nothing) (thd' x)

test = Prelude.map extractSample $ arc (sound (p "bd cp")) (0,1)

main = mainWidget $ el "div" $ text (intercalate "," test)
