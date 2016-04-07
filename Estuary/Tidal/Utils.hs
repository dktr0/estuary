module Tidal.Utils where

import Sound.Tidal.Context
import Sound.Tidal.Utils (fst',snd',thd')
import Data.Map (Map, lookup)
import Sound.OSC.Type

rationalToDouble :: Rational -> Double
rationalToDouble r = (fromIntegral (numerator r)) / (fromIntegral (denominator r))

showEventArc :: Event a -> String
showEventArc x = (show (fst' x)) -- ++ " " ++ (show (snd' x))

getEventArc :: Event a -> (Double, Double)
getEventArc x =  (rationalToDouble $ fst (fst' x), rationalToDouble $ snd (fst' x))

extractArcs :: String -> [(Double,Double)]
extractArcs pat = (Prelude.map getEventArc $ takeArc pat)

extractSample :: Event OscMap -> String
extractSample x = f mm
 where f (Just (Just y)) = ascii_to_string $ d_ascii_string y
       mm = Data.Map.lookup (S "sound" Nothing) (thd' x)

takeArc :: String -> [Event OscMap]
takeArc x = arc (sound (p x)) (0, 1)

showSoundPattern :: String -> String
showSoundPattern x = (intercalate "," (Prelude.map extractSample $ takeArc x) )

showAllArcs :: String -> [String]
showAllArcs pat = (Prelude.map showEventArc $ takeArc pat)
