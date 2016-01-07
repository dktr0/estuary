module Main where

import Reflex
import Reflex.Dom
import Sound.Tidal.Context as Tidal
import Sound.Tidal.Utils (fst',snd',thd')
{-import Estuary.Tidal.Util-}
import Data.Map
import Sound.OSC.Type
import Safe (readMay)

showEventArc :: Parseable a => Tidal.Event a -> String
showEventArc x = (show (fst' x)) ++ " " ++ (show (snd' x))

extractSample :: Tidal.Event OscMap -> String
extractSample x = f mm
 where f (Just (Just y)) = ascii_to_string $ d_ascii_string y
       mm = Data.Map.lookup (S "sound" Nothing) (thd' x)

takeArc :: String -> [Tidal.Event OscMap]
takeArc x = arc (sound (p x)) (0, 1)

showSoundPattern :: String -> String
showSoundPattern x = (intercalate "," (Prelude.map extractSample $ takeArc x) )

test = Prelude.map extractSample $ takeArc "bd cp"

main = mainWidget $ el "div" $ do
  input <- textInput def
  result <- forDyn (_textInput_value input) showSoundPattern
  dynText result

patternInput :: MonadWidget t m => m (Dynamic t (Maybe String))
patternInput = do
  n <- textInput $ def & textInputConfig_initialValue .~ "bd,bp"
  mapDyn readMay $ _textInput_value n
