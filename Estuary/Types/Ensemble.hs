module Estuary.Types.Ensemble where

import qualified Data.Map.Strict as Map

import Estuary.Types.Definition
import Estuary.Types.View
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Sound.Tidal.Tempo (Tempo(..))
import Text.JSON
import Text.Read

import Data.Ratio

data Ensemble = Ensemble {
  password :: String,
  defs :: Map.Map Int Definition,
  views :: Map.Map String View,
  defaultView :: View,
  tempo :: Tempo -- {at :: UTCTime, beat :: Double, cps :: Double, paused :: Bool, clockLatency :: Double}
  }

instance JSON Tempo where
  showJSON (Tempo at' beat' cps' paused' clockLatency') =
    encJSDict [
      ("at",showJSON at'),
      ("beat",showJSON beat'),
      ("cps",showJSON cps'),
      ("paused",showJSON paused'),
      ("clockLatency",showJSON clockLatency')
      ]
  readJSON (JSObject x) = Tempo <$> valFromObj "at" x <*> valFromObj "beat" x <*> valFromObj "cps" x <*> valFromObj "paused" x <*> valFromObj "clockLatency" x

instance JSON UTCTime where
  showJSON x = encJSDict [("n",showJSON $ numerator r),("d",showJSON $ denominator r)]
    where r = (realToFrac (utcTimeToPOSIXSeconds x) :: Rational)
  readJSON (JSObject x) = f <$> valFromObj "n" x <*> valFromObj "d" x
    where f n d = posixSecondsToUTCTime $ fromRational $ n % d

instance JSON Ensemble where
  showJSON (Ensemble pwd' defs' views' defaultView' tempo') =
    encJSDict [
      ("password",showJSON pwd'),
      ("defs",showJSON defs'),
      ("views",showJSON views'),
      ("defaultView",showJSON defaultView'),
      ("tempo",showJSON tempo')
      ]
  readJSON (JSObject x) = Ensemble <$> valFromObj "password" x <*> valFromObj "defs" x <*> valFromObj "views" x <*> valFromObj "defaultView" x <*> valFromObj "tempo" x



emptyEnsemble :: Ensemble
emptyEnsemble = Ensemble {
  password = "",
  defs = Map.empty,
  views = Map.empty,
  defaultView = standardView,
  tempo = Tempo { at=UTCTime (ModifiedJulianDay 0) (fromInteger 0), beat=0.0, cps=0.5, paused=False, clockLatency=0.0 }
  }

setPassword :: String -> Ensemble -> Ensemble
setPassword s e = e { password = s }

editDef :: Int -> Definition -> Ensemble -> Ensemble
editDef z d s = s { defs = Map.insert z d (defs s) }

editDefaultView :: View -> Ensemble -> Ensemble
editDefaultView v s = s { defaultView = v }

editView :: String -> View -> Ensemble -> Ensemble
editView w v s = s { views = Map.insert w v (views s) }

deleteView :: String -> Ensemble -> Ensemble
deleteView v e = e { views = Map.delete v (views e) }

tempoChange :: UTCTime -> Double -> Ensemble -> Ensemble
tempoChange t f e = e { tempo = newTempo }
  where
    newTempo = (tempo e) { at = t, beat = b, cps = f }
    delta = realToFrac $ diffUTCTime t (at (tempo e))
    b = (delta * (cps (tempo e))) + (beat (tempo e))
