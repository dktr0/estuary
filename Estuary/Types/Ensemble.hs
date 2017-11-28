module Estuary.Types.Ensemble where

import qualified Data.Map.Strict as Map

import Estuary.Types.Definition
import Estuary.Types.View
import Data.Time.Clock
import Data.Time.Calendar
import Sound.Tidal.Tempo (Tempo(..))

data Ensemble = Ensemble {
  password :: String,
  defs :: Map.Map Int Definition,
  views :: Map.Map String View,
  defaultView :: View,
  tempo :: Tempo -- {at :: UTCTime, beat :: Double, cps :: Double, paused :: Bool, clockLatency :: Double}
  }

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
