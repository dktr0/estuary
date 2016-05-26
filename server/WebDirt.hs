module WebDirt where

import Text.JSON
import Control.Applicative
import Control.Monad
import Sound.Tidal.Context
import qualified Data.Map as Map

-- as defined in Sound.Tidal.Stream:
-- type ParamMap = Map.Map Param (Maybe Value)
-- data Param = S {name :: String, sDefault :: Maybe String} | F {name :: String, fDefault :: Maybe Double} | I {name :: String, iDefault :: Maybe Int}
-- data Value = VS { svalue :: String } | VF { fvalue :: Double } | VI { ivalue :: Int } deriving (Show,Eq,Ord)

data WebDirtEvent = WebDirtEvent Double ParamMap

instance JSON WebDirtEvent where
    readJSON _ = Error "Reading JSON as WebDirtEvent not supported"
    showJSON (WebDirtEvent t ps) = makeObj (t':Map.elems (Map.mapWithKey (f) ps))
      where t' = ("when",showJSON t)
            f (S name _) (Just (VS x)) = (name,showJSON x)
            f (F name _) (Just (VF x)) = (name,showJSON x)
            f (I name _) (Just (VI x)) = (name,showJSON x)

render :: ParamPattern -> Double -> Double -> [WebDirtEvent]
render patt cps cycles = map f (arc patt (0,(approxRational cycles 0.001)))
  where f ((a,b),(c,d),params) = WebDirtEvent ((fromRational a)/cps) params
