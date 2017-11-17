module Estuary.Languages.CQenze where

import qualified Sound.Tidal.Context as Tidal

cqenzeShow :: String -> String
cqenzeShow x = x

cqenzeParamPattern :: String -> Tidal.ParamPattern
cqenzeParamPattern x = Tidal.s $ Tidal.p x
