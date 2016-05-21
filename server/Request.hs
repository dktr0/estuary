module Request where

import Text.JSON
import Control.Applicative
import Control.Monad

data Request = Cps Double | Hush | Pattern Int String | Info String

instance Show Request where
  show (Cps x) = "cps " ++ (show x)
  show (Hush) = "hush"
  show (Pattern n p) = "d" ++ (show n) ++ " $ " ++ p
  show (Info x) = "info: " ++ x

instance JSON Request where
  showJSON (Cps x) = encJSDict [("cps",x)]
  showJSON (Hush) = encJSDict [("hush","hush")]
  showJSON (Pattern n p) = encJSDict [("d"++(show n),p)]
  showJSON (Info x) = encJSDict [("info",x)]

  readJSON (JSObject x) | (firstKeyIs "cps" x) = Cps <$> (valFromObj "cps" x)
  readJSON (JSObject x) | (firstKeyIs "hush" x) = Ok Hush
  readJSON (JSObject x) | (firstKeyIs "d1" x) = Pattern 1 <$> (valFromObj "d1" x)
  readJSON (JSObject x) | (firstKeyIs "info" x) = Info <$> (valFromObj "info" x)
  readJSON _ = Error "First key must be cps, hush, or d1-9"

firstKeyIs :: String -> JSObject JSValue -> Bool
firstKeyIs key = f . fromJSObject
 where f ((x,_):_) = x==key
       f _ = False
