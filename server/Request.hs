module Request where

import Text.JSON
import Control.Applicative
import Control.Monad

data Request = Cps Double |
               Hush |
               Pattern Int String |
               Info String |
               Render String Double Double

instance Show Request where
  show (Cps x) = "cps " ++ (show x)
  show (Hush) = "hush"
  show (Pattern n p) = "d" ++ (show n) ++ " $ " ++ p
  show (Info x) = "info: " ++ x
  show (Render patt cps cycles) = "render (cps=" ++ (show cps) ++ "; cycles=" ++ (show cycles) ++ "): " ++ patt

instance JSON Request where
  showJSON (Cps x) = encJSDict [("cps",x)]
  showJSON (Hush) = encJSDict [("hush","hush")]
  showJSON (Pattern n p) = encJSDict [("d"++(show n),p)]
  showJSON (Info x) = encJSDict [("info",x)]
  showJSON (Render patt cps cycles) = encJSDict [("render",patt),("cps",show cps),("cycles",show cycles)]

  readJSON (JSObject x) | (firstKeyIs "cps" x) = Cps <$> (valFromObj "cps" x)
  readJSON (JSObject x) | (firstKeyIs "hush" x) = Ok Hush
  readJSON (JSObject x) | (firstKeyIs "d1" x) = Pattern 1 <$> (valFromObj "d1" x)
  readJSON (JSObject x) | (firstKeyIs "d2" x) = Pattern 2 <$> (valFromObj "d2" x)
  readJSON (JSObject x) | (firstKeyIs "d3" x) = Pattern 3 <$> (valFromObj "d3" x)
  readJSON (JSObject x) | (firstKeyIs "d4" x) = Pattern 4 <$> (valFromObj "d4" x)
  readJSON (JSObject x) | (firstKeyIs "d5" x) = Pattern 5 <$> (valFromObj "d5" x)
  readJSON (JSObject x) | (firstKeyIs "d6" x) = Pattern 6 <$> (valFromObj "d6" x)
  readJSON (JSObject x) | (firstKeyIs "d7" x) = Pattern 7 <$> (valFromObj "d7" x)
  readJSON (JSObject x) | (firstKeyIs "d8" x) = Pattern 8 <$> (valFromObj "d8" x)
  readJSON (JSObject x) | (firstKeyIs "d9" x) = Pattern 9 <$> (valFromObj "d9" x)
  readJSON (JSObject x) | (firstKeyIs "info" x) = Info <$> (valFromObj "info" x)
  readJSON (JSObject x) | (firstKeyIs "render" x) = Render <$> (valFromObj "render" x) <*> (valFromObj "cps" x) <*> (valFromObj "cycles" x)
  readJSON _ = Error "First key must be cps, hush, info or d1-9"

firstKeyIs :: String -> JSObject JSValue -> Bool
firstKeyIs key = f . fromJSObject
 where f ((x,_):_) = x==key
       f _ = False
