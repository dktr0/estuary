module Estuary.Utility where

import Text.JSON 

-- the function lastOrNothing is useful with fmapMaybe on events containing lists of EstuaryProtocol items
-- so that no event is fired when the list is empty

lastOrNothing :: [a] -> Maybe a
lastOrNothing [] = Nothing
lastOrNothing xs = Just (last xs)


-- firstKey is useful when writing instance definitions for the class JSON 
-- it extracts the first key from a JavaScript object which can then be used in pattern matching

firstKey :: JSObject JSValue -> String
firstKey = fst . head . fromJSObject

