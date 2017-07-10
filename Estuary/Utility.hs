module Estuary.Utility where

-- the function lastOrNothing is useful with fmapMaybe on events containing lists of EstuaryProtocol items
-- so that no event is fired when the list is empty

lastOrNothing :: [a] -> Maybe a
lastOrNothing [] = Nothing
lastOrNothing xs = Just (last xs)
