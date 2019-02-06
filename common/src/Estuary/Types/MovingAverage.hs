module Estuary.Types.MovingAverage where

type MovingAverage = (Int,[Double])

newAverage :: Int -> MovingAverage
newAverage l = (l,[])

updateAverage :: MovingAverage -> Double -> (Double,MovingAverage)
updateAverage s x = (sum xs / fromIntegral (length xs),(fst s,xs))
  where xs = take (fst s) $ x:(snd s)
