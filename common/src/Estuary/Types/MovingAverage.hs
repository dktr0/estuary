module Estuary.Types.MovingAverage where

type MovingAverage = (Int,[Double])

newAverage :: Int -> MovingAverage
newAverage l = (l,[])

getAverage :: MovingAverage -> Double
getAverage (_,[]) = 0
getAverage (_,xs) = sum xs / fromIntegral (length xs)

getPeak :: MovingAverage -> Double
getPeak (_,[]) = 0
getPeak (_,xs) = maximum xs

updateAverage :: MovingAverage -> Double -> MovingAverage
updateAverage s x = (fst s, take (fst s) $ x:(snd s))
