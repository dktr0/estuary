module Estuary.Languages.CineCer0.DynamicOpacity where

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time

import Estuary.Types.Tempo

-- sets a default opacity for new videos --
defaultOpacity :: Tempo -> NominalDiffTime -> UTCTime -> Rational
defaultOpacity _ _ _ = 100


------ Manually changes the opacity of a video ------

opacityChanger:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Rational
opacityChanger arg t len now = arg  


  -- HOW TO GET THE EVALUATION TIME??????
-- fade In waiting number of cycles and with a fade time
-- fadeIn:: Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Rational
-- fadeIn waitCycles fadeTime t len now = 

-- -- fade Out waiting a number of cycles and with a fade time...  ¿if opacity already 0 then do nothing?
-- fadeOut:: Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Rational
-- fadeOut waitCycles fadeTime t len now = 


--------- Helper Functions ------------

getPercentage:: Rational -> Rational -> Rational
getPercentage value scale = (value/scale) * 100        

reglaDeTres:: Rational -> Rational -> Rational -> Rational
reglaDeTres normScale normPos realScale = (normPos*realScale) / normScale

    --   startorendPos           vidLength
cycleSecs:: NominalDiffTime -> NominalDiffTime -> Rational
cycleSecs startPos vlen
    | startPos < vlen = (realToFrac vlen :: Rational) - (realToFrac startPos :: Rational)
    | otherwise =
    let sp = realToFrac startPos :: Rational
        vl = realToFrac vlen :: Rational
        x = sp / vl
        floored = fromIntegral (floor x) :: Rational
        rest = vl * floored
        cycle = sp - rest
    in cycle


-- Ramper !!! ------ Creates a ramp given the rendering time (now)

ramp:: UTCTime -> UTCTime -> UTCTime -> Rational
ramp now start end 
    | start > now = 0
    | end < now = 1
    | otherwise = 
        let processInterval = diffUTCTime end start
            renderInterval = diffUTCTime now start 
            result = getPercentage (realToFrac renderInterval :: Rational) (realToFrac processInterval :: Rational)
        in result * 0.01

-- test functions

today = fromGregorian 2019 07 18

myUTCTest = UTCTime today 30

myUTCTest2 = UTCTime today 60

myTempo = Tempo { cps= 0.5, at= myUTCTest, beat= 10.3}

-- test functions for ramps

startPoint = UTCTime today 30

endPoint = UTCTime today 40

renderTime = UTCTime today 34.5