module Estuary.Languages.CineCer0.DynamicOpacity where

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time

import Estuary.Types.Tempo


-- defaultOpacity :: Tempo -> NominalDiffTime -> UTCTime -> Rational
-- defaultOpacity _ _ _ = 100

opacityChanger:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Rational
opacityChanger arg t len now = arg  


--------- Helper Functions ------------

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


-- test functions

today = fromGregorian 2019 07 18

myUTCTest = UTCTime today 30

myUTCTest2 = UTCTime today 60

myTempo = Tempo { cps= 0.5, at= myUTCTest, beat= 10.3}