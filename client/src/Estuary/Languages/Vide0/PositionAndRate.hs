module Estuary.Languages.Vide0.PositionAndRate where

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time

import Estuary.Types.Tempo


-- type VideoLength = NominalDiffTime
-- type Now = UTCTime
-- type Rate = NominalDiffTime
-- type Position = NominalDiffTime


------ Play at Natural Rate and without alteration ------

               -- Tempo -> VideoLength -> Now -> Position
playNatural_Pos:: Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playNatural_Pos t vl now =
    let vLen = realToFrac vl :: Rational
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Rational
        lengths = difb0 / vLen
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm vLen
    in Just (realToFrac result)

playNatural_Rate :: Tempo -> NominalDiffTime -> UTCTime -> Rational
playNatural_Rate t vl now = 1

------ Makes a video shorter or longer acording to given # of cycles ------

-- gets the onset time of the video in playEvery
             -- Rational -> Tempo ->     VideoLength -> Now     -> Position
playEvery_Pos:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playEvery_Pos c t vl now =
    let n = c
        ec = (elapsedCycles t now)
        floored = floor (ec/n)
        nlb = (fromIntegral floored :: Rational)*n
        pos= (realToFrac vl) * ((ec-nlb)/n)
    in Just (realToFrac pos)

-- gets the rate time for the video in playEvery
              -- Rational -> Tempo ->     VideoLength ->     Now -> Rate
playEvery_Rate:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playEvery_Rate c t vl now =
    let vLen = realToFrac vl :: Rational
        n = c
        cps' = cps t
        rate = vLen/(n/cps')
    in Just (realToFrac rate)

------ Rounds the duration of video to the nearest cycle ------

             -- Tempo -> VideoLength -> Now -> Position
playRound_Pos:: Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playRound_Pos t vlen now =
    let vl = realToFrac vlen :: Rational
        cp = (cps t)
        cpDur = 1/cp -- reciprocal of cps is duration in secs
        cPerLen = vl/cpDur -- how many cycles for 1 whole video
        newLVinCPS = fromIntegral (round cPerLen) :: Rational -- this is new length
        inSecs = newLVinCPS *cpDur -- THIS IS THE ONE YOU DIVIDE with the difb0
-- I need to provide the seconds from beatZero, after rounding and
-- trasnforming to seconds it is not necessary to think in cycles.
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Rational
        lengths = difb0 / inSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm inSecs
    in Just (realToFrac  result) --transforms this into seconds

playRound_Rate:: Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playRound_Rate t vlen now =
    let vl = realToFrac vlen :: Rational
        cp = (cps t)
        cpDur = 1/cp
        cPerLen = vl/cpDur
        floored = fromIntegral (floor cPerLen) :: Rational -- new length in cycles
        newVl = floored / cp -- new length in seconds
        rate = vl / newVl
    in Just (realToFrac rate)

------ Chop the video for any given cycles with normal rate ------

-- gets the interval of the video reproduced with an offset time and
-- a length in cycles, OJO the startPos is normalised from 0 to 1.
             -- OnsetPos ->   Cycles -> Tempo ->     VideoLength ->     Now -> Position
playChop_Pos':: Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playChop_Pos' onsetPos cycles t vlen now =
    let vl = realToFrac vlen :: Rational
        cd = cycles
        sP = reglaDeTres 1 onsetPos vl
        cp = (cps t)
        cpDur = 1/cp
        cInSecs= cd * cpDur
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Rational
        lengths = difb0 / cInSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        pos = reglaDeTres 1 posNorm cInSecs
    in Just (realToFrac (sP + pos))


              -- StartPos ->  Cycles -> Tempo -> VideoLength ->         Now -> Rate
playChop_Rate':: Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playChop_Rate' startPos cyclesDur t vlen now = Just (realToFrac 1)


------------- Gives a chop start and end position and adjust the rate to any given cycles ------
-------- the UBER chop ---------
         --    startPos -> endPos   -> Cycles   -> Tempo -> VideoLength     -> Now     -> Position
playChop_Pos:: Rational -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playChop_Pos startPos endPos cycles t vlen now =
    let cp = (cps t)
        cpsDur = 1/cp
        vl = realToFrac vlen :: Rational
        start = reglaDeTres 1 startPos vl
        end = reglaDeTres 1 endPos vl
        interval = end - start
        intCyc = interval / cpsDur
        rounded = fromIntegral (round intCyc) :: Rational
        inSecs = rounded * cpsDur
        ec = (elapsedCycles t now)
        ecFloored = fromIntegral (floor ec) :: Rational
        ecNow = ec - ecFloored
        pos = reglaDeTres 1 ecNow interval
        pos' = start + pos
    in Just (realToFrac pos')


playChop_Rate:: Rational -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playChop_Rate startPos endPos cycles t vlen now
    | startPos == endPos = Just 0
    | otherwise =
    let cp = (cps t)
        cpsDur = 1/cp
        vl = realToFrac vlen :: Rational
        start = reglaDeTres 1 startPos vl
        end = reglaDeTres 1 endPos vl
        interval = end - start
        cPerLen = interval/cpsDur
        rounded = fromIntegral (round cPerLen) :: Rational -- new length in cycles
        newVl = rounded / cp -- new length in seconds
        rate = interval / newVl
        addNeg = if start > end then rate * (-1) else rate
    in  Just (realToFrac addNeg)


--------- Helper Functions ------------

reglaDeTres:: Rational -> Rational -> Rational -> Rational
reglaDeTres normScale normPos realScale = (normPos*realScale) / normScale


-- test functions

today = fromGregorian 2019 07 18

myUTCTest = UTCTime today 30

myUTCTest2 = UTCTime today 60

myTempo = Tempo { cps= 0.5, at= myUTCTest, beat= 10.3}
