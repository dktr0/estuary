module Estuary.Languages.CineCer0.PositionAndRate where

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time

import Estuary.Types.Tempo


-- type VideoLength = NominalDiffTime
-- type Now = UTCTime
-- type Rate = NominalDiffTime
-- type Position = NominalDiffTime


------ Play at Natural Rate and without alteration ------

               -- Tempo -> VideoLength -> Now -> Position
playNatural_Pos:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playNatural_Pos sh t vl now =
    let vLen = realToFrac vl :: Rational
        cpDur = 1/(cps t)
        off = sh*cpDur
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / vLen
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm vLen
    in Just (realToFrac result)

playNatural_Rate :: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playNatural_Rate sh t vl now = Just 1

------ Makes a video shorter or longer acording to given # of cycles ------

-- gets the onset time of the video in playEvery
             -- Rational -> Tempo ->     VideoLength -> Now     -> Position
playEvery_Pos:: Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playEvery_Pos c sh t vl now =
    let n = c
        ec = (elapsedCycles t now)
        ecOf = ec - sh
        floored = floor (ecOf/n)
        nlb = (fromIntegral floored :: Rational)*n
        pos= (realToFrac vl) * ((ecOf-nlb)/n)
    in Just (realToFrac pos)

-- gets the rate time for the video in playEvery
              -- Rational -> Tempo ->     VideoLength ->     Now -> Rate
playEvery_Rate:: Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playEvery_Rate c sh t vl now =
    let vLen = realToFrac vl :: Rational
        n = c
        cps' = cps t
        rate = vLen/(n/cps')
    in Just (realToFrac rate)

------ Rounds the duration of video to the nearest cycle ------

            --    Shift ->  Tempo ->     VideoLength ->     Now -> Position
playRound_Pos:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playRound_Pos sh t vlen now =
    let vl = realToFrac vlen :: Rational
        cp = (cps t)
        cpDur = 1/cp -- reciprocal of cps is duration in secs
        off = sh*cpDur
        cPerLen = vl/cpDur -- how many cycles for 1 whole video
        newLVinCPS = fromIntegral (round cPerLen) :: Rational -- this is new length
        inSecs = newLVinCPS *cpDur -- THIS IS THE ONE YOU DIVIDE with the difb0
-- I need to provide the seconds from beatZero, after rounding and
-- trasnforming to seconds it is not necessary to think in cycles.
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / inSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm inSecs
    in Just (realToFrac  result) --transforms this into seconds

playRound_Rate:: Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playRound_Rate sh t vlen now =
    let vl = realToFrac vlen :: Rational
        cp = (cps t)
        cpDur = 1/cp
        cPerLen = vl/cpDur
        floored = fromIntegral (floor cPerLen) :: Rational -- new length in cycles
        newVl = floored / cp -- new length in seconds
        rate = vl / newVl
    in Just rate


------ Chop the video for any given cycles with normal rate ------
-- gets the interval of the video reproduced with an offset time and
-- a length in cycles, OJO the startPos is normalised from 0 to 1.
             -- OnsetPos ->   Cycles -> Shift ->    Tempo ->     VideoLength ->     Now -> Position
playChop_Pos':: Rational -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playChop_Pos' onsetPos cycles sh t vlen now =
    let vl = realToFrac vlen :: Rational
        cd = cycles
        sP = reglaDeTres 1 onsetPos vl
        cp = (cps t)
        cpDur = 1/cp
        off = sh*cpDur
        cInSecs= cd * cpDur
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / cInSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        pos = reglaDeTres 1 posNorm cInSecs
    in Just (realToFrac (sP + pos))


              -- StartPos ->  Cycles ->  Shift    -> Tempo -> VideoLength ->         Now -> Rate
playChop_Rate':: Rational -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playChop_Rate' startPos cyclesDur sh t vlen now = Just 1


------------- Gives a chop start and end position and adjust the rate to any given cycles ------
-------- the UBER chop ---------
         --    startPos -> endPos   -> Cycles   ->    Shift -> Tempo -> VideoLength     -> Now     -> Position
playChop_Pos:: Rational -> Rational -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playChop_Pos startPos endPos cycles sh t vlen now =
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
        ecOff = ec - sh
        ecFloored = fromIntegral (floor ecOff) :: Rational
        ecNow = ecOff - ecFloored
        pos = reglaDeTres 1 ecNow interval
        pos' = start + pos
    in Just (realToFrac pos')


playChop_Rate:: Rational -> Rational -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playChop_Rate startPos endPos cycles sh t vlen now
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

-------- the UBER chop with start and end position in seconds instead of 0 to 1 ---------

         --           startPos     -> endPos          -> Cycles   ->   Shift  -> Tempo -> VideoLength     -> Now     -> Position
playChopSecs_Pos:: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playChopSecs_Pos startPos endPos cycles sh t vlen now =
    let cp = (cps t)
        cpsDur = 1/cp
        vl = realToFrac vlen :: Rational
        start = cycleSecs startPos vlen
        end = cycleSecs startPos vlen
        interval = end - start
        intCyc = interval / cpsDur
        rounded = fromIntegral (round intCyc) :: Rational
        inSecs = rounded * cpsDur
        ec = (elapsedCycles t now)
        ecOff = ec - sh
        ecFloored = fromIntegral (floor ecOff) :: Rational
        ecNow = ecOff - ecFloored
        pos = reglaDeTres 1 ecNow interval
        pos' = start + pos
    in Just (realToFrac pos')


playChopSecs_Rate:: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playChopSecs_Rate startPos endPos cycles sh t vlen now
    | startPos == endPos = Just 0
    | otherwise =
    let cp = (cps t)
        cpsDur = 1/cp
        vl = realToFrac vlen :: Rational
        start = cycleSecs startPos vlen
        end = cycleSecs startPos vlen
        interval = end - start
        cPerLen = interval/cpsDur
        rounded = fromIntegral (round cPerLen) :: Rational -- new length in cycles
        newVl = rounded / cp -- new length in seconds
        rate = interval / newVl
        addNeg = if start > end then rate * (-1) else rate
    in  Just (realToFrac addNeg)

------- playNow  -- Starts the video with the evaluation time

-- giving a start position in seconds

           --  startPos        -- rate
playNow_Pos:: NominalDiffTime -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe NominalDiffTime
playNow_Pos startPos rate t vlen now = Just $ (realToFrac (cycleSecs startPos vlen))

playNow_Rate:: NominalDiffTime -> Rational -> Tempo -> NominalDiffTime -> UTCTime -> Maybe Rational
playNow_Rate startPos rate t vlen now = Just rate

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
