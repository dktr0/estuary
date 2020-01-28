{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Estuary.Languages.CineCer0.Signal where

import Data.Time
import Estuary.Types.Tempo


 --              Tempo    Video Length      render T   eval T
type Signal a = Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> a

instance Num a => Num (Signal a) where
    x + y = \t dur renderTime evalTime -> (x t dur renderTime evalTime) + (y t dur renderTime evalTime)
    x * y = \t dur renderTime evalTime -> (x t dur renderTime evalTime) * (y t dur renderTime evalTime)
    negate x = \t dur renderTime evalTime -> negate (x t dur renderTime evalTime)
    abs x = \t dur renderTime evalTime -> abs (x t dur renderTime evalTime)
    signum x = \t dur renderTime evalTime -> signum (x t dur renderTime evalTime)
    fromInteger x = \t dur renderTime evalTime -> fromInteger x

------ functions that generate signals

constantSignal :: a -> Signal a
constantSignal x = \_ _ _ _ -> x

-- Temporal Functions

------ Manually apply a rate into a video ------

applyRate:: Rational -> Signal (Maybe Rational)
applyRate rate t length render eval = Just rate


------ Play at Natural Rate and without alteration ------

               -- Tempo -> VideoLength -> Now -> Position
playNatural_Pos:: Rational -> Signal (Maybe NominalDiffTime)
playNatural_Pos sh t vl render eval =
    let vLen = realToFrac vl :: Rational
        cpDur = 1/(cps t)
        off = sh*cpDur
        difb0 = realToFrac (diffUTCTime render (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / vLen
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm vLen
    in Just (realToFrac result)

playNatural_Rate :: Rational-> Signal (Maybe Rational)
playNatural_Rate sh t vl render eval = Just 1

------ Makes a video shorter or longer acording to given # of cycles ------

-- gets the onset time of the video in playEvery
             -- Rational -> Tempo ->     VideoLength -> Now     -> Position
playEvery_Pos:: Rational -> Rational -> Signal (Maybe NominalDiffTime)
playEvery_Pos c sh t vl render eval =
    let n = c
        ec = (elapsedCycles t render)
        ecOf = ec - sh
        floored = floor (ecOf/n)
        nlb = (fromIntegral floored :: Rational)*n
        pos= (realToFrac vl) * ((ecOf-nlb)/n)
    in Just (realToFrac pos)

-- gets the rate time for the video in playEvery
              -- Rational -> Tempo ->     VideoLength ->     Now -> Rate
playEvery_Rate:: Rational -> Rational -> Signal (Maybe Rational)
playEvery_Rate c sh t vl render eval =
    let vLen = realToFrac vl :: Rational
        n = c
        cps' = cps t
        rate = vLen/(n/cps')
    in Just (realToFrac rate)

------ Rounds the duration of video to the nearest cycle ------

            --    Shift ->  Tempo ->     VideoLength ->     Now -> Position
playRound_Pos:: Rational -> Signal (Maybe NominalDiffTime)
playRound_Pos sh t vlen render eval =
    let vl = realToFrac vlen :: Rational
        cp = (cps t)
        cpDur = 1/cp -- reciprocal of cps is duration in secs
        off = sh*cpDur
        cPerLen = vl/cpDur -- how many cycles for 1 whole video
        newLVinCPS = fromIntegral (round cPerLen) :: Rational -- this is new length
        inSecs = newLVinCPS *cpDur -- THIS IS THE ONE YOU DIVIDE with the difb0
-- I need to provide the seconds from beatZero, after rounding and
-- trasnforming to seconds it is not necessary to think in cycles.
        difb0 = realToFrac (diffUTCTime render (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / inSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm inSecs
    in Just (realToFrac  result) --transforms this into seconds

playRound_Rate:: Rational -> Signal (Maybe Rational)
playRound_Rate sh t vlen render eval =
    let vl = realToFrac vlen :: Rational
        cp = (cps t)
        cpDur = 1/cp
        cPerLen = vl/cpDur
        floored = fromIntegral (floor cPerLen) :: Rational -- new length in cycles
        newVl = floored / cp -- new length in seconds
        rate = vl / newVl
    in Just rate


------------ playRoundMetre ------------
---- round the duration of the video to a power of N so it can align with the notion of metre

playRoundMetrePos:: Rational -> Signal (Maybe NominalDiffTime)
playRoundMetrePos sh t vlen render eval =
    let vl = realToFrac vlen :: Rational
        cpDur = realToFrac (1/(cps t)) :: Rational
        off = sh*cpDur
        cPerLen = vl/cpDur -- how many cycles for 1 whole video
        newVLinCPS = stretchToMetre cPerLen
        inSecs = newVLinCPS *cpDur
        difb0 = realToFrac (diffUTCTime render (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / inSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        result = reglaDeTres 1 posNorm inSecs
    in Just (realToFrac  result) --transforms this into seconds

playRoundMetreRate:: Rational -> Signal (Maybe Rational)
playRoundMetreRate sh t vlen render eval =
    let vl = realToFrac vlen :: Rational
        cpDur = realToFrac (1/(cps t)) :: Rational
        off = (realToFrac sh :: Rational) *cpDur
        cPerLen = vl/cpDur -- how many cycles for 1 whole video
        newVLinCPS = stretchToMetre cPerLen
        newVLinSecs = newVLinCPS * cpDur
        rate = realToFrac vl / newVLinSecs
    in Just rate


stretchToMetre:: Rational -> Rational
stretchToMetre cPlen =
    let ceilFloor =  getCeilFloor [2] cPlen
        ceil = last ceilFloor
        floor = last $ init ceilFloor
    in if (cPlen - floor) < (ceil - cPlen) then floor else ceil

getCeilFloor:: [Rational] -> Rational -> [Rational]
getCeilFloor [] _ = []
getCeilFloor n ln
        | last n < ln =
            let result = (last n)*2
            in result : getCeilFloor (n++[result]) ln
        | last n < ln = n
        | otherwise = []



------ Chop the video for any given cycles with normal rate ------
-- gets the interval of the video reproduced with an offset time and
-- a length in cycles, OJO the startPos is normalised from 0 to 1.
             -- OnsetPos ->   Cycles -> Shift ->    Tempo ->     VideoLength ->     Now -> Position
playChop_Pos':: Rational -> Rational -> Rational -> Signal (Maybe NominalDiffTime)
playChop_Pos' startPos cycles sh t vlen render eval =
    let vl = realToFrac vlen :: Rational
        cd = cycles
        sP = reglaDeTres 1 startPos vl
        cp = (cps t)
        cpDur = 1/cp
        off = sh*cpDur
        cInSecs= cd * cpDur
        difb0 = realToFrac (diffUTCTime render (beatZero t)) :: Rational
        difb0' = difb0 - off
        lengths = difb0' / cInSecs
        posNorm = lengths - fromIntegral (floor lengths) :: Rational
        pos = reglaDeTres 1 posNorm cInSecs
    in Just (realToFrac (sP + pos))


              -- StartPos ->  Cycles ->  Shift    -> Tempo -> VideoLength ->         Now -> Rate
playChop_Rate':: Rational -> Rational -> Rational -> Signal (Maybe Rational)
playChop_Rate' startPos cycles sh t vlen render eval = Just 1


------------- Gives a chop start and end position and adjust the rate to any given cycles ------
-------- the UBER chop ---------
         --    startPos -> endPos   -> Cycles   ->    Shift -> Tempo -> VideoLength     -> Now     -> Position
playChop_Pos:: Rational -> Rational -> Rational -> Rational -> Signal (Maybe NominalDiffTime)
playChop_Pos startPos endPos cycles sh t vlen render eval =
    let cp = (cps t)
        cpsDur = 1/cp
        vl = realToFrac vlen :: Rational
        start = reglaDeTres 1 startPos vl
        end = reglaDeTres 1 endPos vl
        interval = end - start
        intCyc = interval / cpsDur
        rounded = fromIntegral (round intCyc) :: Rational
        inSecs = rounded * cpsDur
        ec = (elapsedCycles t render)
        ecOff = ec - sh
        ecFloored = fromIntegral (floor ecOff) :: Rational
        ecRender = ecOff - ecFloored
        pos = reglaDeTres 1 ecRender interval
        pos' = start + pos
    in Just (realToFrac pos')


playChop_Rate:: Rational -> Rational -> Rational -> Rational -> Signal (Maybe Rational)
playChop_Rate startPos endPos cycles sh t vlen render eval
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
playChopSecs_Pos:: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> Signal (Maybe NominalDiffTime)
playChopSecs_Pos startPos endPos cycles sh t vlen render eval =
    let cp = (cps t)
        cpsDur = 1/cp
        vl = realToFrac vlen :: Rational
        start = cycleSecs startPos vlen
        end = cycleSecs endPos vlen
        interval = end - start
        intCyc = interval / cpsDur
        rounded = fromIntegral (round intCyc) :: Rational
        inSecs = rounded * cpsDur
        ec = (elapsedCycles t render)
        ecOff = ec - sh
        ecFloored = fromIntegral (floor ecOff) :: Rational
        ecRender = ecOff - ecFloored
        pos = reglaDeTres 1 ecRender interval
        pos' = start + pos
    in Just (realToFrac pos')


playChopSecs_Rate:: NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> Signal (Maybe Rational)
playChopSecs_Rate startPos endPos cycles sh t vlen render eval
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
playNow_Pos:: NominalDiffTime -> Rational -> Signal (Maybe NominalDiffTime)
playNow_Pos startPos rate t vlen render eval = Just $ (realToFrac (cycleSecs startPos vlen))

playNow_Rate:: NominalDiffTime -> Rational -> Signal (Maybe Rational)
playNow_Rate startPos rate t vlen render eval = Just rate

-- Geometry




-- Image


---- Opacity

-- sets a default opacity for new videos --
-- defaultOpacity :: Signal Rational
-- defaultOpacity = \_ _ _ _ -> 100

------ Manually changes the opacity of a video ------

opacityChanger:: Rational -> Signal Rational
opacityChanger arg t len rend eval = arg


-- Dynamic Functions

ramp :: NominalDiffTime -> Rational -> Rational -> Signal Rational
ramp durVal startVal endVal = \_ _ renderTime evalTime ->
  let startTime = diffUTCTime evalTime evalTime -- place holder, add quant later
      endTime' = addUTCTime durVal evalTime
      endTime = diffUTCTime endTime' evalTime
  in ramp' renderTime evalTime startTime endTime startVal endVal

-- ramp:: UTCTime -> UTCTime -> NominalDiffTime -> Rational -> Rational -> Rational
-- ramp renderTime evalTime durVal startVal endVal =
--   let startTime = diffUTCTime evalTime evalTime -- place holder, add quant later
--       endTime' = addUTCTime durVal evalTime
--       endTime = diffUTCTime endTime' evalTime
--   in ramp'' renderTime evalTime startTime endTime startVal endVal

-- Add Signal type!!!!!
-- Ramper with new features !!! ------ Creates a ramp given the rendering time (now)
ramp':: UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> Rational
ramp' renderTime evalTime startTime endTime startVal endVal
    | addUTCTime startTime evalTime > renderTime = startVal
    | addUTCTime endTime evalTime < renderTime = endVal
    | otherwise =
        let segmentVal = endVal - startVal
            startScale = startVal
            start = addUTCTime startTime evalTime
            end = addUTCTime endTime evalTime
            processInterval = diffUTCTime end start
            renderInterval = diffUTCTime renderTime start
            result = getPercentage (realToFrac renderInterval :: Rational) (realToFrac processInterval :: Rational) segmentVal
        in startScale + (result)
--This function need to be corrected. Provide duration instead of startTime and endTime. starTime = evalTime (for now), endTime = evalTime + duration

--------- Helper Functions ------------

getPercentage:: Rational -> Rational -> Rational -> Rational
getPercentage value scale limit = (value/scale) * limit

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

-- test functions for ramps

startPoint = UTCTime today 30

endPoint = UTCTime today 40

renderTime = UTCTime today 34.5
