{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Estuary.Languages.CineCer0.Signal where

import Data.Time
import Data.Tempo


 --              Tempo    Video Length       render T   eval T    anchor t
type Signal a = Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> UTCTime -> a

{- instance Functor Signal where
  fmap f s = \t videoDur renderT evalT anchorT -> f (s t videoDur renderT evalT anchorT)

instance Applicative Signal where
  pure x = \_ _ _ _ _ -> x
  f <*> x = \t videoDur renderT evalT anchorT -> f t videoDur renderT evalT anchorT $ x t videoDur renderT evalT anchorT

instance Monad Signal where
  a >>= f = \t videoDur renderT evalT anchorT -> f (a t videoDur renderT evalT anchorT) a t videoDur renderT evalT anchorT
-}

instance Num a => Num (Signal a) where
    x + y = \t dur renderTime evalTime anchTime -> (x t dur renderTime evalTime anchTime) + (y t dur renderTime evalTime anchTime)
    x * y = \t dur renderTime evalTime anchTime -> (x t dur renderTime evalTime anchTime) * (y t dur renderTime evalTime anchTime)
    negate x = \t dur renderTime evalTime anchTime -> negate (x t dur renderTime evalTime anchTime)
    abs x = \t dur renderTime evalTime anchTime -> abs (x t dur renderTime evalTime anchTime)
    signum x = \t dur renderTime evalTime anchTime -> signum (x t dur renderTime evalTime anchTime)
    fromInteger x = \t dur renderTime evalTime anchTime -> fromInteger x


------

multiplyMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
multiplyMaybe Nothing Nothing = Nothing
multiplyMaybe (Just x) Nothing = Just x
multiplyMaybe Nothing (Just x) = Just x
multiplyMaybe (Just x) (Just y) = Just (x*y)

multipleMaybeSignal :: Num a => Signal (Maybe a) -> Signal (Maybe a) -> Signal (Maybe a)
multipleMaybeSignal x y = \a b c d e -> multiplyMaybe (x a b c d e) (y a b c d e)


------ functions that generate signals

constantSignal :: a -> Signal a
constantSignal x = \_ _ _ _ _ -> x

defaultAnchor:: Tempo -> UTCTime -> UTCTime
defaultAnchor t eval = quantAnchor 1 0 t eval

-- calculates the anchorTime
quantAnchor:: Rational -> Rational -> Tempo -> UTCTime -> UTCTime
quantAnchor cycleMult offset t eval =
  let ec = (timeToCount t eval)
      currentCycle = fromIntegral (floor ec):: Rational 
      align = if ec - currentCycle > 0.95 then 2 else 1
      toQuant = currentCycle + align
      quanted = nextBeat cycleMult offset toQuant
  in countToTime t (quanted) 

constantSignal' :: Maybe a -> Signal (Maybe a)
constantSignal' Nothing = \_ _ _ _ _ -> Nothing
constantSignal' (Just x) = \_ _ _ _ _ -> (Just x)

-- Temporal Functions

-------

freeRun:: Signal (Maybe NominalDiffTime) 
freeRun t vl render eval anchor = Nothing


------ Play at Natural Rate and without alteration ------

--playNatural _Pos and _Rate will play de video in its pregiven rate and the 0:00" position
-- of the video will align with the cycle 0 beat 0 of Estuary's clock (Tempo).
-- This function does not align in any way with the cycle and beat count of the Tempo
-- it has only one argument (that can also be found in every time function): shift,
-- shift changes the start value of the video: 0.5 shift in a 12 secs video will align the 0 of the tempo
-- with the video at its 50% (6:00 secs)
-- ??? the code suggests that the shift is multiples of cycles of the tempo rather than video length ???

playNatural_Pos:: Rational -> Signal (Maybe NominalDiffTime)
playNatural_Pos sh t vlen render eval anchor =
    let lenInCycles = (realToFrac vlen :: Rational) * (freq t) 
        ec = (timeToCount t render) --  30
        ecOf = ec - sh
        pos' = ecOf / lenInCycles  -- 30 / 13 =  2.3076923076923
        posPerc = pos' - (fromIntegral (floor pos') :: Rational) -- 0.3076923076923 
        pos = posPerc * lenInCycles -- 4
    in Just $ realToFrac (pos/(freq t)) -- 8 

playNatural_Rate :: Rational -> Signal (Maybe Rational)
playNatural_Rate sh t vl render eval anchor = Just 1

------ Makes a video shorter or longer acording to given # of cycles ------

-- this fucntion aligns the position and stretches (or collapses) the video length
--according to a given number of cycles. If a 3 is passed as the cycle argument,
-- the video provided is 12 secs long and the cycles of the Tempo last 2.5 secs, the function
-- will transform the video so it total duration is now 7.5 (the duration of 3 cycles) and its 0:00" mark
-- will align with the begining of a cycle.
-- the args for this func are cycle and shift


-- gets the onset time of the video in playEvery
playEvery_Pos:: Rational -> Rational -> Signal (Maybe NominalDiffTime)
playEvery_Pos c sh t vl render eval anchor 
    | c == 0 = Nothing
    | otherwise =
    let n = c -- 4  -- if c is 0 needs to do something safe
        ec = (timeToCount t render) -- 30 cycles (60 secs if each cycle is 2 secs long)
        ecOf = ec - sh
        floored = floor (ecOf/n) -- 30/4 = 7.5 then floored= 7
        nlb = (fromIntegral floored :: Rational)*n -- 7 * 4 = 28
        pos= (realToFrac vl) * ((ecOf-nlb)/n) -- 12.5 secs * 30-28/4 = 12.5 * 0.5
    in Just (realToFrac pos)

-- gets the rate time for the video in playEvery

playEvery_Rate:: Rational -> Rational -> Signal (Maybe Rational)
playEvery_Rate c sh t vl render eval anchor =
    let vLen = realToFrac vl :: Rational
        n = c
        freq' = freq t
        rate = vLen/(n/freq')
    in Just (realToFrac rate)

------ Rounds the duration of video to the nearest cycle ------

-- round _Rate and _Pos will modify the rate of a video to fit its length to the closest number of cycles,
-- if the video's length is 12 secs and the cycles last 2.5 secs the video will be fitted to 12.5 secs,
-- in this way the video will be as close to its original length but will be aligned to the Tempo of Estuary
-- In this example, 5 cycles will hold the whole video.
-- this functions only accept from the player one argument: shift

playRound_Pos:: Rational -> Signal (Maybe NominalDiffTime)
playRound_Pos sh t vlen render eval anchor 
    | (freq t) == 0 = Nothing -- double check if this is adequate behaviour
    | otherwise =
    let lenInCycles = (realToFrac vlen :: Rational) * (freq t) 
        ec = (timeToCount t render) 
        ecSh = ec - sh 
        newVL = fromIntegral (round lenInCycles) :: Rational 
        pos' = ecSh / newVL 
        posPerc = pos' - (fromIntegral (floor pos') :: Rational) 
        pos = posPerc * lenInCycles 
    in Just (realToFrac (pos/(freq t))) 

playRound_Rate:: Rational -> Signal (Maybe Rational)
playRound_Rate sh t vlen render eval anchor =
    let vl = realToFrac vlen :: Rational
        lenInCycles = (realToFrac vlen :: Rational) * (freq t) 
        newVL = fromIntegral (round lenInCycles) :: Rational -- 6.0
        newVl = (newVL/(freq t)) -- 6.0 / 0.5 -- 12.0
        rate = vl / newVl --old video length in secs / new video length in secs
    in Just rate

------------ playRoundMetre ------------

-- Similar to the above but it fits the length of the video to a power of 2 number of cycles so it can adapt
-- to common music understandings of period, phrase and subdivision. For example, if a video is 12 secs long
-- and each cycle is 2.5 secs long, this functions will change the rate so the video fits in four cycles (10 secs)
-- this functions only accept from the player one argument: shift

---- round the duration of the video to a power of 2 so it can align with the notion of metre
playRoundMetre_Pos:: Rational -> Signal (Maybe NominalDiffTime)
playRoundMetre_Pos sh t vlen render eval anchor
    | (freq t) == 0 = Nothing -- double check if this is adequate behaviour
    | otherwise =
    let lenInCycles = (realToFrac vlen :: Rational) * (freq t) 
        ec = (timeToCount t render) 
        ecSh = ec - sh 
        roundedVL = fromIntegral (round lenInCycles) :: Rational 
        newVL = stretchToMetre lenInCycles
        pos' = ecSh / newVL 
        posPerc = pos' - (fromIntegral (floor pos') :: Rational) 
        pos = posPerc * lenInCycles 
    in Just (realToFrac (pos/(freq t))) 


playRoundMetre_Rate:: Rational -> Signal (Maybe Rational)
playRoundMetre_Rate sh t vlen render eval anchor 
    | (freq t) == 0 = Just 0
    | otherwise =
    let vl = realToFrac vlen :: Rational
        lenInCycles = (realToFrac vlen :: Rational) * (freq t) -- 6.25
        newVL = stretchToMetre lenInCycles   -- 8
        newVLinSecs = newVL / (freq t)  --- 8/0.5 = 16
        rate = vl / newVLinSecs -- 12.5 / 16
    in Just (realToFrac rate)


stretchToMetre:: Rational -> Rational
stretchToMetre cPlen =
    let ceilFloor =  getCeilFloor [0.125/16] cPlen
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

------------- Gives a chop start and end position and adjust the rate to any given cycles ------
-------- the UBER chop ---------

-- Similar to the one above but it takes a start and an end position, normalised from 0 to 1

playChop_Pos'::  Signal Rational -> Signal Rational -> Signal (Maybe NominalDiffTime)
playChop_Pos' startPos' endPos' t vlen render eval anchor = 
  let startPos = startPos' t vlen render eval anchor
      endPos = endPos' t vlen render eval anchor
      lenInCycles = (realToFrac vlen :: Rational) * (freq t) -- 6.5
      (s,e) = intraLoopChop startPos endPos
      start = lenInCycles * s  -- 6.5 * 0.75
      end = lenInCycles * e   -- 6.5 * 1.25
      cycles = end - start
  in playChop_Pos startPos' endPos' (constantSignal cycles) t vlen render eval anchor

playChop_Rate':: Signal Rational -> Signal Rational -> Signal (Maybe Rational)
playChop_Rate' startPos endPos t vlen render eval anchor 
    | startPos t vlen render eval anchor  == endPos t vlen render eval anchor = Just 0
    | (freq t) == 0 = Just 0
    | otherwise = Just 1

playChop_Pos::  Signal Rational -> Signal Rational -> Signal Rational -> Signal (Maybe NominalDiffTime)
playChop_Pos startPos endPos cycles t vlen render eval anchor 
    | startPos t vlen render eval anchor  == endPos t vlen render eval anchor = Just (vlen * (realToFrac (startPos t vlen render eval anchor) :: NominalDiffTime))
    | (freq t) == 0 = Just (realToFrac (startPos t vlen render eval anchor)*vlen)
    | otherwise =
        let lenInCycles = (realToFrac vlen :: Rational) * (freq t) -- 6.5
            (s,e) = intraLoopChop (startPos t vlen render eval anchor) (endPos t vlen render eval anchor)
            start = lenInCycles * s  -- 6.5 * 0.75
            end = lenInCycles * e   -- 6.5 * 1.25
            n = (cycles t vlen render eval anchor) -- 2
            ec = (timeToCount t render) -- 30.7
            ecOf = ec --  - sh   --  shift value used to be here
            floored = floor (ecOf/n)       -- 30.7 /2 = 15 
            nlb = (fromIntegral floored :: Rational)*n -- 15 * 2 = 30
            pos' = start +((end - start) * ((ecOf-nlb)/n)) -- 0.75 + (1.25 - 0.75) * (0.35)
            pos  = if (pos' >= lenInCycles) then (pos' - lenInCycles) else pos' 
        in Just (realToFrac (pos/(freq t)))

intraLoopChop:: Rational -> Rational -> (Rational,Rational)       
intraLoopChop startPos endPos = 
  let sp = startPos - (fromIntegral (floor startPos) :: Rational)
      ep' = endPos - (fromIntegral (floor endPos) :: Rational)
      ep = if (sp > ep') then (ep' + 1) else ep' -- 1.25
  in (sp, ep) -- (0.75,1.25)

playChop_Rate::  Signal Rational -> Signal Rational -> Signal Rational -> Signal (Maybe Rational)
playChop_Rate startPos endPos cycles t vlen render eval anchor 
    | startPos t vlen render eval anchor  == endPos t vlen render eval anchor = Just 0
    | (freq t) == 0 = Just 0
    | otherwise =
    let lenInCycles = (realToFrac vlen :: Rational) * (freq t)
        dur = (lenInCycles * (endPos t vlen render eval anchor)) - (lenInCycles * (startPos t vlen render eval anchor))
        rate' = dur / (cycles t vlen render eval anchor)
        rate = rate' * (signum rate')
    in Just (realToFrac rate)

-- Helper to transform cycles to seconds

-- vl= 12.5 ;; secs= 3.3;
secsToPercen:: Signal Rational -> Signal Rational
secsToPercen secs' t vl r e a = 
    let secs = secs' t vl r e a
        vlen = realToFrac vl :: Rational
        timeInPercen = (secs / vlen) - (fromIntegral (floor (secs / vlen)) :: Rational)
    in timeInPercen



-- rate 2 0 myTempo 13 render eval anchor
rate_Pos:: Rational -> Signal (Maybe NominalDiffTime)
rate_Pos rate t vlen render eval anchor =   Nothing

rate_Rate:: Rational -> Signal (Maybe Rational)
rate_Rate rate t vlen render eval anchor = Just rate


-- helpers to transform Signal

multi:: Signal Rational -> Signal Rational -> Signal Rational
multi a b = a * b

multi':: Signal Rational -> Signal Rational -> Signal (Maybe Rational)
multi' a b t vl rTime eTime aTime = Just $ multi a b t vl rTime eTime aTime

sigMayToSig:: Signal (Maybe Rational) -> Signal Rational
sigMayToSig x = \t vl rT eT aT -> mayToRat (x t vl rT eT aT)

mayToRat:: Maybe Rational -> Rational
mayToRat (Just x) = x

-- this radian func was taken from the internet, makes it easier for me to think sines
radian:: Rational -> Float
radian t = (realToFrac t :: Float) * 2 * pi /360

sineMaybe:: Signal Rational -> Signal (Maybe Rational)
sineMaybe freq t vl rT eT aT = Just $ sine (freq) t vl rT eT aT

sine:: Signal Rational -> Signal Rational
sine freq = \t vl rT eT aT ->
    let reciprocal = 1 / (freq t vl rT eT aT)
        elapsed = (realToFrac (diffUTCTime rT (time t)) :: Rational)
        pos = elapsed / reciprocal
        pos' = (pos) - (realToFrac (floor pos) :: Rational)
        posInRad = pos' * 360
    in realToFrac (sin (radian posInRad)) :: Rational

rangeMaybe:: Signal Rational -> Signal Rational -> Signal Rational -> Signal (Maybe Rational)
rangeMaybe min max input t vl rTime eTime aTime = Just $ range (min) (max) (input) t vl rTime eTime aTime

range:: Signal Rational -> Signal Rational -> Signal Rational -> Signal Rational
range min max input t vl rTime eTime aTime =
    add + ((input t vl rTime eTime aTime) * mult)
    where mult = ((max t vl rTime eTime aTime) - (min t vl rTime eTime aTime))/2
          add = mult + (min t vl rTime eTime aTime)


trimCycleRamps:: [Rational] -> [Rational] -> Signal Rational
trimCycleRamps durs vals
  | length durs >= length vals = 
    let  limit = (length vals) -1
    in ramps (take limit durs) vals
  | otherwise = ramps (take (length vals - 1) $ cycle durs) vals

rampsMaybe :: [Rational] -> [Rational] -> Signal (Maybe Rational)
rampsMaybe durs vals = \t vl rTime eTime aTime -> Just $ ramps' durs vals t vl rTime eTime aTime

ramps':: [Rational] -> [Rational] -> Signal Rational
ramps' durs vals 
  | length vals - length durs /= 1 = trimCycleRamps durs vals
  | otherwise = ramps durs vals 

ramps:: [Rational] -> [Rational] -> Signal Rational
ramps [] vals t vl r e a = last vals
ramps durs vals t vl r e a = if endOfDur > r then ramp (head durs) (head vals) (head $ tail vals) t vl r e a else ramps (tail durs) (tail vals) t vl r e endOfDur
    where endOfDur = addUTCTime (realToFrac x :: NominalDiffTime) a -- anchor time plus duration of previous ramp
          x = (head durs) / (realToFrac (freq t) :: Rational)

rampMaybe :: Rational -> Rational -> Rational -> Signal (Maybe Rational)
rampMaybe durVal startVal endVal = \t vl rTime eTime aTime -> Just $ ramp durVal startVal endVal t vl rTime eTime aTime

ramp :: Rational -> Rational -> Rational -> Signal Rational
ramp durVal startVal endVal = \t vl renderTime evalTime anchorTime ->
  let startTime = anchorTime :: UTCTime -- place holder, add quant later
      durVal' = durVal * (realToFrac (1/(freq t)) :: Rational)
      endTime = addUTCTime (realToFrac durVal' :: NominalDiffTime) anchorTime
  in ramp' renderTime startTime endTime startVal endVal

-- Ramper with new features !!! ------ Creates a ramp given the rendering time (now)
ramp':: UTCTime -> UTCTime -> UTCTime -> Rational -> Rational -> Rational
ramp' renderTime startTime endTime startVal endVal -- delete what is not needed
    | startTime >= renderTime = startVal
    | endTime <= renderTime = endVal
    | otherwise =    -- args: 3 secs of dur, startval: 0.2, endval: 0.7
        let segmentVal = endVal - startVal -- 0.5
            processInterval = realToFrac (diffUTCTime endTime startTime) :: Rational --  3 segs
            momentAtRender = realToFrac (diffUTCTime renderTime startTime) :: Rational -- assuming render is half way through the process: 1.5 out of 3.0
            percOfProcessAtRender = getPercentage momentAtRender processInterval segmentVal
        in startVal + percOfProcessAtRender


fadeIn:: Signal Rational -> Signal Rational
fadeIn dur t vl rT eT aT = ramp dur' 0 1 t vl rT eT aT
    where dur' = realToFrac (dur t vl rT eT aT) :: Rational

fadeOut:: Signal Rational -> Signal Rational
fadeOut dur t vl rT eT aT = ramp dur' 1 0 t vl rT eT aT
    where dur' = realToFrac (dur t vl rT eT aT) :: Rational

fadeIn2:: Signal Rational -> Signal (Maybe Rational)
fadeIn2 dur t vl rT eT aT = rampMaybe dur' 0 1 t vl rT eT aT
    where dur' = realToFrac (dur t vl rT eT aT) :: Rational

fadeOut2:: Signal Rational -> Signal (Maybe Rational)
fadeOut2 dur t vl rT eT aT = rampMaybe dur' 0 1 t vl rT eT aT
    where dur' = realToFrac (dur t vl rT eT aT) :: Rational



--------- Helper Functions ------------

-- gets percentage and modulates the result to a new scale. Example:
-- value: 10 scale: 50 new scale: 1 -> 0.2  (10 is 20% of 50, 20% of 1 is 0.2)
getPercentage:: Rational -> Rational -> Rational -> Rational
getPercentage value scale newScale = (value/scale) * newScale

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

myTempo = Tempo { freq= 0.5, time= myUTCTest, count= 10.3}

myEval = myUTCTest2

myRender = UTCTime today 61.5

myAnchor = quantAnchor 1 0 myTempo myEval


-- test functions for ramps

startPoint = UTCTime today 30

endPoint = UTCTime today 40

renderTime = UTCTime today 34.5
