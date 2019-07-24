module Estuary.Languages.Vide0.PositionAndRate where

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Time

import Estuary.Types.Tempo


type VideoLength = NominalDiffTime
type Now = UTCTime
type Rate = NominalDiffTime
type Position = NominalDiffTime


------ Play at Natural Rate and without alteration ------

playNatural_Pos :: Tempo -> VideoLength -> Now -> Position
playNatural_Pos t vl now =
    let vLen = realToFrac vl :: Double
        difb0 = realToFrac (diffUTCTime now (beatZero t)) :: Double
        lengths = difb0 / vLen
        positionNormalisedToOne = lengths - fromIntegral (floor lengths) :: Double
        result = reglaDeTres 1 positionNormalisedToOne vLen
    in realToFrac result

playNatural_Rate :: Tempo -> VideoLength -> Now -> Rate
playNatural_Rate t vl n = 1

--

------ Makes a video shorter or longer acording to given # of cycles ------

-- gets the onset time of the video in playEvery
playEvery_Pos:: Int -> Tempo -> VideoLength -> Now -> Position
playEvery_Pos c t vl now =
    let n = fromIntegral c :: Double
        ec = (elapsedCycles t now)
        floored = floor (ec/n)
        nlb = (fromIntegral floored :: Double)*n
        pos= (realToFrac vl) * ((ec-nlb)/n)
    in realToFrac pos

-- gets the rate time for the video in playEvery
playEvery_Rate:: Int -> Tempo -> VideoLength -> Now -> Rate
playEvery_Rate c t vl now =
    let vLen = (realToFrac vl)
        n = fromIntegral c :: Double
        cps' = cps t
        rate = vLen/(n/cps')
    in realToFrac rate

--

------ Rounds the duration of video to the nearest cycle ------

playRound_Pos :: Tempo -> VideoLength -> Now -> Position
playRound_Pos t vlen now =
    let vl = realToFrac vlen :: Double
        cp = (cps t)
        cpDur = 1/cp -- reciprocal of cps is duration in secs
        cPerLen = vl/cpDur -- how many cycles for 1 whole video
        newLVinCPS = fromIntegral (floor cPerLen) :: Double -- this is new length
        ec = (elapsedCycles t now) -- ellasped cycles
        ecFloor = fromIntegral (floor ec) :: Double
        ecNow = ec - ecFloor
        result = reglaDeTres 1 ecNow newLVinCPS
    in realToFrac (result/cp) -- /cp transforms this into seconds

playRound_Rate:: Tempo -> VideoLength -> Now -> Rate
playRound_Rate t vlen now =
    let vl = realToFrac vlen :: Double
        cp = (cps t)
        cpDur = 1/cp
        cPerLen = vl/cpDur
        floored = fromIntegral (floor cPerLen) :: Double -- new length in cycles
        newVl = floored / cp -- new length in seconds
        rate = vl / newVl
    in realToFrac rate

--
--


--------- Helper Functions ------------

reglaDeTres:: Double -> Double -> Double -> Double
reglaDeTres normScale normPos realScale = (normPos*realScale) / normScale


-- test functions

today = fromGregorian 2019 07 18

myUTCTest = UTCTime today 30

myUTCTest2 = UTCTime today 60

myTempo = Tempo { cps= 0.5, at= myUTCTest, beat= 10.3}


-----------------------------------
-- From Estuary Tempo, this needs to be erased when implemented un Estuario

-- from Estuary Tempo
-- data Tempo = Tempo {
--   cps :: Double,
--   at :: UTCTime, -- anchor for the tempo grid.
--   beat :: Double
--   } deriving (Eq,{- Data,Typeable, -}Show)
--
--
--
--
-- elapsedCycles :: Tempo -> UTCTime -> Double
-- elapsedCycles t now = elapsedT * cps t + beat t
--   where elapsedT = realToFrac $ diffUTCTime now (at t)
--
-- beatZero :: Tempo -> UTCTime
-- beatZero x = addUTCTime (realToFrac $ beat x * (-1) / cps x) (at x)
