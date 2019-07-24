module Estuary.Languages.Vide0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time

import qualified Estuary.Languages.Vide0.PositionAndRate as VT

import Estuary.Types.Tempo

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  playbackPosition :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime,
  --playbackRate :: Tempo -> NominalDiffTime -> UTCTime -> Rational, --bueno
  playbackRate :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime, --borrar
  posX :: Rational,
  posY :: Rational
  }

instance Show VideoSpec where
  show (VideoSpec vs n _ _ px py) = "Sample Video:" ++ show vs ++ " " ++ "Source Number:" ++ show n ++ " " ++ "Position:" ++ show px ++ show py

--instance Show InputsToNetworks where
-- show (InputsToNetworks rs ps es) = "rolesInputs " ++ (show rs) ++ --"pitchRecognitionInputs " ++ (show ps) ++ "energyInputs " ++ (show es)


stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0,
  playbackPosition = VT.playNatural_Pos,
  playbackRate = VT.playNatural_Rate,
  posX = 0,
  posY = 0
}

setSourceNumber :: VideoSpec -> Int -> VideoSpec
setSourceNumber vs n = vs { sourceNumber = n }

setPosCoord :: Rational -> Rational -> VideoSpec -> VideoSpec
setPosCoord n m vs = vs { posX = n, posY = m }

--setPosCoord' :: VideoSpec -> Rational -> VideoSpec
--setPosCoord' vs n = vs { posCoordinates = n }

-- Time Functions --

--playEvery :: Rational -> VideoSpec -> VideoSpec --bueno
playEvery :: Int -> VideoSpec -> VideoSpec --borrar
playEvery n vs = vs {
  playbackPosition = VT.playEvery_Pos n,
  playbackRate = VT.playEvery_Rate n
  }

playRound :: VideoSpec -> VideoSpec
playRound vs = vs {
  playbackPosition = VT.playRound_Pos,
  playbackRate = VT.playRound_Rate
  }

--playChop' :: Int -> Int -> VideoSpec -> VideoSpec
--playChop' n n vs = vs {
--  timePosition = VT.something n
--  timeRate = VT.something n
--  }

--playChop :: Int -> Int -> Int -> VideoSpec -> VideoSpec
--playChop n n n vs = vs {
--  timePosition = VT.something n
--  timeRate = VT.something n
--  }
