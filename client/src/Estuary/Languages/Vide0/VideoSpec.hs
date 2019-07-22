module Estuary.Languages.Vide0.VideoSpec where

import Language.Haskell.Exts
import Control.Applicative
import Data.Time

import qualified Estuary.Languages.Vide0.PositionAndRate as VT

import Estuary.Types.Tempo

data VideoSpec = VideoSpec {
  sampleVideo :: String,
  sourceNumber :: Int,
  timePosition :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime,
  timeRate :: Tempo -> NominalDiffTime -> UTCTime -> NominalDiffTime
  }

instance Show VideoSpec where
  show (VideoSpec vs n _ _) = show vs ++ show n

--instance Show InputsToNetworks where
-- show (InputsToNetworks rs ps es) = "rolesInputs " ++ (show rs) ++ --"pitchRecognitionInputs " ++ (show ps) ++ "energyInputs " ++ (show es)


stringToVideoSpec :: String -> VideoSpec
stringToVideoSpec x = VideoSpec {
  sampleVideo = x,
  sourceNumber = 0,
  timePosition = VT.playNatural_Pos,
  timeRate = VT.playNatural_Rate
}

setSourceNumber :: VideoSpec -> Int -> VideoSpec
setSourceNumber vs n = vs { sourceNumber = n }


-- Time Functions --

playEvery :: Int -> VideoSpec -> VideoSpec
playEvery n vs = vs {
  timePosition = VT.playEvery_Pos n,
  timeRate = VT.playEvery_Rate n
  }

playRound :: VideoSpec -> VideoSpec
playRound vs = vs {
  timePosition = VT.playRound_Pos,
  timeRate = VT.playRound_Rate
  }
