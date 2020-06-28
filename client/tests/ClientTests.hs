{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec
import Data.Text (Text)
import Data.Either
import Data.Time
import Estuary.Types.Tempo

import qualified Estuary.Types.Terminal as Terminal

main :: IO ()
main = microspec $ do

  describe "the terminal command parser" $ do
    it "parses an empty string as an error" $ isLeft (Terminal.parseCommand "") `shouldBe` True
    it "parses a chat that begins with a non-! character" $ Terminal.parseCommand "hello" `shouldBe` Right (Terminal.Chat "hello")
    it "parses a StartStreaming command" $ Terminal.parseCommand "!startStreaming" `shouldBe` Right Terminal.StartStreaming

  -- describe "the CineCer0 parser" $ do
  --   it "test1" $ cineCer0 eTime "ramp 3 0 1" `shouldBe` Right $ Spec



-- test video spec

vsTest = VideoSpec {
  sampleVideo = "",
  anchorTime = defaultAnchor,
  playbackPosition = playNatural_Pos 0.0,
  playbackRate = playNatural_Rate 0.0,
  mute = constantSignal True,
  volume = constantSignal 0.0,
  posX = constantSignal 0.0,
  posY = constantSignal 0.0,
  width = constantSignal 1.0,
  height = constantSignal 1.0,
  opacity = constantSignal' Nothing,
  blur = constantSignal Nothing,
  brightness = constantSignal Nothing,
  contrast = constantSignal Nothing,
  grayscale = constantSignal Nothing,
  saturate = constantSignal Nothing,
  mask = emptyText
}

-- test
hoy = fromGregorian 2019 05 04 

unMes = fromGregorian 2019 06 04

timeTempo = UTCTime hoy 0 -- time in which the tempo mark starts counting

renderTime = UTCTime hoy 60 -- one month later the render time is introduced

evalTime = UTCTime hoy 58 -- one month later the evaltime is introduced

anchorTime = defaultAnchor myTempo evalTime -- calculated from the tempo and the evaltime

myTempo = Tempo { freq= 0.5, time= timeTempo, count= 100} 

vl = realToFrac 12.5 :: NominalDiffTime

mark ndt utc = addUTCTime ndt utc