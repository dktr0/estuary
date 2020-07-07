{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec
import Data.Text (Text)
import Data.Either
import Data.Time
import Data.IntMap as IntMap
import Estuary.Types.Tempo
import Estuary.Languages.CineCer0.Signal
import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Parser
import Estuary.Languages.CineCer0.Spec

import qualified Estuary.Types.Terminal as Terminal

main :: IO ()
main = microspec $ do

  describe "the terminal command parser" $ do
    it "parses an empty string as an error" $ isLeft (Terminal.parseCommand "") `shouldBe` True
    it "parses a chat that begins with a non-! character" $ Terminal.parseCommand "hello" `shouldBe` Right (Terminal.Chat "hello")
    it "parses a StartStreaming command" $ Terminal.parseCommand "!startStreaming" `shouldBe` Right Terminal.StartStreaming

  describe "the CineCer0 parser" $ do
    it "parses an empty string" $ fmap (IntMap.null . videoSpecMap) (cineCer0 eTime "") `shouldBe` Right True
    it "parses just a comment" $ fmap (IntMap.null . videoSpecMap) (cineCer0 eTime "{-comment-}") `shouldBe` Right True
    it "parses empty statements" $ fmap (IntMap.null . videoSpecMap) (cineCer0 eTime ";;") `shouldBe` Right True
    it "name of video test" $ fmap (fmap sampleVideo . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "\"myMovie.mov\"") `shouldBe` Right (Just "myMovie.mov")
    it "vol parser test" $ fmap (fmap (\vs -> (volume vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "vol 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "posX parser test" $ fmap (fmap (\vs -> (posX vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "setPosX 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "posY parser test" $ fmap (fmap (\vs -> (posY vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "setPosY 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "width parser test" $ fmap (fmap (\vs -> (width vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "width 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "height parser test" $ fmap (fmap (\vs -> (height vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "height 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just 0.5)
    it "opacity parser test" $ fmap (fmap (\vs -> (opacity vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "opacity 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "blur parser test" $ fmap (fmap (\vs -> (blur vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "blur 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "contrast parser test" $ fmap (fmap (\vs -> (contrast vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "contrast 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "grayscale parser test" $ fmap (fmap (\vs -> (grayscale vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "grayscale 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))
    it "saturate parser test" $ fmap (fmap (\vs -> (saturate vs) tempoTest vidlen rTime eTime aTime) . IntMap.lookup 0 . videoSpecMap) (cineCer0 eTime "saturate 0.5 $ \"myMovie.mov\"") `shouldBe` Right (Just (Just 0.5))


-- test
hoy = fromGregorian 2019 05 04

unMes = fromGregorian 2019 06 04

timeTempo = UTCTime hoy 0 -- time in which the tempo mark starts counting

rTime = UTCTime hoy 60 -- one month later the render time is introduced

eTime = UTCTime hoy 58 -- one month later the evaltime is introduced

aTime = defaultAnchor myTempo eTime -- calculated from the tempo and the evaltime

tempoTest = Tempo { freq= 0.5, time= timeTempo, count= 100}

vidlen = realToFrac 12.5 :: NominalDiffTime

mark ndt utc = addUTCTime ndt utc
