{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec
import Data.Text (Text)
import Data.Either

import qualified Estuary.Types.Terminal as Terminal

main :: IO ()
main = microspec $ do

  describe "the terminal command parser" $ do
    it "parses an empty string as an error" $ isLeft (Terminal.parseCommand "") `shouldBe` True
    it "parses a chat that begins with a non-! character" $ Terminal.parseCommand "hello" `shouldBe` Right (Terminal.Chat "hello")
    it "parses a StartStreaming command" $ Terminal.parseCommand "!startStreaming" `shouldBe` Right Terminal.StartStreaming

  describe "the CineCer0 parser" $ do
    it "needs some tests" $ (2+2) `shouldBe` 4
