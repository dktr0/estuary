{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.MiniTidalTests where

import Test.Hspec
import Estuary.Languages.MiniTidal
import Sound.Tidal.Context as Tidal

parsesTo :: String -> ParamPattern -> Expectation
parsesTo s p = (arc (miniTidalParser s) $ (0,16)) `shouldBe` (arc p $ (0,16))

main :: IO ()
main = hspec $ do

  describe "miniTidalParser" $ do

    it "parses the empty string as silence" $
      "" `parsesTo` silence

    it "parses a string containing only spaces as silence" $
      "    " `parsesTo` silence

    it "parses a single 's' pattern" $
      "s \"bd cp\"" `parsesTo` s "bd cp"

    it "parses two merged patterns" $
      "s \"bd cp\" # pan \"0 1\"" `parsesTo` (s "bd cp" # pan "0 1")

    it "parses three merged patterns" $
      "s \"bd cp\" # pan \"0 1\" # gain \"0.5 0.7\"" `parsesTo`
        (s "bd cp" # pan "0 1" # gain "0.5 0.7")

    it "parses three merged patterns with right associative brackets" $
      "s \"bd cp\" # (pan \"0 1\" # gain \"0.5 0.7\")" `parsesTo`
        (s "bd cp" # (pan "0 1" # gain "0.5 0.7"))

    it "parses three merged patterns with left associative brackets" $
      "(s \"bd cp\" # pan \"0 1\") # gain \"0.5 0.7\"" `parsesTo`
        ((s "bd cp" # pan "0 1") # gain "0.5 0.7")

    it "parses simple patterns in brackets applied to ParamPattern functions" $
      "s (\"bd cp\")" `parsesTo` (s ("bd cp"))

    it "parses simple patterns applied to ParamPattern functions with $" $
      "s $ \"bd cp\"" `parsesTo` (s $ "bd cp")

    it "parses addition of simple patterns" $
      "n (\"0 1\" + \"2 3\")" `parsesTo` (n ("0 1" + "2 3"))

    it "parses multiplication of simple patterns as a merged parampattern" $
      "s \"arpy*8\" # up (\"3\" * \"2\")" `parsesTo` (s "arpy*8" # up ("3" * "2"))

    it "parses pan patterns" $
      "pan \"0 0.25 0.5 0.75 1\"" `parsesTo` (pan "0 0.25 0.5 0.75 1")

    it "parses sinewave oscillators" $
      "pan sinewave" `parsesTo` (pan sinewave)

    it "parses sinewave1 oscillators" $
      "pan sinewave1" `parsesTo` (pan sinewave1)

    it "parses sinewave1 oscillators used in pan patterns" $
      "s \"arpy*8\" # pan sinewave1" `parsesTo` (s "arpy*8" # pan sinewave1)

    it "parses fast transformations of parampatterns" $
      "fast 2 $ s \"bd cp\"" `parsesTo` (fast 2 $ s "bd cp")
