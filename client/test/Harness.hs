{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Harness where

import Data.ByteString
import Data.ByteString.Handle

import Test.Hspec
import Test.Hspec.Runner

import GHC.Generics

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure

import qualified Data.ByteString.Lazy.Char8 as C8

data TestResults = TestResults {
  report :: String,
  success :: Bool
} deriving (Generic, FromJSVal, ToJSVal)

hspecInHarness :: Spec -> IO ()
hspecInHarness spec = do
  inHarness <- isInHarness
  if inHarness then do
    results <- capturedHspec spec
    putResults results
  else do
    hspec spec

capturedHspec :: Spec -> IO TestResults
capturedHspec spec = do
  (output, summary) <- writeHandle False $ \h -> do
    (flip hspecWithResult) spec defaultConfig {
        configOutputFile = Left h,
        configColorMode = ColorAlways
      }

  return $ TestResults {
    report = C8.unpack output,
    success = summaryFailures summary == 0
  }

putResults :: TestResults -> IO ()
putResults results = do
  val <- toJSVal results
  js_putResults val

foreign import javascript unsafe
  "Boolean(window['ESTUARY_TEST_HARNESS_ENABLED'])"
  isInHarness :: IO Bool

foreign import javascript unsafe
  "window['ESTUARY_TEST_HARNESS_RESULTS_CB']($1)"
  js_putResults :: JSVal -> IO ()