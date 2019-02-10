{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main(main) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Map
import Data.Maybe
import Data.Time

import GHCJS.DOM.Types(JSString, HTMLElement(..), HTMLTextAreaElement(..), uncheckedCastTo, fromJSString, toJSString)
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLTextAreaElement as HTMLTextAreaElement

import Estuary.RenderInfo

import Estuary.Protocol.Foreign

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Context
import Estuary.Types.Hint
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Types.Sited
import Estuary.Types.Live
import Estuary.Types.TidalParser
import Estuary.Types.TextNotation
import Estuary.Types.EnsembleResponse

import Estuary.Test.Protocol
import Estuary.Test.Estuary
import qualified Estuary.Test.Dom as DomUtils
import Estuary.Test.Util

import Estuary.Widgets.Navigation

import Test.Hspec

import Harness

-- type code -> hit eval -> expect certain network message
--                       -> expect parse icon

main :: IO ()
main = hspecInHarness $ do
  (protocol, reqStream, respStream) <- runIO $ do
    (protocol, reqStream, respStream) <- estuaryProtocolWithInspectors
    initialize protocol reqStream respStream
    return (protocol, reqStream, respStream)

  describe "clicking the eval button on a minitidal widget" $ do
    it "produces an ensemble request for evaluating the text" $ do
      -- The editors should not take more that 1s to appear after receiving the
      -- expected layout.
      editor <- tryUntilTimeout 1000 5 $ do
        Just (editor :: HTMLElement) <- DomUtils.findMatchingSelectorInDocument ".estuary .page .eightMiddleL .textPatternChain:nth-child(2)"
        return editor

      evalText editor "s \"bd\""

      expectMessage reqStream $ do
        withinMillis 5000
        toMatch $ \case
          EnsembleRequest (ZoneRequest _ definition) ->
            case definition of
              TextProgram (Live (TidalTextNotation MiniTidal, "s \"bd\"") L3) ->
                Matches
              _ -> AlmostMatches
          _ -> DoesNotMatch

      return ()

    it "indicates an error for incorrect code" $ do
      editor <- tryUntilTimeout 1000 5 $ do
        Just (editor :: HTMLElement) <- DomUtils.findMatchingSelectorInDocument ".estuary .page .eightMiddleL .textPatternChain:nth-child(2)"
        return editor

      evalText editor "definitelyNotTidalCode!!!"

      Just (evalBtnContainer :: HTMLElement) <- DomUtils.findMatchingSelector editor ".textInputLabel"
      tryUntilTimeout 100 5 $ do
        (errText :: String) <- DomUtils.immediateText evalBtnContainer
        errText `shouldBe` "!"

initialize :: EstuaryProtocolObject -> Chan (Maybe Request) -> Chan (Maybe Response) -> IO ()
initialize protocol reqStream respStream = do
  silentEstuaryWithInitialPage protocol $ Collaborate "abc"

  let password = "abc"
  performRequest protocol $ EnsembleRequest (AuthenticateInEnsemble password)

  EnsembleResponse (DefaultView view) <- expectMessage respStream $ do
    withinMillis 10000
    toMatch $ \case
      EnsembleResponse (DefaultView view) -> Matches
      _ -> DoesNotMatch

  return ()

evalText :: HTMLElement -> String -> IO ()
evalText editor text = do
  Just (evalBtn :: HTMLElement) <- DomUtils.findMatchingSelector editor "button"
  Just (txtArea :: HTMLTextAreaElement) <- DomUtils.findMatchingSelector editor "textarea"
  
  DomUtils.changeValue txtArea text  
  threadDelay $ 100 * 1000 -- somewhat arbitrary, there needs to be a delay because of a race in the reflex event loop
  DomUtils.click evalBtn