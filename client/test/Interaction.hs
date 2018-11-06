{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main(main) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Map
import Data.Maybe
import Data.Time

import GHCJS.DOM.Types(Element, HTMLElement, HTMLTextAreaElement, castToHTMLElement, fromJSString)
import qualified GHCJS.DOM.HTMLElement as HTMLElement
import qualified GHCJS.DOM.HTMLTextAreaElement as HTMLTextAreaElement

import Estuary.RenderInfo

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
import Estuary.Types.EditOrEval
import Estuary.Types.TidalParser
import Estuary.Types.TextNotation
import Estuary.Types.EnsembleResponse

import Estuary.Test.Protocol
import Estuary.Test.Estuary
import Estuary.Test.Dom

import Estuary.Widgets.Navigation

import Reflex.Dom hiding (link)
import Reflex.Dynamic

import Test.Hspec

-- type code -> hit eval -> expect certain network message
--                       -> expect parse icon

main :: IO ()
main = hspec $ do
  describe "clicking the eval button on a minitidal widget" $ do
    (reqStream, respStream) <- runIO $ do
      (protocol, reqStream, respStream) <- estuaryProtocolWithInspectors

      silentEstuaryWithInitialPage protocol $ Collaborate "abc"

      let password = "abc"
      performRequest protocol $ EnsembleRequest (Sited "abc" (AuthenticateInEnsemble password))
      
      EnsembleResponse (Sited _ (DefaultView view)) <- expectMessage respStream $ do
        withinMillis 10000
        toMatch $ \case
          EnsembleResponse (Sited "abc" (DefaultView view)) -> Matches
          _ -> DoesNotMatch

      -- The editors should not take more that 1s to appear after receiving the
      -- expected layout.
      threadDelay $ 1000 * 1000

      Just (editor :: Element) <- findMatchingSelectorInDocument ".estuary .page .eightMiddleL .textPatternChain:nth-child(2)"
      Just (evalBtn :: HTMLElement) <- findMatchingSelector editor "button"
      Just (txtArea :: HTMLTextAreaElement) <- findMatchingSelector editor "textarea"

      HTMLTextAreaElement.setValue txtArea $ Just "s \"bd\""
      HTMLElement.click evalBtn

      return (reqStream, respStream)

    it "produces an ensemble request for evaluating the text" $ do 
      expectMessage reqStream $ do
        withinMillis 3000
        toMatch $ \case
          EnsembleRequest (Sited _ (ZoneRequest (Sited _ editOrEval))) -> 
            case editOrEval of
              Edit (TextProgram (Live (TidalTextNotation MiniTidal, "s \"bd\"") L3)) ->
                Matches
              _ -> AlmostMatches
          _ -> DoesNotMatch
      return ()
