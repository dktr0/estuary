{-# LANGUAGE LambdaCase #-}

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
import Estuary.Types.Terminal
import Estuary.Types.Tempo
import Estuary.Types.Sited
import Estuary.Types.Live
import Estuary.Types.EditOrEval
import Estuary.Types.TidalParser
import Estuary.Types.TextNotation

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
      protocol <- silentEstuaryWithInitialPage $ Collaborate "abc"
      (reqStream, respStream) <- attachProtocolInspectors protocol

      threadDelay $ 1000 * 1000

      Just editor  <- findMatchingSelectorInDocument ".estuary .page .eightMiddleL .textPatternChain:nth-child(2)"
      Just evalBtn <- findMatchingSelector (editor :: Element) "button"
      Just txtArea <- findMatchingSelector (editor :: Element) "textarea"

      HTMLTextAreaElement.setValue (txtArea :: HTMLTextAreaElement) $ Just "s \"bd\""
      HTMLElement.click (evalBtn :: HTMLElement)

      return (reqStream, respStream)

    it "produces an ensemble request for evaluating the text" $ do 
      expectRequest reqStream $ do
        withinMillis 1000
        toMatch $ \case
          EnsembleRequest (Sited _ (ZoneRequest (Sited _ editOrEval))) -> 
            case editOrEval of
              Evaluate (TextProgram (Live (TidalTextNotation MiniTidal, "s \"bd\"") L3)) ->
                Matches
              _ -> AlmostMatches
          _ -> DoesNotMatch