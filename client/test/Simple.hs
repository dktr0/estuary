module Main(main) where

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Map
import Data.Maybe
import Data.Time

import GHCJS.DOM.Types(IsHTMLElement, fromJSString)
import GHCJS.DOM.HTMLElement(getInnerText)

import Estuary.RenderInfo
import Estuary.Types.Context
import Estuary.WebDirt.WebDirt(WebDirt, newWebDirt)
import Estuary.WebDirt.SuperDirt(SuperDirt, newSuperDirt)
import Estuary.Widgets.Estuary(header)

import Estuary.Test.Reflex(renderSync)
import Estuary.Test.Dom
import Estuary.Test.Estuary

import Reflex.Dom hiding (link)
import Reflex.Dynamic

import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    describe "header" $ do
      it "changes the header when the language changes" $ do
        ctx <- initCtx
        h <- renderSync $ do
          let dynCtx = constDyn ctx
          let dynRenferInfo = constDyn emptyRenderInfo
          header dynCtx dynRenferInfo
          return ()
        text <- elInnerText h
        text `shouldStartWith` "BLAH"

elInnerText :: (IsHTMLElement e) => e -> IO (String)
elInnerText el = do
  mTextJs <- getInnerText $ el
  return mTextJs

foreign import javascript safe
  "null"
  nullWebDirt :: WebDirt

foreign import javascript safe
  "null"
  nullSuperDirt :: SuperDirt
