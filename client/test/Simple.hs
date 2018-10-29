module Main(main) where

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.IO.Class

import Data.Map
import Data.Maybe
import Data.Time

import GHCJS.DOM.Types(Element, HTMLElement, castToHTMLElement, fromJSString)
import GHCJS.DOM.HTMLElement(getInnerText)

import Estuary.RenderInfo
import Estuary.Types.Context
import Estuary.WebDirt.WebDirt(WebDirt, newWebDirt)
import Estuary.WebDirt.SuperDirt(SuperDirt, newSuperDirt)
import Estuary.Widgets.Estuary(header)

import Reflex.Dom hiding (link)
import Reflex.Dynamic

import Test.Hspec

-- main :: IO ()
-- main = do
--   hspec $ do
--     describe "header" $ do
--       it "changes the header when the language changes" $
--         headerShould $ \header -> do
--           text <- elInnerText header
--           return $ text `shouldStartWith` "BLAH"

main :: IO ()
main = do
  hspec $ do
    describe "header" $ do
      it "changes the header when the language changes" $ do
        h <- renderHeaderSync
        text <- elInnerText h
        text `shouldStartWith` "BLAH"

renderHeaderSync :: IO HTMLElement
renderHeaderSync = do
  resultContainer <- newEmptyMVar
  finishedRender <- async $ takeMVar resultContainer
  link finishedRender -- any exceptions should be thrown here

  ctx <- initCtx
  mainWidget $ do
    let dynCtx = constDyn ctx
    let dynRenferInfo = constDyn emptyRenderInfo

    (e, _) <- elAttr' "div" (fromList [("id", "test-container")]) $ do
      header dynCtx dynRenferInfo

    let containerEl = castToHTMLElement $ _el_element e
    postBuildEv <- getPostBuild
    performEvent_ $ ffor postBuildEv $ \_ -> liftIO $ do
      putMVar resultContainer containerEl

  wait finishedRender

-- type code -> hit eval -> expect certain network message
--                       -> expect parse icon

-- headerShould :: (HTMLElement -> IO a) -> IO ()
-- headerShould test = do
--   ctx <- initCtx
--   mainWidget $ do
--     let dynCtx = constDyn ctx
--     let dynRenferInfo = constDyn emptyRenderInfo

--     (e, _) <- elAttr' "div" (fromList [("id", "test-container")]) $ do
--       header dynCtx dynRenferInfo

--     let containerEl = castToHTMLElement $ _el_element e
--     checkWidgetState $ test containerEl >> return ()
--   putStrLn "Done"

checkWidgetState :: (MonadWidget t m) => IO () -> m ()
checkWidgetState action = do
  --doQuit <- getQuitWidget
  postBuildEv <- getPostBuild
  performEvent_ $ ffor postBuildEv $ \_ -> do
    liftIO $ action >> putStrLn "Done check"
    --doQuit

initCtx :: IO Context
initCtx = do
  now <- Data.Time.getCurrentTime
  let ctx = initialContext now nullWebDirt nullSuperDirt
  return $ ctx {
    webDirtOn = False
  }



elInnerText :: HTMLElement -> IO (String)
elInnerText el = do
  mTextJs <- getInnerText $ el
  return $ maybe "" id mTextJs

foreign import javascript safe
  "null"
  nullWebDirt :: WebDirt

foreign import javascript safe
  "null"
  nullSuperDirt :: SuperDirt

-- foreign import javascript safe
--   "require('puppeteer')"
--   importPuppeteer :: IO (JSVal)