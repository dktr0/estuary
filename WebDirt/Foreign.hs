{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE TypeFamilies #-} -- @I don't remember what this was for, might not need....
{-# LANGUAGE OverloadedStrings  #-}

module Main where
import Reflex
import Reflex.Dom
import Control.Monad
import Data.Map
import Control.Monad.IO.Class
import Control.Monad.Ref
import GHC.IORef
import qualified GHCJS.Prim as Prim

import qualified Data.Maybe
import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import  GHCJS.Foreign.Internal


--Not sure what 'unsafe' key means
foreign import javascript unsafe "test()" writeHello :: IO () -- Have to give it a type here
foreign import javascript unsafe "onload()" onLoad :: IO()
foreign import javascript unsafe "$r = new WebDirt(null,null,null, function() {console.log('callback from WebDirt constructor completed');});" newWebDirt ::IO(T.JSVal)
-- $1, $2 etc... are where haskell function parameters are passed
foreign import javascript unsafe "$1.queue({ sample_name: 'cp', sample_n:0});" simpleMessage ::T.JSVal->IO (T.JSVal)
foreign import javascript unsafe "$1.queue({sample_name: $2, sample_n:0});" playSample::T.JSVal ->T.JSVal -> IO()

foreign import javascript unsafe "$r = {a:'lol' ,b:3}" testString::IO T.JSVal
foreign import javascript unsafe "window.onload = alert('loaded the page');" onloadTest::IO()
foreign import javascript unsafe "console.log($1);" consoleLog::T.JSString -> IO ()



main = do
  webDirt <- newWebDirt
  mainWidget $ simpleWidget webDirt


--widget::MonadWidget t m => T.JSString -> m(Event t (IO ()))


--simpleWidget::MonadWidget t m=>T.JSVal-> m()
simpleWidget webDirtObj = el "div" $ do
  --playButton<- liftM (liftIO (playSample webDirtObj "cp")) $ button "Play"
  --text " Sample Name: "
  sampleName <- textInput def
  let sampleName' = _textInput_value sampleName -- dynamic string
  --sampleName'' <- forDyn sampleName' (\x -> playSample webDirtObj (Prim.toJSString x)) --dynamic t (IO())
  playButton <- forDyn sampleName' (\k -> liftM (playSample webDirtObj (Prim.toJSString k)<$) $ button "Play") -- Dynamic (Event t ( IO ()))
  let playButton' = switchPromptlyDyn playButton -- Event t (IO ())
  performEvent playButton'
  return ()
  --
  -- let sampleName' = updated (_textInput_value sampleName) -- Event ([string])
  --
  --
  --
  --
  -- x <- fmap (\k -> playSample webDirtObj (Prim.toJSString k)) sampleName' -- Event (IO())
  -- playButton <- liftM (x<$) $ button "Play" ::MonadWidget t m => m(Event t (IO()))
  -- a<-coin
  -- performEvent playButton
  --

  --display sampleName'
  --let ioevent = forDyn sampleName' (\k-> playSample webDirtObj (k::T.JSString))
--  playButton <- liftM (liftIO (playSample webDirtObj sampleName')) $ button "Play"
--  x <- forDyn sampleName'' (\k-> liftM (liftIO (playSample webDirtObj k) <$) =<< button "play")
  --performEvent playButton


--toJSString::String->T.JSString
--toJSString s =

widget::MonadWidget t m=>T.JSVal-> m()
widget webDirtObj = el "div" $ do
  playButton<- liftM (liftIO (simpleMessage webDirtObj) <$) $ button "Play cp:0"
  performEvent playButton
  return ()

consolePrintButton::(MonadWidget t m) =>T.JSString -> m ()
consolePrintButton s = el "div" $ do
  a <- liftM (liftIO (consoleLog s) <$) $ button "Printsomething" -- ::MonadWidget t m=> m (Event t (m ()))
  performEvent (a)
  return ()



-- performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
-- performArg f x = performEvent (fmap (liftIO . f) x)
-- perfEvn.. (Event t(m(a))    where m = MonadIO
