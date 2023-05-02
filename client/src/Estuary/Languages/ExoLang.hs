{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.ExoLang (ExoLang,loadExoLang,awaitExoLang,testExoLang) where

import Data.Text
import GHCJS.DOM.Types hiding (Text)
import Data.IORef
import Data.JSVal.Promise

-- import Estuary.Types.Tempo
-- import Estuary.Render.ForeignTempo

newtype ExoLang = ExoLang (IORef (Either Promise ExoLangObject))

loadExoLang :: Text -> IO ExoLang
loadExoLang path = do
  aPromise <- _loadExoLang path
  x <- newIORef (Left aPromise)
  pure $ ExoLang x

foreign import javascript safe
  "$r = import($1);"
  _loadExoLang :: Text -> IO Promise

awaitExoLang :: ExoLang -> IO ()
awaitExoLang (ExoLang ioref) = do
  x <- readIORef ioref
  case x of
    Left aPromise -> do
      y <- await aPromise
      case y of
        Left _ -> putStrLn "uhoh, error awaiting promise of exolang!!!"
        Right jsModule -> do
          obj <- exoLang jsModule
          writeIORef ioref (Right obj)
    Right _ -> pure ()

foreign import javascript safe
  "$r = $1.exoLang();"
  exoLang :: JSVal -> IO ExoLangObject

newtype ExoLangObject = ExoLangObject JSVal

instance PToJSVal ExoLangObject where pToJSVal (ExoLangObject x) = x

instance PFromJSVal ExoLangObject where pFromJSVal = ExoLangObject

testExoLang :: ExoLang -> IO (Maybe Int)
testExoLang (ExoLang ioref) = do
  x <- readIORef ioref
  case x of
    Left _ -> pure Nothing
    Right obj -> do
      y <- _testExoLang obj
      pure $ pure y

foreign import javascript safe
  "$r = $1.test();"
  _testExoLang :: ExoLangObject -> IO Int


{-

foreign import javascript safe
  "new LocoMotion.LocoMotion($1)"
  newExoLang :: HTMLCanvasElement -> IO ExoLang\

-- Nothing values represent successful evaluation
evaluate :: ExoLang -> Int -> Text -> IO (Maybe Text)
evaluate lm z x = _evaluate lm z x >>= (pure . exoResultToErrorText)

foreign import javascript safe
  "$1.evaluate($2,$3)"
  _evaluate :: ExoLang -> Int -> Text -> IO ExoResult

foreign import javascript safe
  "$1.clearZone($2)"
  clearZone :: ExoLang -> Int -> IO ()

foreign import javascript unsafe
  "$1.preAnimate()"
  preAnimate :: ExoLang -> IO ()

foreign import javascript unsafe
  "$1.animateZone($2)"
  animateZone :: ExoLang -> Int -> IO ()

foreign import javascript unsafe
  "$1.postAnimate()"
  postAnimate :: ExoLang -> IO ()

foreign import javascript unsafe
  "$1.setTempo($2)"
  _setTempo :: ExoLang -> ForeignTempo -> IO ()

setTempo :: ExoLang -> Tempo -> IO ()
setTempo lm x = _setTempo lm $ toForeignTempo x

-}
