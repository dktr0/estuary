{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.ExoLang
  (ExoLang,loadExoLang,awaitExoLang,ExoResult,exoResultToErrorText,utcTimeToWhenPOSIX,
  evaluate,clearZone,preAnimate,animateZone,postAnimate,setTempo) where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad (void)
import GHCJS.DOM.Types hiding (Text)
import Data.IORef
import Data.JSVal.Promise

import Estuary.Types.Tempo
import Estuary.Render.ForeignTempo


newtype ExoLang = ExoLang (IORef (Either (Text,Promise,HTMLCanvasElement) ExoLangObject))

loadExoLang :: HTMLCanvasElement -> Text -> IO ExoLang
loadExoLang cvs path = do
  aPromise <- _loadExoLang path
  x <- newIORef $ Left (path,aPromise,cvs)
  pure $ ExoLang x

foreign import javascript safe
  "$r = import($1);"
  _loadExoLang :: Text -> IO Promise

-- block until an ExoLang has finished loading
awaitExoLang :: ExoLang -> IO ()
awaitExoLang (ExoLang ioref) = do
  x <- readIORef ioref
  case x of
    Left (path,aPromise,cvs) -> do
      y <- await aPromise
      case y of
        Left _ -> putStrLn $ "ERROR: couldn't load exoLang at " ++ unpack path
        Right jsModule -> do
          obj <- exoLang jsModule cvs
          writeIORef ioref (Right obj)
          putStrLn $ "loaded exoLang at " ++ unpack path
    Right _ -> pure ()

-- do something with an ExoLang (if it is not finished loading yet, wait for it to do so)
withExoLang :: ExoLang -> (ExoLangObject -> IO a) -> IO (Either Text a)
withExoLang (ExoLang ioref) f = do
  x <- readIORef ioref
  case x of
    Left (path,aPromise,cvs) -> do
      y <- await aPromise
      case y of
        Left _ -> pure $ Left $ "ERROR: couldn't load exoLang at " <> path
        Right jsModule -> do
          obj <- exoLang jsModule cvs
          writeIORef ioref (Right obj)
          putStrLn $ "loaded exoLang at " ++ unpack path
          Right <$> f obj
    Right obj -> Right <$> f obj

foreign import javascript safe
  "$r = $1.exoLang($2);"
  exoLang :: JSVal -> HTMLCanvasElement -> IO ExoLangObject

newtype ExoLangObject = ExoLangObject JSVal

instance PToJSVal ExoLangObject where pToJSVal (ExoLangObject x) = x

instance PFromJSVal ExoLangObject where pFromJSVal = ExoLangObject


-- Nothing values represent successful evaluation
evaluate :: ExoLang -> Int -> Text -> IO (Maybe Text)
evaluate x z txt = do
  r <- withExoLang x $ _evaluate z txt
  case r of
    Left err -> pure (Just err)
    Right exoResult -> do
      case exoResultToErrorText exoResult of
        Just err -> pure (Just err)
        Nothing -> pure Nothing

foreign import javascript safe
  "$3.evaluate($1,$2)"
  _evaluate :: Int -> Text -> ExoLangObject -> IO ExoResult


clearZone :: ExoLang -> Int -> IO ()
clearZone x z = void $ withExoLang x $ _clearZone z

foreign import javascript safe
  "$2.clearZone($1)"
  _clearZone :: Int -> ExoLangObject -> IO ()


preAnimate :: ExoLang -> IO ()
preAnimate x = void $ withExoLang x _preAnimate

foreign import javascript unsafe
  "$1.preAnimate()"
  _preAnimate :: ExoLangObject -> IO ()


animateZone :: ExoLang -> Int -> IO ()
animateZone x z = void $ withExoLang x $ _animateZone z

foreign import javascript unsafe
  "$2.animateZone($1)"
  _animateZone :: Int -> ExoLangObject -> IO ()


postAnimate :: ExoLang -> IO ()
postAnimate x = void $ withExoLang x _postAnimate

foreign import javascript unsafe
  "$1.postAnimate()"
  _postAnimate :: ExoLangObject -> IO ()


setTempo :: ExoLang -> Tempo -> IO ()
setTempo x t = void $ withExoLang x $ _setTempo $ toForeignTempo t

foreign import javascript unsafe
  "$2.setTempo($1)"
  _setTempo :: ForeignTempo -> ExoLangObject -> IO ()


newtype ExoResult = ExoResult JSVal

instance PToJSVal ExoResult where pToJSVal (ExoResult x) = x

instance PFromJSVal ExoResult where pFromJSVal = ExoResult

foreign import javascript safe
  "$1.success"
  _exoResultSuccess :: ExoResult -> Bool

foreign import javascript safe
  "$1.error"
  _exoResultError :: ExoResult -> Text

exoResultToErrorText :: ExoResult -> Maybe Text
exoResultToErrorText x = case _exoResultSuccess x of
  True -> Nothing
  False -> Just $ _exoResultError x


utcTimeToWhenPOSIX :: UTCTime -> Double
utcTimeToWhenPOSIX = realToFrac . utcTimeToPOSIXSeconds
