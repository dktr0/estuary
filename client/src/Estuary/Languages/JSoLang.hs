{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.JSoLang (JSoLang, create, parse) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Nullable
import Data.Text
import Control.Exception

newtype JSoLang = JSoLang JSVal

instance PToJSVal JSoLang where pToJSVal (JSoLang x) = x

instance PFromJSVal JSoLang where pFromJSVal = JSoLang

foreign import javascript safe
  "new Function('input',$1)"
  _create :: Text -> IO (Nullable JSoLang)

create :: Text -> IO (Either Text JSoLang)
create x = do
  (do
    y <- nullableToMaybe <$> _create x
    case y of
      Just j -> return $ Right j
      Nothing -> return $ Left "strange error: evaluating parser returned null"
   ) `catch` (\e -> return $ Left $ pack $ show (e :: SomeException))

parse :: JSoLang -> Text -> IO (Either Text Text)
parse jsoLang input = do
  x <- _parse jsoLang input
  ok <- nullableToMaybe <$> _ok x
  case ok of
    Just r -> return $ Right r
    Nothing -> do
      error <- nullableToMaybe <$> _error x
      case error of
        Just e -> return $ Left e
        Nothing -> return $ Left "jsolang: .parse did not return a valid object"

foreign import javascript safe
  "$1($2)"
  _parse :: JSoLang -> Text -> IO JSVal

foreign import javascript safe
  "$1.error"
  _error :: JSVal -> IO (Nullable Text)

foreign import javascript safe
  "$1.ok"
  _ok :: JSVal -> IO (Nullable Text)
