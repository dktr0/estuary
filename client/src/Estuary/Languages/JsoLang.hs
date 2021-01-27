{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.JsoLang (JsoLang, create, parse) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Nullable
import Data.Text
import Control.Exception

newtype JsoLang = JsoLang JSVal

instance PToJSVal JsoLang where pToJSVal (JsoLang x) = x

instance PFromJSVal JsoLang where pFromJSVal = JsoLang

foreign import javascript safe
  "new Function('input',$1)"
  _create :: Text -> IO (Nullable JsoLang)

create :: Text -> IO (Either Text JsoLang)
create x = do
  (do
    y <- nullableToMaybe <$> _create x
    case y of
      Just j -> return $ Right j
      Nothing -> return $ Left "strange error: evaluating parser returned null"
   ) `catch` (\e -> return $ Left $ pack $ show (e :: SomeException))

parse :: JsoLang -> Text -> IO (Either Text Text)
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
  _parse :: JsoLang -> Text -> IO JSVal

foreign import javascript safe
  "$1.error"
  _error :: JSVal -> IO (Nullable Text)

foreign import javascript safe
  "$1.ok"
  _ok :: JSVal -> IO (Nullable Text)
