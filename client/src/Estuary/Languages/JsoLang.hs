{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.JsoLang (JsoLang, create, parse) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Nullable
import Data.Text

newtype JsoLang = JsoLang JSVal

instance PToJSVal JsoLang where pToJSVal (JsoLang x) = x

instance PFromJSVal JsoLang where pFromJSVal = JsoLang

foreign import javascript safe
  "eval(\"{parse:function(input){\" + $1 + \"}}\")"
  create :: Text -> IO JsoLang
  -- but really this needs to be safer, more along the lines of parse below
  -- ie. IO (Either Text JsoLang)

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
  "$1.parse($2)"
  _parse :: JsoLang -> Text -> IO JSVal

foreign import javascript safe
  "$1.error"
  _error :: JSVal -> IO (Nullable Text)

foreign import javascript safe
  "$1.ok"
  _ok :: JSVal -> IO (Nullable Text)
