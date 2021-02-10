{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.JSoLang (JSoLang, define, parse) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Text
import Control.Exception

newtype JSoLang = JSoLang JSVal

instance PToJSVal JSoLang where pToJSVal (JSoLang x) = x

instance PFromJSVal JSoLang where pFromJSVal = JSoLang

foreign import javascript safe
  "peg.generate($1)"
  _define :: Text -> IO JSoLang

define :: Text -> IO (Either Text JSoLang)
define x =
  (_define x >>= (return . Right))
  `catch` (\e -> return $ Left $ pack $ show (e :: SomeException))

parse :: JSoLang -> Text -> IO (Either Text Text)
parse jsoLang input =
  (_parse jsoLang input >>= (return . Right))
  `catch` (\e -> return $ Left $ pack $ show (e :: SomeException))

foreign import javascript safe
  "$1.parse($2)"
  _parse :: JSoLang -> Text -> IO Text
