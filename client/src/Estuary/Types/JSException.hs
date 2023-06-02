module Estuary.Types.JSException where

import Control.Exception.Base (Exception)
import GHCJS.DOM.Types hiding (Text)

data JSException = JSException JSVal

instance PToJSVal JSException where pToJSVal (JSException x) = x

instance PFromJSVal JSException where pFromJSVal = JSException

instance Show JSException where
  show (JSException _) = "A JSException"

instance Exception JSException

