{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Client.Extension where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad (void,when)
import GHCJS.Types (isUndefined)
import GHCJS.DOM.Types hiding (Text)
import Data.JSVal.Promise
import Control.Exception.Base (throwIO)
import GHCJS.Foreign.Callback (Callback, syncCallback3, OnBlocked(ContinueAsync))

import Estuary.Types.AsyncValue
import Estuary.Types.JSException
import Estuary.Types.Tempo
import Estuary.Render.ForeignTempo
import Estuary.Types.NoteEvent
import Estuary.Render.Renderer (Renderer)
import qualified Estuary.Render.Renderer as Renderer
import Estuary.Types.TextNotation
import Estuary.Types.Definition
import Estuary.Resources (addResourceOp,Resources)
import Estuary.Types.ResourceOp (ResourceOp(InsertResource))
import Estuary.Types.ResourceType (ResourceType(Audio))

newtype Extension = Extension JSVal
instance PToJSVal Extension where pToJSVal (Extension x) = x
instance PFromJSVal Extension where pFromJSVal = Extension

type API = Callback (JSVal -> JSVal -> JSVal -> IO ())
{-
newtype API = API JSVal
instance PToJSVal API where pToJSVal (API x) = x
instance PFromJSVal API where pFromJSVal = API
-}

foreign import javascript safe
  "$r = importExoLang($1);"
  extensionPromise :: Text -> IO Promise

foreign import javascript safe "$r = $1.onLaunch($2);" _onLaunch :: Extension -> API -> IO JSVal

anAPIcall :: JSVal -> IO ()
anAPIcall j = do
  Just str <- fromJSVal j
  print $ "anAPIcall: " ++ str
  
insertSound :: Resources -> JSVal -> JSVal -> JSVal -> IO ()
insertSound resources url bankName n = do
  Just url' <- fromJSVal url
  Just bankName' <- fromJSVal bankName
  Just n' <- fromJSVal n
  addResourceOp resources $ InsertResource Audio url' (bankName',n')
  print "completed addResourceOp"
-- addResourceOp :: MonadIO m => Resources -> ResourceOp -> m ()
-- data ResourceOp = InsertResource ResourceType Text Location 

testExtension :: Resources -> Text -> IO ()
testExtension resources path = do
  api <- syncCallback3 ContinueAsync (insertSound resources)
  p <- extensionPromise path
  r <- await p
  case r of
    Left j -> throwIO (JSException j)
    Right j -> do
      let ext = pFromJSVal j
      putStrLn $ "loaded extension from " ++ unpack path
      _onLaunch ext api 
      pure ()

