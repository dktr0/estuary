{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Client.Extension where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad (void,when)
import GHCJS.Types (isUndefined,jsval)
import GHCJS.DOM.Types hiding (Text)
import Data.JSVal.Promise
import Control.Exception.Base (throwIO)
import GHCJS.Foreign.Callback (Callback, syncCallback, syncCallback1, syncCallback2, syncCallback3, OnBlocked(ContinueAsync))
import JavaScript.Object

import Estuary.Types.AsyncValue
import Estuary.Types.JSException
import Estuary.Types.Tempo
import Estuary.Render.ForeignTempo
import Estuary.Types.NoteEvent
import Estuary.Render.Renderer (Renderer)
import qualified Estuary.Render.Renderer as Renderer
import Estuary.Types.TextNotation
import Estuary.Types.Definition
import Estuary.Resources (clearResourceOps,addResourceOp,setDefaultResourceOps,Resources)
import Estuary.Types.ResourceOp (ResourceOp(..))
import Estuary.Types.ResourceType (ResourceType(..))

newtype Extension = Extension JSVal
instance PToJSVal Extension where pToJSVal (Extension x) = x
instance PFromJSVal Extension where pFromJSVal = Extension

newtype API = API JSVal
instance PToJSVal API where pToJSVal (API x) = x
instance PFromJSVal API where pFromJSVal = API

foreign import javascript safe
  "$r = importExoLang($1);"
  extensionPromise :: Text -> IO Promise

foreign import javascript safe "$r = $1.onLaunch($2);" _onLaunch :: Extension -> API -> IO JSVal

clearResources :: Resources -> IO ()
clearResources resources = do
  clearResourceOps resources 
  print "Estuary ensemble extension did clearResources"

defaultResources :: Resources -> IO ()
defaultResources resources = do
  setDefaultResourceOps resources
  print "Estuary ensemble extension did defaultResources"

reslist :: Resources -> JSVal -> IO ()
reslist resources url = do
  Just url' <- fromJSVal url
  addResourceOp resources $ ResourceListURL url'
  print "Estuary ensemble extension did reslist"
  
insertSound :: Resources -> JSVal -> JSVal -> JSVal -> IO ()
insertSound resources url bankName n = do
  Just url' <- fromJSVal url
  Just bankName' <- fromJSVal bankName
  Just n' <- fromJSVal n
  addResourceOp resources $ InsertResource Audio url' (bankName',n')
  print "Estuary ensemble extension did insertSound"
  
deleteSound :: Resources -> JSVal -> JSVal -> IO ()
deleteSound resources bankName n = do
  Just bankName' <- fromJSVal bankName
  Just n' <- fromJSVal n
  addResourceOp resources $ DeleteResource Audio (bankName',n')
  print "Estuary ensemble extension did deleteSound"

appendSound :: Resources -> JSVal -> JSVal -> IO ()
appendSound resources url bankName = do
  Just url' <- fromJSVal url
  Just bankName' <- fromJSVal bankName
  addResourceOp resources $ AppendResource Audio url' bankName'
  print "Estuary ensemble extension did appendSound"

exolang :: Resources -> JSVal -> JSVal -> IO ()
exolang resources name url = do
  Just name' <- fromJSVal name
  Just url' <- fromJSVal url
  addResourceOp resources $ InsertResource ExoLang url' (name',0) 
  print "Estuary ensemble extension did exolang"
  
newAPI :: Resources -> IO API
newAPI resources = do
  clearResources' <- syncCallback ContinueAsync (clearResources resources)
  defaultResources' <- syncCallback ContinueAsync (defaultResources resources)
  reslist' <- syncCallback1 ContinueAsync (reslist resources)
  insertSound' <- syncCallback3 ContinueAsync (insertSound resources)
  deleteSound' <- syncCallback2 ContinueAsync (deleteSound resources)
  appendSound' <- syncCallback2 ContinueAsync (appendSound resources)
  exolang' <- syncCallback2 ContinueAsync (exolang resources)
  o <- create
  setProp "clearResources" (jsval clearResources') o
  setProp "defaultResources" (jsval defaultResources') o
  setProp "reslist" (jsval reslist') o
  setProp "insertSound" (jsval insertSound') o
  setProp "deleteSound" (jsval deleteSound') o
  setProp "appendSound" (jsval appendSound') o
  setProp "exolang" (jsval exolang') o
  pure $ API $ jsval o

testExtension :: Resources -> Text -> IO ()
testExtension resources path = do
  api <- newAPI resources
  p <- extensionPromise path
  r <- await p
  case r of
    Left j -> throwIO (JSException j)
    Right j -> do
      let ext = pFromJSVal j
      putStrLn $ "loaded extension from " ++ unpack path
      _onLaunch ext api 
      pure ()

