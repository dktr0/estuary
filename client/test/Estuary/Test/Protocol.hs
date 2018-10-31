module Estuary.Test.Protocol where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad

import Test.Hspec

import Text.JSON

import Estuary.Protocol.Foreign

import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse

import GHCJS.DOM.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Types
import GHCJS.Foreign.Callback

data MessageMatchStatus
  = Matches
  | AlmostMatches
  | DoesNotMatch

instance Monoid MessageMatchStatus where
  mempty = Matches
  Matches `mappend` _ = Matches
  _ `mappend` Matches = Matches
  AlmostMatches `mappend` _ = AlmostMatches
  _ `mappend` AlmostMatches = AlmostMatches
  DoesNotMatch `mappend` _ = DoesNotMatch

type MatchTest a = a -> MessageMatchStatus

emptyMatchTest :: MatchTest a
emptyMatchTest _ = DoesNotMatch

data MessageExpectation a r = MessageExpectation {
  messageTimeout :: Maybe Int,
  matchTest :: MatchTest a,
  supplement :: r
}

mergeMessageExpectations :: (s1 -> s2 -> s3) -> MessageExpectation a s1 -> MessageExpectation a s2 -> MessageExpectation a s3
mergeMessageExpectations merger (MessageExpectation time1 test1 s1) (MessageExpectation time2 test2 s2) =
  MessageExpectation {
    messageTimeout = case time2 of { Nothing -> time1; _ -> time2 },
    matchTest = \a -> (test1 a) `mappend` (test2 a),
    supplement = merger s1 s2
  }

instance Functor (MessageExpectation a) where
  fmap f expectation = expectation { supplement = f (supplement expectation) }

instance Applicative (MessageExpectation a) where
  pure x = emptyMessageExpectation { supplement = x }
  (<*>) = mergeMessageExpectations ($)

instance Monad (MessageExpectation a) where
  expectation >>= f = mergeMessageExpectations (const id) expectation (f (supplement expectation))

emptyMessageExpectation :: MessageExpectation a ()
emptyMessageExpectation = MessageExpectation {
  messageTimeout = Nothing,
  matchTest = emptyMatchTest,
  supplement = ()
}

withinMillis :: Int -> MessageExpectation a ()
withinMillis t = emptyMessageExpectation { messageTimeout = Just t }

toMatch :: MatchTest a -> MessageExpectation a ()
toMatch test = emptyMessageExpectation { matchTest = test }

expectRequest :: Chan (Maybe Request) -> MessageExpectation Request a -> IO ()
expectRequest reqChan e@(MessageExpectation Nothing _ _) = 
  expectRequest reqChan $ e { messageTimeout = Just 1000 }
expectRequest reqChan (MessageExpectation (Just ms) test _) = do
  almostReqsVar <- newMVar []

  result <- race (threadDelay $ ms * 1000) (checkRequest reqChan test almostReqsVar)
  case result of
    Right _ -> return ()
    Left _ -> do
      almostReqs <- readMVar almostReqsVar
      let desc = case almostReqs of
            [] -> "    No similar messages produced."
            almostMatches -> foldr (\l r -> l ++ "\n" ++ r) "" $ fmap (\r -> "    - " ++ encode r) almostMatches
      expectationFailure $ "Message was not produced within " ++ show ms ++ "ms." ++ desc

checkRequest :: Chan (Maybe Request) -> MatchTest Request -> MVar [Request] -> IO Request
checkRequest chan test almost = do
  mReq <- readChan chan
  case mReq of
    Nothing -> checkRequest chan test almost
    Just req -> case test req of
      Matches -> return req
      AlmostMatches -> do
        modifyMVar_ almost $ return . ((:) req)
        checkRequest chan test almost
      DoesNotMatch -> checkRequest chan test almost



attachMsgRecvProtocolInspector :: EstuaryProtocolObject -> (Maybe Response -> IO ()) -> IO ()
attachMsgRecvProtocolInspector (EstuaryProtocolObject protocol) inspect = do
  cb <- asyncCallback1 $ \nJsSerResp -> 
    inspect $ do -- in Maybe 
      jsSerResp <- mfilter isNull (pure nJsSerResp)
      serResp <- pFromJSVal jsSerResp
      case decode serResp of
        Ok resp -> return resp
        Error _ -> Nothing
  js_attachOnRecvMsg protocol cb
  releaseCallback cb

attachSendMsgProtocolInspector :: EstuaryProtocolObject -> (Maybe Request -> IO ()) -> IO ()
attachSendMsgProtocolInspector (EstuaryProtocolObject protocol) inspect = do
  cb <- asyncCallback1 $ \nJsSerReq -> 
    inspect $ do -- in Maybe 
      jsSerReq <- mfilter isNull (pure nJsSerReq)
      serResp <- pFromJSVal jsSerReq
      case decode serResp of
        Ok resp -> return resp
        Error _ -> Nothing
  js_attachOnSendMsg protocol cb
  releaseCallback cb

foreign import javascript safe
  "estuaryProtocolInspector.onRecvMsg($1, $2)"
  js_attachOnRecvMsg:: JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "estuaryProtocolInspector.onSendMsg($1, $2)"
  js_attachOnSendMsg:: JSVal -> Callback (JSVal -> IO ()) -> IO ()

attachProtocolInspectors :: EstuaryProtocolObject -> IO (Chan (Maybe Request), Chan (Maybe Response))
attachProtocolInspectors protocol = do
  reqChan <- newChan
  respChan <- newChan

  attachSendMsgProtocolInspector protocol $ writeChan reqChan
  attachMsgRecvProtocolInspector protocol $ writeChan respChan

  return (reqChan, respChan)