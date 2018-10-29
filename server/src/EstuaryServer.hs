{-# LANGUAGE OverloadedStrings #-}
module EstuaryServer where

import Data.Text (Text)
import Data.List ((\\))
import Data.Maybe (fromMaybe,isJust,fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception

import qualified Database.SQLite.Simple as SQLite
import Text.JSON
import qualified Network.WebSockets as WS
import qualified Network.Wai as WS
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings, ssIndices, ssMaxAge)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip
import WaiAppStatic.Types (unsafeToPiece,MaxAge(..))
import Data.Time
import Data.Time.Clock.POSIX
import Data.Map

import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.EditOrEval
import qualified Estuary.Types.Ensemble as E
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.View
import Estuary.Types.Client
import Estuary.Types.Server
import Estuary.Types.Database

runServerWithDatabase :: String -> Int -> SQLite.Connection -> IO ()
runServerWithDatabase pswd port db = do
  putStrLn $ "Estuary collaborative editing server, listening on port " ++ (show port)
  putStrLn $ "password: " ++ pswd
  es <- readEnsembles db
  postLog db $ (show (size es)) ++ " ensembles restored from database"
  s <- newMVar $ newServer { password = pswd, ensembles = es }
  let settings = (defaultWebAppSettings "Estuary.jsexe") {
    ssIndices = [unsafeToPiece "index.html"],
    ssMaxAge = MaxAgeSeconds 30 -- 30 seconds max cache time
    }
  run port $ gzipMiddleware $ WS.websocketsOr WS.defaultConnectionOptions (webSocketsApp db s) (staticApp settings)

gzipMiddleware :: WS.Middleware
gzipMiddleware = gzip $ def {
  gzipFiles = GzipPreCompressed GzipIgnore
}

postLog :: SQLite.Connection -> String -> IO ()
postLog db msg = do
  postLogToDatabase db msg
  putStrLn msg

webSocketsApp :: SQLite.Connection -> MVar Server -> WS.ServerApp -- = PendingConnection -> IO ()
webSocketsApp db s ws = do
  ws' <- try $ WS.acceptRequest ws
  case ws' of
    Right ws'' -> do
      ss <- takeMVar s
      let (h,ss') = addClient ss ws''
      let cc = connectionCount ss' + 1
      let ss'' = ss' { connectionCount=cc }
      putMVar s ss''
      postLog db $ "received new connection (" ++ (show cc) ++ " connections since launch)"
      (WS.forkPingThread ws'' 30) `catch`
        \(SomeException e) -> postLog db $ "exception in forking ping thread: " ++ (show e)
      (getServerClientCount s >>= respondAll s . ServerClientCount) `catch`
        \(SomeException e) -> postLog db $ "exception during transmission of serverClientCount: " ++ (show e)
      processLoop db ws'' s h
    Left (SomeException e) -> do
      postLog db $ "exception during WS.acceptRequest: " ++ (show e)

processLoop :: SQLite.Connection -> WS.Connection -> MVar Server -> ClientHandle -> IO ()
processLoop db ws s h = do
  m <- try $ WS.receiveData ws
  case m of
    Right m' -> do
      processMessage db ws s h m' `catch` processMessageExceptions db
      processLoop db ws s h
    Left WS.ConnectionClosed -> close db s h "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close db s h "connection closed by request from peer"
    Left (WS.ParseException e) -> do
      postLog db $ "parse exception: " ++ e
      processLoop db ws s h
    -- Left (WS.UnicodeException e) -> do
    --  postLog db $ "Unicode exception: " ++ e
    --  processLoop db ws s h

processMessage :: SQLite.Connection -> WS.Connection -> MVar Server -> ClientHandle -> Text -> IO ()
processMessage db ws s h m = do
  let m' = decode (T.unpack m) :: Result JSString
  case m' of
    Ok m'' -> processResult db s h $ decode (fromJSString m'')
    Error x'' -> postLog db $ "Error: " ++ x''

processMessageExceptions :: SQLite.Connection -> SomeException -> IO ()
processMessageExceptions db e = postLog db $ "Exception (processResult): " ++ (show e)

close :: SQLite.Connection -> MVar Server -> ClientHandle -> String -> IO ()
close db s h msg = do
  postLog db $ "closing connection: " ++ msg
  updateServer s $ deleteClient h
  return ()


onlyIfAuthenticated :: MVar Server -> ClientHandle -> IO () -> IO ()
onlyIfAuthenticated s h f = do
  s' <- readMVar s
  let c = clients s' Map.! h
  if (authenticated c) then f else putStrLn "ignoring request from non-authenticated client"

onlyIfAuthenticatedInEnsemble :: MVar Server -> ClientHandle -> IO () -> IO ()
onlyIfAuthenticatedInEnsemble s h f = do
  s' <- readMVar s
  let c = clients s' Map.! h
  if (authenticatedInEnsemble c) then f else putStrLn "ignoring request from client not authenticated in ensemble"

processResult :: SQLite.Connection -> MVar Server -> ClientHandle -> Result Request -> IO ()
processResult db _ c (Error x) = postLog db $ "Error (processResult): " ++ x
processResult db s c (Ok x) = processRequest db s c x

processRequest :: SQLite.Connection -> MVar Server -> ClientHandle -> Request -> IO ()

processRequest db s c (Authenticate x) = do
  pwd <- getPassword s
  if x == pwd
    then do
      postLog db $ "received authenticate with correct password"
      updateClient s c $ \x -> x { authenticated = True }
    else do
      postLog db $ "received authenticate with wrong password"
      updateClient s c $ \x -> x { authenticated = False }

processRequest db s c GetEnsembleList = do
  postLog db $ "GetEnsembleList"
  getEnsembleList s >>= respond s c

processRequest db s c (JoinEnsemble x) = do
  postLog db $ "joining ensemble " ++ x
  updateClientWithServer s c f
  s' <- takeMVar s
  let e = ensembles s' Map.! x -- *** this is unsafe and should be refactored, same problem below in f too ***
  let t = E.tempo e
  respond' s' c $ EnsembleResponse (Sited x (NewTempo t))
  let defs' = fmap (EnsembleResponse . Sited x . ZoneResponse) $ Map.mapWithKey Sited $ fmap Edit $ E.defs e
  mapM_ (respond' s' c) $ defs'
  respond' s' c $ EnsembleResponse (Sited x (DefaultView (E.defaultView e)))
  let views' = fmap (EnsembleResponse . Sited x . View) $ Map.mapWithKey Sited $ E.views e
  mapM_ (respond' s' c) $ views'
  putMVar s s'
  where
    f s' c' = c' { ensemble = Just x, authenticatedInEnsemble = E.password ((ensembles s') Map.! x) == "" }

processRequest db s c LeaveEnsemble = do
  postLog db $ "leaving ensemble"
  updateClient s c $ \c' -> c' { ensemble = Nothing, authenticatedInEnsemble = False }

processRequest db s c (CreateEnsemble name pwd) = onlyIfAuthenticated s c $ do
  postLog db $ "CreateEnsemble " ++ name
  t <- getCurrentTime
  updateServer s $ createEnsemble name pwd t
  getEnsembleList s >>= respondAll s
  saveNewEnsembleToDatabase s name db

processRequest db s c (EnsembleRequest x) = processInEnsemble db s c x

processRequest db s c GetServerClientCount = do
  postLog db "GetServerClientCount"
  getServerClientCount s >>= respond s c . ServerClientCount

processInEnsemble :: SQLite.Connection -> MVar Server -> ClientHandle -> Sited String EnsembleRequest -> IO ()
processInEnsemble db s c (Sited e x) = processEnsembleRequest db s c e x

processEnsembleRequest :: SQLite.Connection -> MVar Server -> ClientHandle -> String -> EnsembleRequest -> IO ()

processEnsembleRequest db s c e x@(AuthenticateInEnsemble p2) = do
  p1 <- getEnsemblePassword s e
  let p2' = if p1 == "" then "" else p2
  if p1 == p2'
    then do
      postLog db $ "successful AuthenticateInEnsemble in " ++ e
      updateClient s c $ setAuthenticatedInEnsemble True
    else do
      postLog db $ "failed AuthenticateInEnsemble in " ++ e
      updateClient s c $ setAuthenticatedInEnsemble False

processEnsembleRequest db s c e x@(SendChat name msg) = onlyIfAuthenticatedInEnsemble s c $ do
  postLog db $ "SendChat in " ++ e ++ " from " ++ name ++ ": " ++ msg
  respondEnsemble s e $ EnsembleResponse (Sited e (Chat name msg))

processEnsembleRequest db s c e x@(ZoneRequest (Sited zone (Edit value))) = onlyIfAuthenticatedInEnsemble s c $ do
  postLog db $ "Edit in (" ++ e ++ "," ++ (show zone) ++ "): " ++ (show value)
  updateServer s $ edit e zone value
  respondEnsembleNoOrigin s c e $ EnsembleResponse (Sited e (ZoneResponse (Sited zone (Edit value))))
  saveEnsembleToDatabase s e db

processEnsembleRequest db s c e x@(ZoneRequest (Sited zone (Evaluate value))) = onlyIfAuthenticatedInEnsemble s c $ do
  postLog db $ "Eval in (" ++ e ++ "," ++ (show zone) ++ "): " ++ (show value)
  respondEnsembleNoOrigin s c e $ EnsembleResponse (Sited e (ZoneResponse (Sited zone (Evaluate value))))

processEnsembleRequest db s c e ListViews = do
  postLog db $ "ListViews in " ++ e
  vs <- getViews s e -- IO [String]
  respond s c (EnsembleResponse (Sited e (ViewList vs)))

processEnsembleRequest db s c e (GetView v) = do
  postLog db $ "GetView " ++ v ++ " in ensemble " ++ e
  getView s e v >>= maybe (return ()) (\v' -> respond s c (EnsembleResponse (Sited e (View (Sited v v')))))

processEnsembleRequest db s c e (PublishView (Sited key value)) = onlyIfAuthenticatedInEnsemble s c $ do
  postLog db $ "PublishView in (" ++ e ++ "," ++ key ++ "): " ++ (show value)
  updateServer s $ setView e key value
  saveEnsembleToDatabase s e db

processEnsembleRequest db s c e (PublishDefaultView v) = onlyIfAuthenticatedInEnsemble s c $ do
  postLog db $ "PublishDefaultView in " ++ e
  updateServer s $ setDefaultView e v
  saveEnsembleToDatabase s e db

processEnsembleRequest db s c e (DeleteView x) = do
  postLog db $ "DeleteView " ++ x ++ " in ensemble " ++ e
  updateServer s $ deleteView e x
  saveEnsembleToDatabase s e db

processEnsembleRequest db s c e x@(SetTempo t) = onlyIfAuthenticatedInEnsemble s c $ do
  updateServer s $ tempoChangeInEnsemble e t
  newTempo <- getTempoInEnsemble s e -- *** this one too
  if isJust newTempo then do
    let newTempo' = fromJust newTempo
    respondAll s $ EnsembleResponse $ Sited e $ NewTempo newTempo'
    postLog db $ "TempoChange in " ++ e
    saveEnsembleToDatabase s e db
  else postLog db $ "attempt to TempoChange in non-existent ensemble " ++ e

processEnsembleRequest db _ _ _ _ = postLog db $ "warning: action failed pattern matching"


send :: Response -> [Client] -> IO ()
send x cs = forM_ cs $ \y -> do
  (WS.sendTextData (connection y) $ (T.pack . encodeStrict) x)
  `catch` \(SomeException e) -> putStrLn $ "send exception: " ++ (show e)

respond :: MVar Server -> ClientHandle -> Response -> IO ()
respond s c x = withMVar s $ (send x) . (:[]) . (Map.! c)  . clients

-- respond' is for use when one already has a lock on the server MVar'
respond' :: Server -> ClientHandle -> Response -> IO ()
respond' s c x = send x $ (:[]) $ (Map.! c) $ clients s

respondAll :: MVar Server -> Response -> IO ()
respondAll s x = withMVar s $ (send x) . Map.elems . clients

respondAllNoOrigin :: MVar Server -> ClientHandle -> Response -> IO ()
respondAllNoOrigin s c x = withMVar s $ (send x) . Map.elems . Map.delete c . clients

respondEnsemble :: MVar Server -> String -> Response -> IO ()
respondEnsemble s e x = withMVar s $ (send x) . Map.elems . ensembleFilter e . clients

respondEnsembleNoOrigin :: MVar Server -> ClientHandle -> String -> Response -> IO ()
respondEnsembleNoOrigin s c e x = withMVar s $ (send x) . Map.elems . Map.delete c . ensembleFilter e . clients

ensembleFilter :: String -> Map.Map ClientHandle Client -> Map.Map ClientHandle Client
ensembleFilter e = Map.filter $ (==(Just e)) . ensemble

saveNewEnsembleToDatabase :: MVar Server -> String -> SQLite.Connection -> IO ()
saveNewEnsembleToDatabase s name db = do
  s' <- readMVar s
  f $ Data.Map.lookup name (ensembles s')
  where
    f (Just e) = writeNewEnsemble db name e
    f Nothing = postLog db $ "saveNewEnsembleToDatabase lookup failure for ensemble " ++ name

saveEnsembleToDatabase :: MVar Server -> String -> SQLite.Connection -> IO ()
saveEnsembleToDatabase s name db = do
  s' <- readMVar s
  f $ Data.Map.lookup name (ensembles s')
  where
    f (Just e) = writeEnsemble db name e
    f Nothing = postLog db $ "saveEnsembleToDatabase lookup failure for ensemble " ++ name
