module Main where

import Control.Concurrent (MVar, newMVar)
import Control.Monad (forever)
import qualified Sound.Tidal.Context as Tidal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = WS.Connection

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

main = do
  putStrLn "simple Tidal websocket server"
  (cps,getNow) <- Tidal.bpsUtils
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9161 $ app state

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  Main.interact conn state

interact :: WS.Connection -> MVar ServerState -> IO ()
interact conn state = do
  ds <- Tidal.dirtStream
  forever $ do
    msg <- WS.receiveData conn
    T.putStrLn msg
    ds $ Tidal.sound (Tidal.p (T.unpack msg))
