module Main where

import Control.Concurrent (MVar, newMVar)
import Control.Monad (forever)
import qualified Sound.Tidal.Context as Tidal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Language.Haskell.Interpreter as Hint

type Client = WS.Connection

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

main = estuaryServer 9162

estuaryServer port = do
  putStrLn "Tidal websocket server for Estuary"
  (cps,getNow) <- Tidal.bpsUtils
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ app state

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  Main.interact conn state

-- hintOscPattern  :: (MonadIO m, Control.Monad.Catch.MonadMask m) => String -> m (Either InterpreterError Tidal.OscPattern)
hintOscPattern x = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]
  Hint.interpret x (Hint.as::Tidal.OscPattern)

patternOrSilence :: Either Hint.InterpreterError Tidal.OscPattern -> IO Tidal.OscPattern
patternOrSilence (Left err) = do
  putStrLn (show err)
  return $ Tidal.sound (Tidal.p "")
patternOrSilence (Right patt) = do
  putStrLn (show patt)
  return patt

interact :: WS.Connection -> MVar ServerState -> IO ()
interact conn state = do
  ds <- Tidal.dirtStream
  forever $ do
    msg <- WS.receiveData conn
    let s = T.unpack msg
    putStrLn s
    x <- hintOscPattern s
    y <- patternOrSilence x
    ds $ y
