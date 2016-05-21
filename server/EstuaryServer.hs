module Main where

import Control.Concurrent (MVar, newMVar)
import Control.Monad (forever)
import qualified Sound.Tidal.Context as Tidal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Language.Haskell.Interpreter as Hint
import Data.List

type Client = WS.Connection

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

main = estuaryServer 9162

estuaryServer port = do
  putStrLn "Tidal websocket server for Estuary"
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ app state

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  Main.interact conn state

-- hintParamPattern  :: (MonadIO m, Control.Monad.Catch.MonadMask m) => String -> m (Either InterpreterError Tidal.ParamPattern)
hintParamPattern x = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]
  Hint.interpret x (Hint.as::Tidal.ParamPattern)

patternOrSilence :: Either Hint.InterpreterError Tidal.ParamPattern -> IO Tidal.ParamPattern
patternOrSilence (Left err) = do
  putStrLn (show err)
  return $ Tidal.sound (Tidal.p "")
patternOrSilence (Right patt) = do
  putStrLn (show patt)
  return patt

interact :: WS.Connection -> MVar ServerState -> IO ()
interact conn state = do
  (cps,getNow) <- Tidal.bpsUtils
  (ds, _) <- Tidal.superDirtSetters getNow
  forever $ do
    msg <- WS.receiveData conn
    let msg' = T.unpack msg
    let cmd = splitCmd msg'
    act ds cmd
  where act _  Nothing = return ()
        act ds (Just ("/eval", code)) = do putStrLn code
                                           x <- hintParamPattern code
                                           y <- patternOrSilence x
                                           ds $ y
        act _ (Just (cmd, _)) = do putStrLn ("Unknown cmd: " ++ cmd)
                                   return ()

splitCmd :: String -> Maybe (String, String)
splitCmd s = do n <- elemIndex ' ' s
                return $ (take n s, drop (n+1) s)
