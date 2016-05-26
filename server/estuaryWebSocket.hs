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
import Data.Ratio
import Text.JSON

import Request
import WebDirt

type Client = WS.Connection

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

main = do
  putStrLn "Tidal websocket server for Estuary"
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9162 $ app state

app :: MVar ServerState -> WS.ServerApp
app state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  Main.interact conn state

hintParamPattern  :: String -> IO (Either InterpreterError Tidal.ParamPattern)
hintParamPattern x = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]
  Hint.interpret x (Hint.as::Tidal.ParamPattern)

interact :: WS.Connection -> MVar ServerState -> IO ()
interact conn state = do
  (cps,getNow) <- Tidal.bpsUtils
  dss <- mapM (\_ -> Tidal.dirtStream) [0..8]
  --  (d1, _) <- Tidal.superDirtSetters getNow
  forever $ do
    msg <- WS.receiveData conn
    case decode (T.unpack msg) of (Error e) -> putStrLn ("Error: " ++ e)
                                  (Ok request) -> do
                                    putStrLn (show request)
                                    respond (cps,dss) request


respond :: (Double -> IO (),[Tidal.ParamPattern -> IO()]) -> Request -> IO ()
respond _ (Info i) = return ()
respond (_,dss) Hush = mapM_ ($ Tidal.silence) dss
respond (cps,_) (Cps x) = cps x
respond (_,dss) (Pattern n p) = do
  x <- hintParamPattern p
  case x of (Left error) -> putStrLn "Error interpreting pattern"
            (Right paramPattern) -> dss!!(n-1) $ paramPattern
respond _ (Render patt cps cycles) = do
  x <- hintParamPattern patt
  case x of (Left error) -> putStrLn "Error interpreting pattern"
            (Right paramPattern) -> putStrLn (encode (render paramPattern cps cycles))
