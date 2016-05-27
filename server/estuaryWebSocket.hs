module Main where

import Control.Concurrent (MVar, newMVar)
import Control.Monad (forever)
import Control.Exception (try)
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Tidal.Stream as Tidal
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

type TidalState = (Double -> IO (),[Tidal.ParamPattern -> IO()])

main = do
  putStrLn "Tidal websocket server for Estuary, listening on port 9162"
  WS.runServer "0.0.0.0" 9162 $ (\pending -> do
    conn <- WS.acceptRequest pending
    putStrLn "received new connection"
    WS.forkPingThread conn 30
    (cps,getNow) <- Tidal.bpsUtils
    dss <- mapM (\_ -> Tidal.dirtStream) [0..8]
    loop (cps,dss) conn
    )

loop :: TidalState -> WS.Connection -> IO ()
loop s@(cps,dss) conn = do
  m <- try (WS.receiveData conn)
  case m of
    Right x -> do
      processResult s (decode (T.unpack x))
      loop s conn
    Left WS.ConnectionClosed -> close s "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close s "by request from peer"
    Left (WS.ParseException e) -> close s ("parse exception: " ++ e)

close :: TidalState -> String -> IO ()
close (cps,dss) msg = do
  putStrLn ("connection closed: " ++ msg)
  hush dss

processResult :: TidalState -> Result Request -> IO ()
processResult _ (Error e) = putStrLn ("Error: " ++ e)
processResult state (Ok request) = do
  putStrLn (show request)
  respond state request

respond :: TidalState -> Request -> IO ()
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

hintParamPattern  :: String -> IO (Either InterpreterError Tidal.ParamPattern)
hintParamPattern x = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]
  Hint.interpret x (Hint.as::Tidal.ParamPattern)

hush = mapM_ ($ Tidal.silence)
