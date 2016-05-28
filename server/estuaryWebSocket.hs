module Main where

import Control.Exception (try)
import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Tidal.Stream as Tidal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.List
import Data.Ratio
import Text.JSON

import Request
import WebDirt
import TidalHint

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
      y <- processResult s (decode (T.unpack x))
      case y of Just z -> WS.sendTextData conn (T.pack (encodeStrict z))
                Nothing -> return ()
      loop s conn
    Left WS.ConnectionClosed -> close s "unexpected loss of connection"
    Left (WS.CloseRequest _ _) -> close s "by request from peer"
    Left (WS.ParseException e) -> close s ("parse exception: " ++ e)

close :: TidalState -> String -> IO ()
close (cps,dss) msg = do
  putStrLn ("connection closed: " ++ msg)
  hush dss

hush = mapM_ ($ Tidal.silence)

processResult :: TidalState -> Result Request -> IO (Maybe JSValue)
processResult _ (Error e) = do
  putStrLn ("Error: " ++ e)
  return Nothing
processResult state (Ok request) = do
  putStrLn (show request)
  processRequest state request

processRequest :: TidalState -> Request -> IO (Maybe JSValue)

processRequest _ (Info i) = return Nothing

processRequest (_,dss) Hush = do
  hush dss
  return Nothing

processRequest (cps,_) (Cps x) = do
  cps x
  return Nothing

processRequest (_,dss) (Pattern n p) = do
  x <- hintParamPattern p
  case x of (Left error) -> do
              putStrLn "Error interpreting pattern"
              return Nothing
            (Right paramPattern) -> do
              dss!!(n-1) $ paramPattern
              return Nothing

processRequest _ (Render patt cps cycles) = do
  x <- hintParamPattern patt
  case x of (Left error) -> do
              putStrLn "Error interpreting pattern"
              return Nothing
            (Right paramPattern) -> do
              let r = render paramPattern cps cycles
              putStrLn (encodeStrict r)
              return (Just (showJSON r))
