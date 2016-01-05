module Main where

import Reflex
import Reflex.Dom
import Sound.Tidal.Context as Tidal
import Sound.Tidal.Utils (fst',snd',thd')
import Data.Map
import Sound.OSC.Type

import Data.JSString (JSString)
import qualified Data.JSString as JSS
import JavaScript.Web.CloseEvent
import JavaScript.Web.MessageEvent
import qualified JavaScript.Web.WebSocket as WS

showEventArc :: Parseable a => Tidal.Event a -> String
showEventArc x = (show (fst' x)) ++ " " ++ (show (snd' x))

extractSample :: Tidal.Event OscMap -> String
extractSample x = f mm
 where f (Just (Just y)) = ascii_to_string $ d_ascii_string y
       mm = Data.Map.lookup (S "sound" Nothing) (thd' x)

test = Prelude.map extractSample $ arc (sound (p "bd cp")) (0,1)

onClose :: Maybe (CloseEvent -> IO ())
onClose = Just (\_ -> putStrLn "connection closed")

onMessage :: Maybe (MessageEvent -> IO ())
onMessage = Just (f . getData)
 where f (StringData js) = putStrLn "a string"
       f (BlobData js) = putStrLn "a blob"
       f (ArrayBufferData js) = putStrLn "an array buffer"

-- request = WebSocketRequest { url=JSS.pack "ws:://127.0.0.1:8005",protocols=[],onClose=onClose,onMessage=onMessage}

main = do
  putStrLn "making main widget..."
  mainWidget $ el "div" $ text (intercalate "," test)

--  putStrLn "connecting to WebSocket..."
--  socket <- WS.connect request
