module Main where

import Reflex
import Reflex.Dom
import Sound.Tidal.Context as Tidal
import Sound.Tidal.Utils (fst',snd',thd')
{-import Estuary.Tidal.Util-}
import Data.Map
import Sound.OSC.Type
import Safe (readMay)

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

takeArc :: String -> [Tidal.Event OscMap]
takeArc x = arc (sound (p x)) (0, 1)

showSoundPattern :: String -> String
showSoundPattern x = (intercalate "," (Prelude.map extractSample $ takeArc x) )

onClose :: Maybe (CloseEvent -> IO ())
onClose = Just (\_ -> putStrLn "connection closed")

onMessage :: Maybe (MessageEvent -> IO ())
onMessage = Just (f . getData)
 where f (StringData js) = putStrLn "a string"
       f (BlobData js) = putStrLn "a blob"
       f (ArrayBufferData js) = putStrLn "an array buffer"

openSocket = do
  let request = WebSocketRequest {
    url=JSS.pack "ws:://127.0.0.1:8005",
    protocols=[],
    onClose=onClose,
    onMessage=onMessage
  }
  socket <- WS.connect request

makeMainWidget = mainWidget $ el "div" $ do
  input <- textInput def
  result <- forDyn (_textInput_value input) showSoundPattern
  dynText result

main = do
  putStrLn "making main widget..."
  makeMainWidget
  putStrLn "opening socket..."
  openSocket
  putStrLn "finished!"

patternInput :: MonadWidget t m => m (Dynamic t (Maybe String))
patternInput = do
  n <- textInput $ def & textInputConfig_initialValue .~ "bd,bp"
  mapDyn readMay $ _textInput_value n
