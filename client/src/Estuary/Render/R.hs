{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.R where

import Data.Time
import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception (evaluate,catch,SomeException,try)
import Data.IntMap.Strict as IntMap
import Data.Map.Strict as Map
import Data.IORef
import Sound.MusicW
import TextShow
import GHCJS.Types (JSVal)


import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.TextNotation
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
-- import Estuary.Render.DynamicsMode
import Estuary.Render.MainBus
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Resources
import Estuary.Types.ResourceOp
import Estuary.Client.Settings as Settings
import Estuary.Types.Language


data RenderEnvironment = RenderEnvironment {
  mainBus :: MainBus,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  resources :: Resources,
  ccMap :: IORef (Map.Map Text Double),
  _settings :: IORef Settings
  }

initialRenderEnvironment :: Settings -> IO RenderEnvironment
initialRenderEnvironment s = do
  ac <- getGlobalAudioContextPlayback
  addWorklets ac
  mb <- initializeMainBus
  wdOutput <- getWebDirtOutput mb
  wd <- liftAudioIO $ newWebDirt wdOutput
  initializeWebAudio wd
  sd <- newSuperDirt
  resources' <- newResources
  addResourceOp resources' $ ResourceListURL "samples/resources.json"
  ccMap' <- newIORef Map.empty
  settings' <- newIORef s
  putStrLn "finished initialRenderEnvironment"
  return $ RenderEnvironment {
    mainBus = mb,
    webDirt = wd,
    superDirt = sd,
    resources = resources',
    ccMap = ccMap',
    _settings = settings'
  }

setCC :: Int -> Double -> RenderEnvironment -> IO ()
setCC n v rEnv = modifyIORef' (ccMap rEnv) $ Map.insert (showt n) v

getCC :: Int -> RenderEnvironment -> IO (Maybe Double)
getCC n irc = do
  m <- readIORef $ ccMap irc
  return $ Map.lookup (showt n) m


type R = ReaderT RenderEnvironment (StateT RenderState IO)

runR :: R a -> RenderEnvironment -> RenderState -> IO RenderState
runR r rEnv rState = do
  let r' = runReaderT r rEnv -- :: (StateT RenderState IO) a
  (execStateT r' rState)
  `catch` (\e -> putStrLn "runRenderer" >> putStrLn (show (e :: SomeException)) >> return rState)

pushNoteEvents :: [NoteEvent] -> R ()
pushNoteEvents xs = modify' $ \x -> x { noteEvents = noteEvents x ++ xs }

pushTidalEvents :: [(UTCTime,Tidal.ValueMap)] -> R ()
pushTidalEvents xs = modify' $ \x -> x { tidalEvents = tidalEvents x ++ xs }

-- deprecated/temporary
pushWebDirtEvents :: [JSVal] -> R ()
pushWebDirtEvents xs = modify' $ \x -> x { webDirtEvents = webDirtEvents x ++ xs }


setZoneError :: Int -> Text -> R ()
setZoneError z t = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = IntMap.insert z t oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

clearZoneError :: Int -> R ()
clearZoneError z = do
  s <- get
  let oldErrors = errors $ info s
  let newErrors = IntMap.delete z oldErrors
  modify' $ \x -> x { info = (info s) { errors = newErrors } }

setBaseNotation :: Int -> TextNotation -> R ()
setBaseNotation z n = modify' $ \x -> x { baseNotations = IntMap.insert z n $ baseNotations x}

setEvaluationTime :: Int -> UTCTime -> R ()
setEvaluationTime z n = modify' $ \x -> x { evaluationTimes = IntMap.insert z n $ evaluationTimes x}

clearParamPattern :: Int -> R ()
clearParamPattern z = modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }


-- updating Settings (eg. in response to user action in widgets)

updateSettings :: RenderEnvironment -> Settings -> IO ()
updateSettings re = writeIORef (_settings re)


-- asking/reading settings from within the R monad
-- (note: not all Settings are given accessors here, just those that might
-- be useful during computations with Estuary's two render threads.)

askSettings :: (Settings -> a) -> R a
askSettings f = asks _settings >>= liftIO . readIORef >>= return . f

language :: R Language
language = askSettings Settings.language

canvasOn :: R Bool
canvasOn = askSettings Settings.canvasOn

resolution :: R Punctual.Resolution
resolution = askSettings Settings.resolution

brightness :: R Double
brightness = askSettings Settings.brightness

fpsLimit :: R (Maybe NominalDiffTime)
fpsLimit = askSettings Settings.fpsLimit

cineCer0ZIndex :: R Int
cineCer0ZIndex = askSettings Settings.cineCer0ZIndex

punctualZIndex :: R Int
punctualZIndex = askSettings Settings.punctualZIndex

improvizZIndex :: R Int
improvizZIndex = askSettings Settings.improvizZIndex

hydraZIndex :: R Int
hydraZIndex = askSettings Settings.hydraZIndex

webDirtOn :: R Bool
webDirtOn = askSettings Settings.webDirtOn

unsafeModeOn :: R Bool
unsafeModeOn = askSettings Settings.unsafeModeOn

superDirtOn :: R Bool
superDirtOn = askSettings Settings.superDirtOn
