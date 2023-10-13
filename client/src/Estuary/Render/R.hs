{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.R where

import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception (evaluate,catch,SomeException,try)
import Control.Concurrent.MVar
import Data.IntMap.Strict as IntMap
import Data.Map.Strict as Map
import Data.IORef
import Sound.MusicW
import TextShow
import GHCJS.Types (JSVal)

import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState

import Estuary.Render.MainBus
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Render.RenderOp
import Estuary.Types.Tempo
import Estuary.Resources
import Estuary.Types.ResourceOp
import Estuary.Client.Settings as Settings
import Estuary.Types.Language
import Estuary.Render.WebSerial as WebSerial
import Estuary.Render.WebDirt as WebDirt
import Estuary.Types.RenderInfo
import Estuary.Render.RenderEnvironment

putRenderOps :: MonadIO m => RenderEnvironment -> [RenderOp] -> m ()
putRenderOps re x = liftIO $ do
  ops <- takeMVar $ renderOps re
  putMVar (renderOps re) $ ops ++ x

takeRenderOps :: R [RenderOp]
takeRenderOps = do
  mv <- asks renderOps
  ops <- liftIO $ takeMVar mv
  liftIO $ putMVar mv []
  -- when (length ops > 0) $ liftIO $ putStrLn $ show (length ops) ++ " ops"
  return ops


setCC :: Int -> Double -> RenderEnvironment -> IO ()
setCC n v rEnv = modifyIORef' (ccMap rEnv) $ Map.insert (showt n) v

getCC :: Int -> RenderEnvironment -> IO (Maybe Double)
getCC n irc = do
  m <- readIORef $ ccMap irc
  return $ Map.lookup (showt n) m

updateTidalValueMap :: R ()
updateTidalValueMap = do
  rEnv <- ask
  m <- liftIO $ readIORef $ ccMap rEnv
  modify' $ \x -> x { valueMap = fmap Tidal.toValue $ Map.mapKeys T.unpack m}


type R = ReaderT RenderEnvironment (StateT RenderState IO)

runR :: R a -> RenderEnvironment -> RenderState -> IO RenderState
runR r rEnv rState = do
  let r' = runReaderT r rEnv -- :: (StateT RenderState IO) a
  (execStateT r' rState)
  `catch` (\e -> putStrLn "runRenderer" >> putStrLn (show (e :: SomeException)) >> return rState)

pushNoteEvents :: [NoteEvent] -> R ()
pushNoteEvents xs = modify' $ \x -> x { noteEvents = noteEvents x ++ xs }

{- already obsolete?
pushTidalEvents :: [(UTCTime,Tidal.ValueMap)] -> R ()
pushTidalEvents = pushNoteEvents . fmap tidalEventToNoteEvent
-}

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

clearBaseNotation :: Int -> R ()
clearBaseNotation z = modify' $ \x -> x { baseNotations = IntMap.delete z $ baseNotations x}

setBaseDefinition :: Int -> Definition -> R ()
setBaseDefinition z n = modify' $ \x -> x { baseDefinitions = IntMap.insert z n $ baseDefinitions x}

clearBaseDefinition :: Int -> R ()
clearBaseDefinition z = modify' $ \x -> x { baseDefinitions = IntMap.delete z $ baseDefinitions x}

setEvaluationTime :: Int -> UTCTime -> R ()
setEvaluationTime z n = modify' $ \x -> x { evaluationTimes = IntMap.insert z n $ evaluationTimes x}

clearEvaluationTime :: Int -> R ()
clearEvaluationTime z = modify' $ \x -> x { evaluationTimes = IntMap.delete z $ evaluationTimes x}

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


updateWebDirtVoices :: R ()
updateWebDirtVoices = do
  wd <- asks webDirt
  n <- liftIO $ WebDirt.voices wd
  modify' $ \s -> s { info = (info s) { webDirtVoices = n } }
