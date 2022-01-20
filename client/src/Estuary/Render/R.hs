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

import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.TextNotation
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Render.DynamicsMode
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Resources
import Estuary.Types.ResourceOp
import Estuary.Types.UriOptions as Uri


data RenderEnvironment = RenderEnvironment {
  mainBus :: MainBus,
  webDirt :: WebDirt,
  superDirt :: SuperDirt,
  resources :: Resources,
  ccMap :: IORef (Map.Map Text Double),
  animationOn :: IORef Bool
  }

initialRenderEnvironment :: Uri.UriOptions -> IO RenderEnvironment
initialRenderEnvironment uriOptions = do
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
  animationOn' <- newIORef $ Uri.canvas uriOptions
  putStrLn "finished initialRenderEnvironment"
  return $ RenderEnvironment {
    mainBus = mb,
    webDirt = wd,
    superDirt = sd,
    resources = resources',
    ccMap = ccMap',
    animationOn = animationOn'
  }


-- type CCMap = MVar (Map Text Double)

-- TODO: will rework as operation in R monad
setCC :: Int -> Double -> RenderEnvironment -> IO ()
setCC n v rEnv = modifyIORef' (ccMap rEnv) $ Map.insert (showt n) v

-- TODO: will rework as operation in R monad
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
