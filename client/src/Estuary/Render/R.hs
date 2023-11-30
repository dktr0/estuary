{-# LANGUAGE OverloadedStrings #-}

module Estuary.Render.R where

import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Exception (evaluate,catch,SomeException,try)
import Control.Concurrent.MVar
import Data.IntMap.Strict as IntMap
import Data.Map.Strict as Map
import Data.List (nub)
import Data.IORef
import Sound.MusicW
import TextShow
import GHCJS.Types (JSVal)
import Data.Witherable

import qualified Sound.Tidal.Context as Tidal
import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.Definition
import Estuary.Types.TextNotation
import Estuary.Types.NoteEvent
import Estuary.Types.RenderInfo
import Estuary.Types.RenderState
import Estuary.Languages.ExoLang
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
import Estuary.Render.Renderer

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

setBaseDefinition :: Int -> Definition -> R ()
setBaseDefinition z x = modify' $ \s -> s { baseDefinitions = IntMap.insert z x $ baseDefinitions s }

clearBaseDefinition :: Int -> R ()
clearBaseDefinition z = modify' $ \s -> s { baseDefinitions = IntMap.delete z $ baseDefinitions s }

getBaseDefinition :: Int -> R (Maybe Definition)
getBaseDefinition z = gets (IntMap.lookup z . baseDefinitions)

setBaseRenderer :: Int -> Text -> R ()
setBaseRenderer z name = modify' $ \s -> s { activeRenderersMap = IntMap.insert z name $ activeRenderersMap s }

clearBaseRenderer :: Int -> R ()
clearBaseRenderer z = modify' $ \s -> s { activeRenderersMap = IntMap.delete z $ activeRenderersMap s }

updateActiveRenderers :: R ()
updateActiveRenderers = do
  s <- get
  rEnv <- ask
  rs <- liftIO $ readIORef (allRenderers rEnv)
  let names = nub $ IntMap.elems $ activeRenderersMap s -- [Text]
  let rs' = catMaybes $ fmap (\n -> Map.lookup n rs) names
  modify' $ \s -> s { activeRenderers = rs'  }

getActiveRenderers :: R [Renderer]
getActiveRenderers = gets activeRenderers

getActiveRenderer :: Int -> R (Maybe Renderer)
getActiveRenderer z = do
  brs <- gets activeRenderersMap
  case IntMap.lookup z brs of
    Nothing -> pure Nothing
    Just name -> do
      rEnv <- ask
      allRs <- liftIO $ readIORef (allRenderers rEnv)
      case Map.lookup name allRs of
        Nothing -> pure Nothing
        Just r ->  pure (Just r)
  
getAllRendererNames :: R [TextNotation]
getAllRendererNames = do
  rEnv <- ask
  rs <- liftIO $ readIORef (allRenderers rEnv)
  pure $ Map.keys rs
  
rendererExists :: TextNotation -> R Bool
rendererExists x = do
  xs <- getAllRendererNames 
  pure $ elem x xs
  

clearParamPattern :: Int -> R ()
clearParamPattern z = modify' $ \s -> s { paramPatterns = IntMap.delete z (paramPatterns s) }


-- updating Settings (eg. in response to user action in widgets)

updateSettings :: RenderEnvironment -> Settings -> IO ()
updateSettings re = writeIORef (_settings re)


-- asking/reading settings from within the R monad
-- (note: not all Settings are given accessors here, just those that might
-- be useful during computations with Estuary's render threads)

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


{- insertExoLang :: Text -> Text -> R ()
insertExoLang name url = do
  c <- gets exoLangCanvas
  e <- liftIO $ exoLangRenderer name c url
  rEnv <- ask
  liftIO $ insertRenderer rEnv name e
  liftIO $ T.putStrLn $ "inserted exolang " <> name <> " from " <> url
-}
