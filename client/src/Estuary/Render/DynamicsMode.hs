{-# LANGUAGE ScopedTypeVariables, RankNTypes, JavaScriptFFI #-}
module Estuary.Render.DynamicsMode where

import Sound.MusicW
import GHCJS.DOM.Types (JSVal)
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Control.Monad
import Control.Concurrent.MVar
import Estuary.Render.WebDirt

import GHCJS.Types
import GHCJS.Marshal.Pure

data DynamicsMode =
  DefaultDynamics | -- Gentle compression, with pre-compression levels reduced a bit, should be close to SuperDirt dynamics
  LoudDynamics | -- More aggressive compression and a higher pre-compression level, a bit like classic Dirt
  WideDynamics -- No compression and a lower overall level (-20 dB), useful when routing to external dynamics management
  deriving (Eq,Ord)

dynamicsModes :: [DynamicsMode]
dynamicsModes = [DefaultDynamics,LoudDynamics,WideDynamics]

instance Show DynamicsMode where
  show DefaultDynamics = "Default"
  show LoudDynamics = "Loud"
  show WideDynamics = "Wide"

data PunctualAudioInputMode =
  MicToPunctual |
  WebDirtToPunctualMonitor |
  WebDirtToPunctualNoMonitor
  deriving (Eq,Ord)

punctualAudioInputModes :: [PunctualAudioInputMode]
punctualAudioInputModes = [MicToPunctual,WebDirtToPunctualMonitor,WebDirtToPunctualNoMonitor]

instance Show PunctualAudioInputMode where
  show MicToPunctual = "Microphone"
  show WebDirtToPunctualMonitor = "WebDirt to Punctual and output"
  show WebDirtToPunctualNoMonitor = "WebDirt to Punctual only"

maxDelayTime :: Double
maxDelayTime = 10

data MainBus = MainBus {
  microphoneInput :: MVar Node,
  webDirtOutput :: MVar Node,
  punctualInput :: MVar Node,
  mainBusInput :: MVar Node,
  mainBusDelay :: MVar Node,
  compressorPreGain :: MVar Node,
  compressorSplitter :: MVar Node,
  compressors :: MVar [Node],
  compressorMerger :: MVar Node,
  compressorPostGain :: MVar Node,
  monitorInputGain :: MVar Node,
  audioOutputs :: MVar Int,
  mainBusDynamicsMode :: MVar DynamicsMode
  }

getWebDirtOutput :: MainBus -> IO Node
getWebDirtOutput mb = readMVar $ webDirtOutput mb

getPunctualInput :: MainBus -> IO Node
getPunctualInput mb = readMVar $ punctualInput mb

getMainBusInput :: MainBus -> IO Node
getMainBusInput mb = readMVar $ mainBusInput mb

initializeMainBus :: IO MainBus
initializeMainBus = liftAudioIO $ do
  -- 1. create web audio nodes
  microphoneInput' <- createMicrophone
  webDirtOutput' <- createGain 1.0
  punctualInput' <- createGain 1.0
  mainBusInput' <- createGain 1.0
  mainBusDelay' <- createDelay maxDelayTime
  setValue mainBusDelay' DelayTime 0
  compressorPreGain' <- createGain 1.0
  (compressorSplitter',compressors',compressorMerger') <- createCompressorFan 2
  compressorPostGain' <- createGain 1.0
  monitorInputGain' <- createGain 0
  dest <- createDestination
  -- 2. establish most connections between nodes (from mainBusInput onwards, before that is handled by changePunctualAudioInputMode later)
  connectNodes mainBusInput' mainBusDelay'
  connectNodes mainBusDelay' compressorPreGain'
  connectNodes compressorPreGain' compressorSplitter'
  connectNodes compressorMerger' compressorPostGain'
  connectNodes compressorPostGain' dest
  connectNodes microphoneInput' monitorInputGain'
  connectNodes monitorInputGain' compressorPreGain'
  -- 3. create a MainBus record (full of MVars with nodes in them) to keep track of nodes
  microphoneInput'' <- liftIO $ newMVar microphoneInput'
  webDirtOutput'' <- liftIO $ newMVar webDirtOutput'
  punctualInput'' <- liftIO $ newMVar punctualInput'
  mainBusInput'' <- liftIO $ newMVar mainBusInput'
  mainBusDelay'' <- liftIO $ newMVar mainBusDelay'
  compressorPreGain'' <- liftIO $ newMVar compressorPreGain'
  compressorSplitter'' <- liftIO $ newMVar compressorSplitter'
  compressors'' <- liftIO $ newMVar compressors'
  compressorMerger'' <- liftIO $ newMVar compressorMerger'
  compressorPostGain'' <- liftIO $ newMVar compressorPostGain'
  monitorInputGain'' <- liftIO $ newMVar monitorInputGain'
  audioOutputs' <- liftIO $ newMVar 2
  dynamicsMode' <- liftIO $ newMVar DefaultDynamics
  setChannelCount 2
  let mb = MainBus {
    microphoneInput = microphoneInput'',
    webDirtOutput = webDirtOutput'',
    punctualInput = punctualInput'',
    mainBusInput = mainBusInput'',
    mainBusDelay = mainBusDelay'',
    compressorPreGain = compressorPreGain'',
    compressorSplitter = compressorSplitter'',
    compressors = compressors'',
    compressorMerger = compressorMerger'',
    compressorPostGain = compressorPostGain'',
    monitorInputGain = monitorInputGain'',
    audioOutputs = audioOutputs',
    mainBusDynamicsMode = dynamicsMode'
    }
  -- 4. apply parameters/connections for dynamics mode and Punctual audio input mode
  liftIO $ changeDynamicsMode mb DefaultDynamics
  liftIO $ changePunctualAudioInputMode mb MicToPunctual
  liftIO $ exposeMainBus mb
  return mb


-- exposes Estuary's standing audio nodes in correspondingly named global variables
exposeMainBus :: MainBus -> IO ()
exposeMainBus mb = do
  x1 <- readMVar $ webDirtOutput mb
  x2 <- readMVar $ mainBusInput mb
  x3 <- readMVar $ mainBusDelay mb
  x4 <- readMVar $ compressorPreGain mb
  x5 <- readMVar $ compressorPostGain mb
  _exposeMainBus x1 x2 x3 x4 x5

foreign import javascript unsafe
  "webDirtOutput = $1; mainBusInput = $2; mainBusDelay = $3; compressorPreGain = $4; compressorPostGain = $5;"
  _exposeMainBus :: Node -> Node -> Node -> Node -> Node -> IO ()


createCompressorFan :: AudioIO m => Int -> m (Node,[Node],Node)
createCompressorFan nChnls = do
  splitter <- createChannelSplitter nChnls
  merger <- createChannelMerger nChnls
  comps <- sequence $ (flip fmap) [0..(nChnls-1)] $ \n -> do
    c <- createCompressor (-20) 3 4 0.050 0.100
    connectNodes' splitter n c 0
    connectNodes' c 0 merger n
    return c
  return (splitter,comps,merger)

disconnectCompressorFan :: Node -> [Node] -> Node -> IO ()
disconnectCompressorFan splitter comps merger = mapM_ disconnectAll $ splitter : merger : comps

recreateCompressorFan :: MainBus -> Int -> IO ()
recreateCompressorFan mb nChnls = do
  -- disconnect previous compressor splitter, fan, and merger
  splitter <- readMVar $ compressorSplitter mb
  comps <- readMVar $ compressors mb
  merger <- readMVar $ compressorMerger mb
  disconnectCompressorFan splitter comps merger
  -- create new compressor splitter, fan, and merger for given number of channels
  (newSplitter,newComps,newMerger) <- liftAudioIO $ createCompressorFan nChnls
  swapMVar (compressorSplitter mb) newSplitter
  swapMVar (compressors mb) newComps
  swapMVar (compressorMerger mb) newMerger
  -- connect new splitter and merger to rest of output system
  compressorPreGain' <- readMVar $ compressorPreGain mb
  compressorPostGain' <- readMVar $ compressorPostGain mb
  connectNodes compressorPreGain' newSplitter
  connectNodes newMerger compressorPostGain'
  -- set dynamics mode to stored value
  dynMode <- readMVar (mainBusDynamicsMode mb)
  changeDynamicsMode mb dynMode

setValueCompressorFan :: AudioIO m => MVar [Node] -> Double -> Double -> Double -> Double -> Double -> m ()
setValueCompressorFan mv thr knee ratio att rel = do
  nodes <- liftIO $ readMVar mv
  forM_ nodes $ \n -> do
    setValue n Threshold thr
    setValue n Knee knee
    setValue n CompressionRatio ratio
    setValue n Attack att
    setValue n Release rel

changeDynamicsMode :: MainBus -> DynamicsMode -> IO ()
changeDynamicsMode mb DefaultDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to default dynamics"
  preGain <- liftIO $ readMVar $ compressorPreGain mb
  postGain <- liftIO $ readMVar $ compressorPostGain mb
  setValue preGain Gain (dbamp (-10))
  setValueCompressorFan (compressors mb) (-20) 3 2 0.050 0.1
  setValue postGain Gain 1.0
  liftIO $ swapMVar (mainBusDynamicsMode mb) DefaultDynamics
  return ()

changeDynamicsMode mb LoudDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to loud dynamics"
  preGain <- liftIO $ readMVar $ compressorPreGain mb
  postGain <- liftIO $ readMVar $ compressorPostGain mb
  setValue preGain Gain 1.0
  setValueCompressorFan (compressors mb) (-10) 3 4 0.050 0.1
  setValue postGain Gain 1.0
  liftIO $ swapMVar (mainBusDynamicsMode mb) LoudDynamics
  return ()

changeDynamicsMode mb WideDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to wide dynamics"
  preGain <- liftIO $ readMVar $ compressorPreGain mb
  postGain <- liftIO $ readMVar $ compressorPostGain mb
  setValue preGain Gain 1.0
  setValueCompressorFan (compressors mb) 0 0 1 0.050 0.1
  setValue postGain Gain (dbamp (-20))
  liftIO $ swapMVar (mainBusDynamicsMode mb) WideDynamics
  return ()


changePunctualAudioInputMode :: MainBus -> PunctualAudioInputMode -> IO ()
changePunctualAudioInputMode mb MicToPunctual = do
  putStrLn "routing microphone to punctual input"
  disconnectAudioIO mb
  connectNodesMVar (microphoneInput mb) (punctualInput mb)
  connectNodesMVar (webDirtOutput mb) (mainBusInput mb)

changePunctualAudioInputMode mb WebDirtToPunctualMonitor = do
  putStrLn "routing WebDirt output to punctual input and to monitor output"
  disconnectAudioIO mb
  connectNodesMVar (webDirtOutput mb) (punctualInput mb)
  connectNodesMVar (webDirtOutput mb) (mainBusInput mb)

changePunctualAudioInputMode mb WebDirtToPunctualNoMonitor = do
  putStrLn "routing WebDirt output to punctual input only (no monitoring)"
  disconnectAudioIO mb
  connectNodesMVar (webDirtOutput mb) (punctualInput mb)

connectNodesMVar :: MVar Node -> MVar Node -> IO ()
connectNodesMVar x y = do
  x' <- readMVar x
  y' <- readMVar y
  connectNodes x' y'

disconnectAudioIO :: MainBus -> IO ()
disconnectAudioIO mb = liftAudioIO $ do
  micInput <- liftIO $ readMVar $ microphoneInput mb
  inputMonitor <- liftIO $ readMVar $ monitorInputGain mb
  wdOutput <- liftIO $ readMVar $ webDirtOutput mb
  disconnectAll micInput
  connectNodes micInput inputMonitor
  disconnectAll wdOutput


changeDestination :: MainBus -> (forall m. AudioIO m => m Node) -> IO Node
changeDestination mb destCreator = liftAudioIO $ do
  newDest <- destCreator
  cpg <- liftIO $ readMVar $ compressorPostGain mb
  liftIO $ disconnectAll cpg -- *** TO FIX/INVESTIGATE: will this break analysis system?
  connectNodes cpg newDest
  return newDest -- *** also to fix/investigate: why is new destination not being stored anywhere?


changeDelay :: MainBus -> AudioTime -> IO ()
changeDelay mb newDelayTime
  | newDelayTime > maxDelayTime = putStrLn "delay time greater than maxDelayTime"
  | otherwise = liftAudioIO $ do
    node <- liftIO $ readMVar $ mainBusDelay mb
    setValue node DelayTime newDelayTime
    return ()


changeMonitorInput :: MainBus -> Maybe Double -> IO ()
changeMonitorInput mb Nothing = do
  putStrLn "input monitoring off"
  void $ liftAudioIO $ do
    node <- liftIO $ readMVar $ monitorInputGain mb
    setValue node Gain 0
changeMonitorInput mb (Just x) = do
  putStrLn $ "changing gain on input monitoring to " ++ show x ++ " dB"
  void $ liftAudioIO $ do
    node <- liftIO $ readMVar $ monitorInputGain mb
    setValue node Gain (dbamp x)


setAudioOutputs :: WebDirt -> MainBus -> Int -> IO ()
setAudioOutputs wd mb n = do
  m <- liftAudioIO $ maxChannelCount
  let n' = if n < 2 then 2 else n
  let n'' = if n' > m then m else n'
  liftAudioIO $ setChannelCount n''
  setWebDirtAudioOutputs wd n''
  setNodeChannelCount (webDirtOutput mb) n''
  setNodeChannelCount (mainBusInput mb) n''
  setNodeChannelCount (mainBusDelay mb) n''
  setNodeChannelCount (compressorPreGain mb) n''
  setNodeChannelCount (compressorPostGain mb) n''
  recreateCompressorFan mb n''
  takeMVar (audioOutputs mb)
  putMVar (audioOutputs mb) n''

foreign import javascript unsafe
  "$1.channelCount = $2;"
  _setNodeChannelCount :: Node -> Int -> IO ()

setNodeChannelCount :: MVar Node -> Int -> IO ()
setNodeChannelCount m x = do
  node <- readMVar m
  _setNodeChannelCount node x
