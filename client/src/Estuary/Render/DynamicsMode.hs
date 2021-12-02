{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Estuary.Render.DynamicsMode where

import Sound.MusicW
import GHCJS.DOM.Types (JSVal)
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Control.Monad
import Control.Concurrent.MVar

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
  microphoneInput :: Node,
  webDirtOutput :: Node,
  punctualInput :: Node,
  mainBusInput :: Node,
  mainBusDelay :: Node,
  compressorPreGain :: Node,
  mainBusCompressor :: Node,
  compressorPostGain :: Node,
  monitorInputGain :: Node,
  audioOutputs :: MVar Int
  }

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
  mainBusCompressor' <- createCompressor (-20) 3 4 0.050 0.100
  compressorPostGain' <- createGain 1.0
  monitorInputGain' <- createGain 0
  dest <- createDestination
  -- 2. establish most connections between nodes (from mainBusInput onwards, before that is handled by changePunctualAudioInputMode later)
  connectNodes mainBusInput' mainBusDelay'
  connectNodes mainBusDelay' compressorPreGain'
  connectNodes compressorPreGain' mainBusCompressor'
  connectNodes mainBusCompressor' compressorPostGain'
  connectNodes compressorPostGain' dest
  connectNodes microphoneInput' monitorInputGain'
  connectNodes monitorInputGain' compressorPreGain'
  -- 3. create a MainBus record to keep track of nodes
  audioOutputs' <- liftIO $ newMVar 2
  setChannelCount 2
  let mb = MainBus {
    microphoneInput = microphoneInput',
    webDirtOutput = webDirtOutput',
    punctualInput = punctualInput',
    mainBusInput = mainBusInput',
    mainBusDelay = mainBusDelay',
    compressorPreGain = compressorPreGain',
    mainBusCompressor = mainBusCompressor',
    compressorPostGain = compressorPostGain',
    monitorInputGain = monitorInputGain',
    audioOutputs = audioOutputs'
    }
  -- 4. apply parameters/connections for dynamics mode and Punctual audio input mode
  liftIO $ changeDynamicsMode mb DefaultDynamics
  liftIO $ changePunctualAudioInputMode mb MicToPunctual
  return mb


changeDynamicsMode :: MainBus -> DynamicsMode -> IO ()
changeDynamicsMode mb DefaultDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to default dynamics"
  setValue (compressorPreGain mb) Gain (dbamp (-10))
  let comp = mainBusCompressor mb
  setValue comp Threshold (-20)
  setValue comp Knee 3
  setValue comp CompressionRatio 2
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue (compressorPostGain mb) Gain 1.0
  return ()

changeDynamicsMode mb LoudDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to loud dynamics"
  setValue (compressorPreGain mb) Gain 1.0
  let comp = mainBusCompressor mb
  setValue comp Threshold (-10)
  setValue comp Knee 3
  setValue comp CompressionRatio 4
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue (compressorPostGain mb) Gain 1.0
  return ()

changeDynamicsMode mb WideDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to wide dynamics"
  setValue (compressorPreGain mb) Gain 1.0
  let comp = mainBusCompressor mb
  setValue comp Threshold 0.0
  setValue comp Knee 0.0
  setValue comp CompressionRatio 1.0
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue (compressorPostGain mb) Gain (dbamp (-20))
  return ()


changePunctualAudioInputMode :: MainBus -> PunctualAudioInputMode -> IO ()
changePunctualAudioInputMode mb MicToPunctual = liftAudioIO $ do
  liftIO $ do
    putStrLn "routing microphone to punctual input"
    disconnectAudioIO mb
  connectNodes (microphoneInput mb) (punctualInput mb)
  connectNodes (webDirtOutput mb) (mainBusInput mb)

changePunctualAudioInputMode mb WebDirtToPunctualMonitor = liftAudioIO $ do
  liftIO $ do
    putStrLn "routing WebDirt output to punctual input and to monitor output"
    disconnectAudioIO mb
  connectNodes (webDirtOutput mb) (punctualInput mb)
  connectNodes (webDirtOutput mb) (mainBusInput mb)

changePunctualAudioInputMode mb WebDirtToPunctualNoMonitor = liftAudioIO $ do
  liftIO $ do
    putStrLn "routing WebDirt output to punctual input only (no monitoring)"
    disconnectAudioIO mb
  connectNodes (webDirtOutput mb) (punctualInput mb)

disconnectAudioIO :: MainBus -> IO ()
disconnectAudioIO mb = liftAudioIO $ do
  disconnectAll (microphoneInput mb)
  connectNodes (microphoneInput mb) (monitorInputGain mb)
  disconnectAll (webDirtOutput mb)


changeDestination :: MainBus -> (forall m. AudioIO m => m Node) -> IO Node
changeDestination mb destCreator = liftAudioIO $ do
  newDest <- destCreator
  liftIO $ disconnectAll (compressorPostGain mb) -- *** TO FIX/INVESTIGATE: will this break analysis system?
  connectNodes (compressorPostGain mb) newDest
  return newDest


changeDelay :: MainBus -> AudioTime -> IO ()
changeDelay mb newDelayTime
  | newDelayTime > maxDelayTime = putStrLn "delay time greater than maxDelayTime"
  | otherwise = liftAudioIO $ do
    setValue (mainBusDelay mb) DelayTime newDelayTime
    return ()


changeMonitorInput :: MainBus -> Maybe Double -> IO ()
changeMonitorInput mb Nothing = do
  putStrLn "input monitoring off"
  void $ liftAudioIO $ setValue (monitorInputGain mb) Gain 0
changeMonitorInput mb (Just x) = do
  putStrLn $ "changing gain on input monitoring to " ++ show x ++ " dB"
  void $ liftAudioIO $ setValue (monitorInputGain mb) Gain (dbamp x)


setAudioOutputs :: MainBus -> Int -> IO ()
setAudioOutputs mb n = do
  m <- liftAudioIO $ maxChannelCount
  let n' = if n < 2 then 2 else n
  let n'' = if n' > m then m else n'
  liftAudioIO $ setChannelCount n''
  takeMVar (audioOutputs mb)
  putMVar (audioOutputs mb) n''
