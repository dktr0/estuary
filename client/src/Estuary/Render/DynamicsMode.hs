module Estuary.Render.DynamicsMode where

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
