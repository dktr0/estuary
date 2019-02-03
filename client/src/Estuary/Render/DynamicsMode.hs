module Estuary.Render.DynamicsMode where 

import Sound.MusicW
import Control.Monad.IO.Class (liftIO)

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

initializeMainBus :: IO (Node,Node,Node,Node)
initializeMainBus = liftAudioIO $ do
    acDestination <- createDestination
    ((w,x,y,z),s) <- playSynthNow acDestination $ do
      w <- audioIn
      x <- gain (dbamp (-10)) w
      y <- compressor (-20) 3 4 0.050 0.100 x -- args are: threshold knee ratio attack release input
      z <- gain 1.0 y
      audioOut z
      return (w,x,y,z)
    w' <- nodeRefToNode w s
    x' <- nodeRefToNode x s
    y' <- nodeRefToNode y s
    z' <- nodeRefToNode z s
    return (w',x',y',z')

changeDynamicsMode :: (Node,Node,Node,Node) -> DynamicsMode -> IO ()
changeDynamicsMode (input,preGain,comp,postGain) DefaultDynamics = liftAudioIO $ do  
  liftIO $ putStrLn "changing to default dynamics"
  setValue preGain Gain (dbamp (-10))
  setValue comp Threshold (-20)
  setValue comp Knee 3
  setValue comp CompressionRatio 2
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue postGain Gain 1.0
  return ()
 
changeDynamicsMode (input,preGain,comp,postGain) LoudDynamics = liftAudioIO $ do  
  liftIO $ putStrLn "changing to loud dynamics"
  setValue preGain Gain 1.0
  setValue comp Threshold (-10)
  setValue comp Knee 3
  setValue comp CompressionRatio 4
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue postGain Gain 1.0
  return ()

changeDynamicsMode (input,preGain,comp,postGain) WideDynamics = liftAudioIO $ do  
  liftIO $ putStrLn "changing to wide dynamics"
  setValue preGain Gain 1.0
  setValue comp Threshold 0.0
  setValue comp Knee 0.0
  setValue comp CompressionRatio 1.0
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue postGain Gain (dbamp (-20))
  return ()
