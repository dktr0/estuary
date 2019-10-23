{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Estuary.Render.DynamicsMode where

import Sound.MusicW
import GHCJS.DOM.Types (JSVal)
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

initializeMainBus :: IO (Node,Node,Node,Node,Node,Node,JSVal)
initializeMainBus = liftAudioIO $ do
    acDestination <- createDestination
    ((v,w,x,y,z,a),s) <- playSynthNow acDestination $ do
      v <- audioIn
      w <- delay 5.0 v
      x <- gain (dbamp (-10)) w
      y <- compressor (-20) 3 4 0.050 0.100 x -- args are: threshold knee ratio attack release input
      z <- gain 1.0 y
      a <- analyser 512 0.5 z
      audioOut z
      return (v,w,x,y,z,a)
    v' <- nodeRefToNode v s
    w' <- nodeRefToNode w s
    setValue w' DelayTime 0.0
    x' <- nodeRefToNode x s
    y' <- nodeRefToNode y s
    z' <- nodeRefToNode z s
    a' <- nodeRefToNode a s
    aArray <- liftIO $ arrayForAnalysis a'
    return (v',w',x',y',z',a',aArray)


changeDynamicsMode :: (Node,Node,Node,Node,Node,Node,JSVal) -> DynamicsMode -> IO ()
changeDynamicsMode (input,del,preGain,comp,postGain,_,_) DefaultDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to default dynamics"
  setValue preGain Gain (dbamp (-10))
  setValue comp Threshold (-20)
  setValue comp Knee 3
  setValue comp CompressionRatio 2
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue postGain Gain 1.0
  return ()

changeDynamicsMode (input,del,preGain,comp,postGain,_,_) LoudDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to loud dynamics"
  setValue preGain Gain 1.0
  setValue comp Threshold (-10)
  setValue comp Knee 3
  setValue comp CompressionRatio 4
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue postGain Gain 1.0
  return ()

changeDynamicsMode (input,del,preGain,comp,postGain,_,_) WideDynamics = liftAudioIO $ do
  liftIO $ putStrLn "changing to wide dynamics"
  setValue preGain Gain 1.0
  setValue comp Threshold 0.0
  setValue comp Knee 0.0
  setValue comp CompressionRatio 1.0
  setValue comp Attack 0.050
  setValue comp Release 0.100
  setValue postGain Gain (dbamp (-20))
  return ()

changeDestination :: (Node, Node, Node, Node, Node, Node, JSVal) -> (forall m. AudioIO m => m Node) -> IO Node
changeDestination (_, _, _, _, postGain,_,_) destCreator = liftAudioIO $ do
  newDest <- destCreator
  liftIO $ disconnectAll postGain -- *** TO FIX/INVESTIGATE: will this break analysis system?
  connectNodes postGain newDest
  return newDest

changeDelay :: (Node,Node,Node,Node,Node,Node,JSVal) -> AudioTime -> IO ()
changeDelay (input,del,preGain,comp,postGain,_,_) newDelayTime = liftAudioIO $ do
  setValue del DelayTime newDelayTime
  return ()

foreign import javascript unsafe
  "new Uint8Array($1.frequencyBinCount)"
  arrayForAnalysis :: Node -> IO JSVal

foreign import javascript unsafe
  "$1.getByteFrequencyData($2);"
  getByteFrequencyData :: Node -> JSVal -> IO ()

foreign import javascript unsafe
  "var acc=0; for(var x=0;x<4;x++) { acc=acc+$1[x] }; acc=acc/(4*256); $r = acc"
  getLo :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=4;x<40;x++) { acc=acc+$1[x] }; acc=acc/(36*256); $r = acc"
  getMid :: JSVal -> IO Double

foreign import javascript unsafe
  "var acc=0; for(var x=40;x<256;x++) { acc=acc+$1[x] }; acc=acc/(216*256); $r = acc"
  getHi :: JSVal -> IO Double
