module Estuary.WebDirt.SampleEngine where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)

import Estuary.Types.Context

class SampleEngine e where
  getClockDiff :: e -> IO Double -- diff btw clock used to play sample events and POSIX
  playSample :: e -> (Double,ParamMap) -> IO ()
  peakLevels :: e -> IO [Double]
  rmsLevels :: e -> IO [Double]

-- | Given a time range, calculate the events that occur in that time range
renderPattern :: (UTCTime, NominalDiffTime) -> ContextChange
renderPattern (t,i) c = **** RESUME WORKING HERE (adapting from soon-to-be-deprecated Stream.hs)

-- | IO action to send all events from a given context to a SampleEngine
-- (will use getClockDiff to adjust the frame of reference as necessary)
sendSoundsIO :: SampleEngine e => e -> Context -> IO ()
sendSoundsIO e c = do
  clockDiff <- getClockDiff e
  let latency = clockLatency $ tempo c
  let sounds' = fmap (\(x,y) -> (x+clockDiff+latency,y)) $ sounds c
  E.catch (mapM_ (SampleEngine.playSample e) $ sounds c)
    (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))

sendSounds :: (MonadWidget t m, SampleEngine e)
  => e -> Event t Context -> IO ()
sendSounds e c = performEvent_ $ fmap (liftIO . sendSoundsIO e) c

flushSounds :: (MonadWidget t m, SampleEngine.SampleEngine e)
  => Event t a -> m (Event t ContextChange)
flushSounds x = return $ fmap (const $ \y -> y { events = [] } ) x

renderSendFlush :: (MonadWidget t m, SampleEngine.SampleEngine e)
  => UTCTime -> e -> e -> Dynamic t Context -> m (Event t ContextChange)
renderSendFlush startTime wd sd c = mdo
  tick <- tickLossy (0.125::NominalDiffTime) startTime
  r <- fmap ****???**** tick -- m (Event t (UTCTime, NominalDiffTime))
  r' <- fmap renderPattern rTimes -- m (Event t ContextChange)
  sendSounds wd (gate wdIsOn $ tagDyn c tick)
  sendSounds sd (gate sdIsOn $ tagDyn c tick)
  f <- flushSounds tick
  return $ mergeWith (.) [r',f]
