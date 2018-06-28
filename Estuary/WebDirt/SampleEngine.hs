module Estuary.WebDirt.SampleEngine where

import Reflex
import Reflex.Dom
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Control.Monad.IO.Class (liftIO)
import Control.Exception

import Estuary.Types.Context

class SampleEngine e where
  getClockDiff :: e -> IO Double -- diff btw clock used to play sample events and POSIX
  playSample :: e -> (Double,Tidal.ParamMap) -> IO ()
  getPeakLevels :: e -> IO [Double]
  getRmsLevels :: e -> IO [Double]

-- | Given a time range, calculate the events that occur in that time range
renderPattern :: (UTCTime, NominalDiffTime) -> ContextChange
renderPattern (start,range) c = c { sounds = (sounds c) ++ events' }
  where
    t = tempo c
    start' = (realToFrac $ diffUTCTime start (Tidal.at t)) / Tidal.cps t + Tidal.beat t -- start time in cycles since beginning of tempo
    end = realToFrac range / Tidal.cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.seqToRelOnsetDeltas (toRational start',toRational end) (pattern c) -- events, with times expressed as fractions of the range s to e
    events' = Prelude.map (\(o,_,m) -> (addUTCTime (realToFrac o*range) start,m)) events

-- | IO action to send all events from a given context to a SampleEngine
-- (will use getClockDiff to adjust the frame of reference as necessary)
sendSoundsIO :: SampleEngine e => e -> Context -> IO ()
sendSoundsIO e c = do
  clockDiff <- getClockDiff e
  let latency = Tidal.clockLatency $ tempo c
  let sounds' = fmap (\(x,y) -> (realToFrac (utcTimeToPOSIXSeconds x) - clockDiff + latency,y)) $ sounds c
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))

sendSounds :: (MonadWidget t m, SampleEngine e)
  => e -> Event t Context -> m ()
sendSounds e c = performEvent_ $ fmap (liftIO . sendSoundsIO e) c

flushSounds :: (MonadWidget t m)
  => Event t a -> m (Event t ContextChange)
flushSounds x = return $ fmap (const $ \y -> y { sounds = [] } ) x

renderSendFlush :: (MonadWidget t m, SampleEngine e, SampleEngine e2)
  => UTCTime -> e -> e2 -> Dynamic t Context -> m (Event t ContextChange)
renderSendFlush now wd sd c = do
  tick <- tickLossy (0.125::NominalDiffTime) now
  let r = fmap (renderPattern . (\x -> (x,0.125::NominalDiffTime)) . _tickInfo_lastUTC) tick
  wdIsOn <- mapDyn webDirtOn c
  sdIsOn <- mapDyn superDirtOn c
  sendSounds wd (gate (current wdIsOn) $ tagDyn c tick)
  sendSounds sd (gate (current sdIsOn) $ tagDyn c tick)
  f <- flushSounds tick
  return $ mergeWith (.) [r,f]

