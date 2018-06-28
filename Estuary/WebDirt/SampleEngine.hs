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

renderTidalPattern :: UTCTime -> NominalDiffTime -> Tidal.Tempo -> Tidal.ParamPattern -> [(UTCTime,Tidal.ParamMap)]
renderTidalPattern start range t p = Prelude.map (\(o,_,m) -> (addUTCTime (realToFrac o*range) start,m)) events
  where
    start' = (realToFrac $ diffUTCTime start (Tidal.at t)) / Tidal.cps t + Tidal.beat t -- start time in cycles since beginning of tempo
    end = realToFrac range / Tidal.cps t + start' -- end time in cycles since beginning of tempo
    events = Tidal.seqToRelOnsetDeltas (toRational start',toRational end) (pattern c) -- times expressed as fractions of start->range

sendSounds :: SampleEngine e => e -> Tidal.Tempo -> [(UTCTime,Tidal.ParamMap)] -> IO ()
sendSounds e t sounds = do
  clockDiff <- getClockDiff e
  let latency = Tidal.clockLatency t
  let sounds' = fmap (\(x,y) -> (realToFrac (utcTimeToPOSIXSeconds x) - clockDiff + latency,y)) sounds
  catch (mapM_ (playSample e) sounds')
    (\msg -> putStrLn $ "exception: " ++ show (msg :: SomeException))

render :: (SampleEngine wd, SampleEngine sd) => wd -> sd -> Context -> UTCTime -> IO ()
render wd sd c logicalTime = do
  let sounds = renderTidalPattern logicalTime (0.1::NominalDiffTime) (tempo c) (pattern c)
  if webDirtOn c then sendSounds wd (tempo c) sounds else return ()
  if superDirtOn c then sendSounds sd (tempo c) sounds else return ()

regenerateRenderer :: (SampleEngine wd, SampleEngine sd) => MVar (UTCTime -> IO ()) -> wd -> sd -> Context -> IO ()
regenerateRenderer r wd sd c = putMVar r $ render wd sd c  

--  performEvent_ $ fmap (liftIO . regenerateRenderer r wd sd) $ updated context

forkRenderThread :: MVar (UTCTime -> IO ()) -> IO ()
forkRenderThread r = do
  renderStart <- getUTCTime 
  forkIO $ flip iterateM_ $ renderStart $ \t ->
    r' <- readMVar r
    r' t
    threadDelay 100000 -- 0.1 seconds in microseconds
    return $ addUTCTime (0.1::NominalDiffTime) t

