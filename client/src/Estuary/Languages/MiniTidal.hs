module Estuary.Languages.MiniTidal (miniTidal,renderTidalPattern) where

import Data.Time
import Control.Monad.Except
import Data.IntMap as IntMap
import Data.Text as T
import Control.Exception
import Control.Monad.State.Strict
import Sound.MusicW.AudioContext
import Control.Monad.Reader

import Estuary.Render.R
import Estuary.Render.TextNotationRenderer
import Estuary.Types.RenderState
import Estuary.Types.RenderInfo
import Estuary.Types.Context
import Estuary.Types.TextNotation
import Estuary.Render.MainBus
import Estuary.Types.Tempo
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleC

miniTidal :: TextNotationRenderer
miniTidal = TextNotationRenderer {
  parseZone = _parseZone,
  clearZone' = clearParamPattern,
  preAnimationFrame = _preAnimationFrame,
  zoneAnimationFrame = _zoneAnimationFrame,
  postAnimationFrame = _postAnimationFrame
  }



renderControlPattern :: Context -> Int -> R ()
renderControlPattern c z = do
  wdOn <- webDirtOn
  sdOn <- superDirtOn
  when (wdOn || sdOn) $ do
    s <- get
    let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
    let vMap = valueMap s
    case controlPattern of
      Just controlPattern' -> do
        let lt = renderStart s
        let rp = renderPeriod s
        let tempo' = tempo $ ensemble $ ensembleC c
        newEvents <- liftIO $ (return $! force $ renderTidalPattern vMap lt rp tempo' controlPattern')
          `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
        pushTidalEvents newEvents
      Nothing -> return ()

renderTidalPattern :: Tidal.ValueMap -> UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ValueMap)]
renderTidalPattern vMap start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (time t)) * freq t + count t -- start time in cycles since beginning of tempo
    end = realToFrac range * freq t + start' -- end time in cycles since beginning of tempo
    a = Tidal.Arc (toRational start') (toRational end)
    events = Tidal.query p $ Tidal.State a vMap
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - count t)/freq t)) (time t)
        w1 = Tidal.start $ fromJust $ Tidal.whole e
