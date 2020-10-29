module Estuary.Render.Tidal where

import Estuary.Render.R


parseTidalTextNotation :: Int -> TidalParser -> Text -> R ()
parseTidalTextNotation z p txt = do
  s <- get
  parseResult <- liftIO $ (return $! force (tidalParser p txt)) `catch` (return . Left . (show :: SomeException -> String))
  case parseResult of
    Right r -> do
      modify' $ \x -> x { paramPatterns = insert z r (paramPatterns x) }
      clearZoneError z
    Left e -> setZoneError z $ T.pack e


sequenceToControlPattern :: (Text,[Bool]) -> Tidal.ControlPattern
sequenceToControlPattern (sampleName,pat) = Tidal.s $ parseBP' $ intercalate " " $ fmap f pat
  where f False = "~"
        f True = T.unpack sampleName


renderControlPattern :: Int -> R ()
renderControlPattern z = do
  stgs <- askSettings
  when (webDirtOn stgs || superDirtOn stgs) $ do
    s <- get
    let controlPattern = IntMap.lookup z $ paramPatterns s -- :: Maybe ControlPattern
    case controlPattern of
      Just controlPattern' -> do
        let lt = renderStart s
        let rp = renderPeriod s
        let tempo' = tempo s
        newEvents <- liftIO $ (return $! force $ renderTidalPattern lt rp tempo' controlPattern')
          `catch` (\e -> putStrLn (show (e :: SomeException)) >> return [])
        pushTidalEvents newEvents
      Nothing -> return ()


renderTidalPattern :: UTCTime -> NominalDiffTime -> Tempo -> Tidal.ControlPattern -> [(UTCTime,Tidal.ControlMap)]
renderTidalPattern start range t p = events''
  where
    start' = (realToFrac $ diffUTCTime start (time t)) * freq t + count t -- start time in cycles since beginning of tempo
    end = realToFrac range * freq t + start' -- end time in cycles since beginning of tempo
    events = Tidal.queryArc p (Tidal.Arc (toRational start') (toRational end)) -- events with t in cycles
    events' = Prelude.filter Tidal.eventHasOnset events
    events'' = f <$> events'
    f e = (utcTime,Tidal.value e)
      where
        utcTime = addUTCTime (realToFrac ((fromRational w1 - count t)/freq t)) (time t)
        w1 = Tidal.start $ fromJust $ Tidal.whole e
