module Estuary.Render.Punctual where

import Estuary.Render.R

parsePunctual :: Int -> Text -> UTCTime -> R ()
parsePunctual z txt eTime = do
  s <- get
  let evalTime = utcTimeToAudioSeconds (wakeTimeSystem s, wakeTimeAudio s) eTime -- :: AudioTime/Double
  parseResult <- liftIO $ try $ return $! Punctual.parse evalTime txt
  case parseResult of
    Right (Right punctualProgram) -> do
      punctualProgramChanged z punctualProgram
      clearZoneError z
    Right (Left parseErr) -> setZoneError z $ T.pack $ show parseErr
    Left exception -> setZoneError z $ T.pack $ show (exception :: SomeException))

punctualProgramChanged :: Int -> Punctual.Program -> R ()
punctualProgramChanged z p = do
  s <- get
  re <- ask
  -- A. update PunctualW (audio state) in response to new, syntactically correct program
  let (mainBusIn,_,_,_,_) = mainBus re
  ac <- liftAudioIO $ audioContext
  t <- liftAudioIO $ audioTime
  let prevPunctualW = findWithDefault (Punctual.emptyPunctualW ac mainBusIn 2) z (punctuals s)
  let tempo' = tempo s
  let beat0 = utcTimeToAudioSeconds (wakeTimeSystem s, wakeTimeAudio s) $ origin tempo'
  let cps' = freq tempo'
  newPunctualW <- liftIO $ do
    runAudioContextIO ac $ Punctual.updatePunctualW prevPunctualW (beat0,realToFrac cps') p
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return prevPunctualW)
  modify' $ \x -> x { punctuals = insert z newPunctualW (punctuals s)}
  -- B. update Punctual WebGL state in response to new, syntactically correct program
  let pWebGL = punctualWebGL s
  newWebGL <- liftIO $
    Punctual.evaluatePunctualWebGL (glContext s) (beat0,realToFrac cps') z p pWebGL
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return pWebGL)
  modify' $ \x -> x { punctualWebGL = newWebGL }

updatePunctualResolutionAndBrightness :: R ()
updatePunctualResolutionAndBrightness = do
  stgs <- askSettings
  s <- get
  newWebGL <- liftIO (do
    x <- Punctual.setResolution (glContext s) (resolution stgs) (punctualWebGL s)
    Punctual.setBrightness (brightness stgs) x
    )
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }

animatePunctual :: UTCTime -> Int -> R ()
animatePunctual tNow z = do
  s <- get
  let tNow' = utcTimeToAudioSeconds (wakeTimeSystem s,wakeTimeAudio s) tNow
  newWebGL <- liftIO $
    Punctual.drawPunctualWebGL (glContext s) tNow' z (punctualWebGL s)
    `catch` (\e -> putStrLn (show (e :: SomeException)) >> return (punctualWebGL s))
  modify' $ \x -> x { punctualWebGL = newWebGL }
