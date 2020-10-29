module Estuary.Render.TimeNot where

import Estuary.Render.R

parseTimeNot :: Int -> Text -> UTCTime -> R ()
parseTimeNot z txt eTime = do
  let parseResult = TimeNot.runCanonParser $ T.unpack txt
  case parseResult of
    Right p -> do
      modify' $ \x -> x { timeNots = insert z p (timeNots x) }
      clearZoneError z
    Left e -> setZoneError z (T.pack $ show e)

renderTimeNot :: Int -> R ()
renderTimeNot z = do
  s <- get
  let p = IntMap.lookup z $ timeNots s
  case p of
    (Just p') -> do
      let theTempo = tempo s
      let eTime = IntMap.findWithDefault (wakeTimeSystem s) z $ evaluationTimes s
      let wStart = renderStart s
      let wEnd = renderEnd s
      let oTime = firstCycleStartAfter theTempo eTime
      pushNoteEvents $ fmap TimeNot.mapForEstuary $ TimeNot.render oTime p' wStart wEnd
    Nothing -> return ()
