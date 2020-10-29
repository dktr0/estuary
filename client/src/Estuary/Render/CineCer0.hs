module Estuary.Render.CineCer0 where

import Estuary.Render.R


parseCineCer0 :: Int -> Text -> UTCTime -> R ()
parseCineCer0 z txt eTime = do
  let parseResult :: Either String CineCer0.Spec = CineCer0.cineCer0 eTime $ T.unpack txt -- Either String CineCer0Spec
  case parseResult of
    Right spec -> do
      modify' $ \x -> x { cineCer0Specs = insert z spec (cineCer0Specs x) }
      clearZoneError z
    Left err -> setZoneError z (T.pack err)


renderCineCer0 :: UTCTime -> Int -> R ()
renderCineCer0 tNow z = do
  s <- get
  case videoDivCache s of
    Nothing -> return ()
    Just theDiv -> do
      let spec = IntMap.findWithDefault (CineCer0.emptySpec $ renderStart s) z (cineCer0Specs s)
      let prevState = IntMap.findWithDefault (CineCer0.emptyCineCer0State theDiv) z $ cineCer0States s
      newState <- liftIO $ CineCer0.updateCineCer0State (tempoCache s) tNow spec prevState
      modify' $ \x -> x { cineCer0States = insert z newState (cineCer0States s) }
