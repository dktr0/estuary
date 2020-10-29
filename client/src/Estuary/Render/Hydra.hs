module Estuary.Render.Hydra where

import Data.Time

import Estuary.Render.RenderState
import Estuary.Render.R

parseHydra :: Int -> Text -> R ()
parseHydra z txt = do
  s <- get
  parseResult <- liftIO $ try $ return $! Hydra.parseHydra txt
  case parseResult of
    Right (Right stmts) -> do
      clearZoneError z
      let x = IntMap.lookup z $ hydras s
      hydra <- case x of
        Just h -> return h
        Nothing -> do
          h <- liftIO $ Hydra.newHydra $ hydraCanvas s
          modify' $ \x -> x { hydras = IntMap.insert z h (hydras x)}
          return h
      -- liftIO $ Hydra.setResolution hydra 1280 720
      liftIO $ Hydra.evaluate hydra stmts
    Right (Left parseErr) -> setZoneError z (T.pack $ show parseErr)
    Left exception -> setZoneError z (T.pack $ show (exception :: SomeException))

animateHydra :: UTCTime -> Int -> R ()
animateHydra tNow z = do
  s <- get
  let x = IntMap.lookup z $ hydras s
  case x of
    Just hydra -> liftIO $ Hydra.tick hydra 16.6667
    Nothing -> return ()
