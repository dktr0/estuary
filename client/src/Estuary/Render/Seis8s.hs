module Estuary.Render.Seis8s where

import qualified Sound.Seis8s.Parser as Seis8s

import Estuary.Render.R

parseSeis8s :: Int -> Text -> UTCTime -> R ()
parseSeis8s z txt eTime = do
  let parseResult = Seis8s.parseLang $ T.unpack txt
  case parseResult of
    Right p -> do
      modify' $ \x -> x { seis8ses = insert z p (seis8ses x) }
      clearZoneError z
    Left e -> setZoneError z (T.pack $ show e)

renderSeis8s :: Int -> R ()
renderSeis8s z = do
  s <- get
  let p = IntMap.lookup z $ seis8ses s
  case p of
    Just p' -> pushNoteEvents $ Seis8s.render p' (tempo s) (renderStart s) (renderEnd s)
    Nothing -> return ()
