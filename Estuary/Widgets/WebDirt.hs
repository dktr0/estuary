module Estuary.Widgets.WebDirt where

import Reflex
import Reflex.Dom
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import GHCJS.Types
import Control.Monad
import Control.Monad.IO.Class

-- webDirtWidget :: MonadWidget t m => JSVal -> m (Dynamic t ())
-- webDirtWidget webDirt = el "div" $ do
--   el "div" $ do
--     text "Esp URL: "
--     t <- textInput def
--     let t' = _textInput_input t
--     performEvent_ $ fmap (liftIO . (syncWithEsp webDirt)) t'
--   return $ constDyn ()

webDirtWidget :: MonadWidget t m => JSVal -> m (Dynamic t ())
webDirtWidget webDirt = el "div" $ do
  el "div" $ do
    b <- button "Sync Toggle"
    syncOn <- toggle False b
    syncString <- forDyn syncOn (\x-> if x then "ws://intramuros.mcmaster.ca:8002" else "")
    performEvent_ $ fmap (liftIO . (syncWithEsp webDirt)) $ updated syncString
  return $ constDyn ()
