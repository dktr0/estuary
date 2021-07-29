{-# LANGUAGE OverloadedStrings, RecursiveDo, DeriveGeneric, DeriveAnyClass #-}

module Estuary.Widgets.Reference (Reference(..),navigation) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Control.Monad
import GHC.Generics
import GHCJS.Marshal
import Data.Text (Text)
import qualified Data.Map.Strict as Map

import Estuary.Widgets.Reflex
import Estuary.Help.LanguageHelp
import Estuary.Help.MiniTidal
import Estuary.Help.PunctualAudio
import Estuary.Help.TerminalViewCommands
import Estuary.Help.Hydra
import Estuary.Help.CineCer0.CineCer0
import Estuary.Help.CineCer0.CineCer0Reference
import Estuary.Types.Request
import Estuary.Widgets.Editor
import Estuary.Widgets.Router
import qualified Estuary.Types.Term as Term
import Estuary.Types.TranslatableText
import Estuary.Types.Language
import Estuary.Types.Definition


data Reference =
  MainList |
  MiniTidal |
  Punctual |
  Hydra |
  CineCer0 |
  CineCer0Reference |
  TerminalViewCommands
  deriving (Generic, FromJSVal, ToJSVal)

navigation :: MonadWidget t m => Editor t m (Event t Reference)
navigation = do
  x <- router' MainList never $ referenceWidget
  let a = switchDyn x
  return a

referenceWidget :: MonadWidget t m => Reference -> Editor t m (Event t Reference)

referenceWidget MainList = do
  divClass "reference-title" $ text "estuary"
  divClass "reference-description" $ dynText =<< (translatableText (Map.fromList [
                (English,"Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser."),
                (Español,"Estuary es una plataforma para la colaboración y el aprendizaje a través de la codificación en vivo. Le permite crear sonido, música y elementos visuales en un navegador web.")]))

  el "h3" $ dynText =<< (translatableText (Map.fromList [(English, "Languages"), (Español, "Lenguajes")]))
  navEv1 <- el "ul" $ do
    miniTidal <- el "li" $ goTo MiniTidal $ english "MiniTidal"
    punctual <- el "li" $ goTo Punctual $ english "Punctual"
    hydra <- el "li" $ goTo Hydra $ english "Hydra"
    cineCero <- el "li" $ goTo CineCer0 $ english "CineCer0"
    return $ leftmost [miniTidal, punctual, hydra, cineCero]
  el "h3" $ dynText =<< (translatableText (Map.fromList [(English, "View System"), (Español, "Sistema de Vistas")]))
  navEv2 <- el "ul" $ do
    terminalViewCommands <- el "li" $ goTo TerminalViewCommands (Map.fromList [(English, "Terminal View Commands"), (Español, "Comandos de la Terminal para Cambiar las Vistas")])
    return $ leftmost [terminalViewCommands]
  let navEv = leftmost [navEv1, navEv2]
  return navEv

referenceWidget MiniTidal = do
  navEv <- goTo MainList  $ (Map.fromList [(English, "Home"), (Español, "Inicio")])
  el "h3" $ text "MiniTidal"
  miniTidalHelpFile
  return navEv

referenceWidget Punctual = do
  navEv <- goTo MainList $ (Map.fromList [(English, "Home"), (Español, "Inicio")])
  el "h3" $ text "Punctual"
  punctualAudioHelpFile
  return navEv

referenceWidget Hydra = do
  navEv <- goTo MainList $ (Map.fromList [(English, "Home"), (Español, "Inicio")])
  el "h3" $ text "Hydra"
  hydraHelp
  return navEv

referenceWidget CineCer0 = do
  navEv <- el "div" $ do
    home <- goTo MainList $ (Map.fromList [(English, "Home"), (Español, "Inicio")])
    cinecer0Reference <- goTo CineCer0Reference $ (Map.fromList [(English,  "CineCer0 Reference"), (Español, "Referencia de CineCer0")])
    return $ leftmost [home, cinecer0Reference]
  el "h3" $ text "CineCer0"
  cineCer0Help
  return navEv

referenceWidget CineCer0Reference = do
  navEv <- el "div" $ do
    home <- goTo MainList $ (Map.fromList [(English, "Home"), (Español, "Inicio")])
    cinecer0 <- goTo CineCer0 $ english "CineCer0"
    return $ leftmost [home, cinecer0]
  el "h3" $ dynText =<< (translatableText (Map.fromList [(English,  "CineCer0 Reference"), (Español, "Referencia de CineCer0")]))
  cineCer0Reference
  return navEv

referenceWidget TerminalViewCommands = do
  navEv <- goTo MainList $ (Map.fromList [(English, "Home"), (Español, "Inicio")])
  el "h3" $ dynText =<< (translatableText (Map.fromList [(English,  "Terminal View Commands"), (Español, "Comandos de la terminal para cambia las vistas")]))
  terminalViewCommandsHelpFile
  return navEv

goTo :: MonadWidget t m => Reference -> TranslatableText -> Editor t m (Event t Reference)
goTo targetPage title = do
  liftM (targetPage <$) $ dynButtonWithChild "reference-link" $ dynText =<< (translatableText title)
