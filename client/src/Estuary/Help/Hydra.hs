{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Hydra where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor
import Data.Map.Strict


hydraHelp :: MonadWidget t m => Editor t m ()
hydraHelp = el "div" $ do
  aboutHydra
  notWorkingFunctionsHydra
  examplesHydra


aboutHydra :: MonadWidget t m => Editor t m ()
aboutHydra = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Hydra is a live coding platform for visual sintesis based on video analog synthesizers. Hydra is an open-source project developed by Olivia Jack (https://ojack.github.io/)."),
    (Español,"Hydra es una plataforma de live coding para síntesis visual basado en visuales generados por sintetizadores análogos. Hydra es un proyecto open-source desarrollado por Olivia Jack (https://ojack.github.io/).")
    ])


notWorkingFunctionsHydra :: MonadWidget t m => Editor t m ()
notWorkingFunctionsHydra = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Estuary contains a mini version of Hydra which currently does not run the following list of functions. Some of them will be included in the future."),
    (Español,"Estuary contiene una versión reducida de Hydra, la cual actualmente no corre la siguiente lista de funciones. Algunas de ellas serán incluidas en el futuro.")
    ])
  el "ul" $ do
    el "li" $ dynText =<< (translatableText $ fromList [
      (English,"External source-input functions (video, image, screen, camera), i.e."),
      (Español,"Funciones para entradas de fuentes externas (video, imagen, captura de pantalla, cámara), ej.")
      ])
    el "div" $ el "code" $ text "s0.initScreen()"
    el "div" $ el "code" $ text "scr(s0)"
    el "li" $ dynText =<< (translatableText $ fromList [
      (English,"Anonymous functions as parameters, i.e."),
      (Español,"Funciones anónimas como parámetros, ej.")
      ])
    el "div" $ el "code" $ text "() => mouse.x"
    el "div" $ el "code" $ text "({time}) => time%360"
    el "li" $ dynText =<< (translatableText $ fromList [
      (English,"Audio functions, i.e."),
      (Español,"Funciones de audio, ej.")
      ])
    el "div" $ el "code" $ text "a.show()"
    el "div" $ el "code" $ text "()=>a.fft[1]"
    el "li" $ dynText =<< (translatableText $ fromList [
      (English,"Division of screen into 4 when rendering the four different outputs, i.e."),
      (Español,"División de la pantalla en 4 cuando se renderizan los cuatro diferentes salidas, ej.")
      ])


examplesHydra :: MonadWidget t m => Editor t m ()
examplesHydra = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Examples of working codes in Estuary:"),
    (Español,"Ejemplos de códigos válidos en Estuary:")
    ])
  el "div" $ el "code" $ text "osc().kaleid().out()"
