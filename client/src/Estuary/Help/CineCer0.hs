{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.CineCer0 where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor
import Data.Map.Strict

cineCer0Help :: MonadWidget t m => Editor t m ()
cineCer0Help = el "div" $ do
  aboutCineCer0
  examplesCineCer0
  funcionsCineCer0


aboutCineCer0 :: MonadWidget t m => Editor t m ()
aboutCineCer0 = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"CineCer0 (pronounced “sin–ay–ser-oh”) language targets a similar functionality to CineVivo (by Esteban Betancur). CineCer0 has a economical Haskell-like notation that enables the temporal and geometrical transformation of video and text elements."),
    (Español,"El lenguaje CineCer0 buscar una funcionalidad similar a CineVivo (por Esteban Betancur). CineCer0 usa una notación económica parecida a la de Haskell que habilita la transformación temporal y geométrica de elementos de video y texto.")
    ])


examplesCineCer0 :: MonadWidget t m => Editor t m ()
examplesCineCer0 = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Examples:"),
    (Español,"Ejemplos:")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "every 30 1 $ circleMask 0.5 $ setSize 0.8 $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text ""


funcionsCineCer0 :: MonadWidget t m => Editor t m ()
funcionsCineCer0 = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Functions"),
    (Español,"Funciones")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Videos are played by adings the URL inside quotation marks:"),
    (Español,"Para reproducir videos se debe añadir la URL dentro de comillas:")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "\"videos/hogweed.mov\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Transforming Image:"),
    (Español,"Transformato la Image:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Size: setWidth [w], setHeight [h], setSize [wh] (affects both width and height); where 1 = natural video/text size"),
    (Español,"_Tamaño: setWidth [w], setHeight [h], setSize [wh] (afectando el largo y el ancho); donde 1 = tamaño natural del video/texto")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setWidth 0.5 $ setHeight 0.8 $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setSize 0.5 $ \"videos/hogweed.mov\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Position: setPosX [x], setPosY [y], setCoord [x] [y]; it goes from (-1) = top/left to 1 = bottom/right, 0 = centre"),
    (Español,"_Posición: setPosX [x], setPosY [y], setCoord [x] [y]; va de (-1) = arriba/izquierda a 1 = abajo/derecha, 0 = centro")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setPosX 0.0 $ setPosY (-0.5) $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 (-0.5) $ \"videos/hogweed.mov\""
