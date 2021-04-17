{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.CineCer0.CineCer0Reference where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor
import Data.Map.Strict

cineCer0Reference :: MonadWidget t m => Editor t m ()
cineCer0Reference = el "div" $ do
  funcionsCineCer0

funcionsCineCer0 :: MonadWidget t m => Editor t m ()
funcionsCineCer0 = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Functions"),
    (Español,"Funciones")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Videos are played by adding an URL inside quotation marks:"),
    (Español,"Para reproducir videos se debe añadir la URL dentro de comillas:")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "\"videos/hogweed.mov\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"In a more declarative style:"),
    (Español,"En un estilo más declarativo:")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "video \"videos/hogweed.mov\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Transforming image:"),
    (Español,"Transformar la imagen:")
    ])
  -- SIZE
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Size: setWidth [w], setHeight [h], setSize [wh] (affects both width and height); where 1 = natural video/text size"),
    (Español,"_Tamaño: setWidth [w], setHeight [h], setSize [wh] (afectando el largo y el ancho); donde 1 = tamaño natural del video/texto")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setWidth 0.5 $ setHeight 0.8 $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setSize 0.5 $ \"videos/hogweed.mov\""
  -- POSITION
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Position: setPosX [x], setPosY [y], setCoord [x] [y]; it goes from (-1) = top/left to 1 = bottom/right, 0 = centre. Negative numbers must be in parentesis."),
    (Español,"_Posición: setPosX [x], setPosY [y], setCoord [x] [y]; va de (-1) = arriba/izquierda a 1 = abajo/derecha, 0 = centro. Los números negativos deben de ir en paréntesis.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setPosX 0.0 $ setPosY (-0.5) $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 (-0.5) $ \"videos/hogweed.mov\""
  -- CONCATENAR
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Funtions can be concatenated by using \"$\" or \"()\". They stack to the left."),
    (Español,"Las funciones puedes ser concatenadas usando \"$\" o \"()\". Éstas se acumulan a la derecha.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 (-0.3) $ setSize 0.5 $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 0.3 (setSize 0.5 (\"videos/hogweed.mov\"))"
  -- FILTERS
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Filters: setOpacity [o] (from 0 =full to 1 =none); setBlur [bl] (0 =none, 1++ =more); setBrightness [br] (0-0.99 =less, 1++ =more); setContrast [c] (0-0.99 =less, 1++ =more); setGrayscale [g] (from 0 =none to 1 =full); setSaturate [s] (0-0.99 =less, 1++ =more)"),
    (Español,"_Filtros: setOpacity [o] (0 =opacidad completa a 1 =sin); setBlur [bl] (0 =sin blur, 1++ =más); setBrightness [br] (0-0.99 =menos brillo, 1++ =más); setContrast [c] (0-0.99 =menos contraste, 1++ =más); setGrayscale [g] (de 0 =menos 1 =escala de grises); setSaturate [s] (0-0.99 =menos saturación, 1++ =más)")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setBrightness 1.5 $ setGrayscale 0.5 $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setOpacity 0.5 $ setBlur 1.2 $ setSaturation 0.5 $ \"videos/hogweed.mov\""
  -- MASKING
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Masking: circleMask [m] (the mask goes from 0.99 =smallest, growing from the centre when decreasing the number, 0 =none); circleMask' [m] [x] [y] (works similarly but allows you to set the anchor in the x and y coordenates)."),
    (Español,"_Máscaras: circleMask [m] (la máscara va desde 0.99 =más chica, crece desde el centro cuando el número decrece, 0=sin); circleMask' [m] [x] [y] (funciona similármente pero permite mover en las coordenadas x y el punto de anclaje).")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "circleMask 0.8 $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "circleMask' 0.5 0.1 0.2 $ \"videos/hogweed.mov\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"sqrMask [m] (); rectMask [t] [r] [b] [l] ()"),
    (Español,"")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text " sqrMask 0.5 $ $ \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text " rectMask 0 0.5 0 0.5 $ \"videos/hogweed.mov\""
  -- Text
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Text fucntions:"),
    (Español,"Funciones de texto:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Text: font String, fontSize Float, colour String (accepts colour conventions like white, black, etc. and HEX), rgb Float Float Float (rgba with a 4th Float for alpha), hsv Float Float Float (hsva with a 4th Float for alpha, synonym: hsl), strike, bold and italic"),
    (Español,"Text: fontFamily String, fontSize Float, colour String (accepta nombres convencionales de colores en inglés como white, black, etc. y HEX), rgb Float Float Float (rgba añade un cuarto argumento para alpha), hsv Float Float Float (hsva añade un cuarto argumento para alpha, sinónimo: hsl), strike, bold and italic")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text " text \"volviéndolo a grabaren las manos epilépticas del mar\""
    el "li" $ elClass "div" "ieRef" $ text " size 0.25 $ colour \"aqua\" $ strike $ bold $ italic $ text \"Siempre como 1 de esas lunas imprevistas\""
    el "li" $ elClass "div" "ieRef" $ text " size 0.5 $ rgba 0.8 0.5 0 0.5 $ fontSize 400 $ text \"que provocaron el infartote los subways descuidados\""
    el "li" $ elClass "div" "ieRef" $ text " hsv 0.33 0.1 0.3 $ font \"Times New Romans\" $ size 0.25 $ text \"Ayer: hoy & mañana –de repente–\""
  -- Audio
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Audio: vol Float; sets volume where 1 is full volume for the video. Video's audio is raw and not processed by Estuary's engine (compression or routing will not work on it)"),
    (Español,"Audio: vol Float; control de volume donde 1 es volumen máximo. El audio del video es directo y noe stá siendo procesado por Estuary (compresión o ruteo no funcionan)")
    ])
  -- Time functions
  -- chronometric time
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Chronometric fucntions:"),
    (Español,"Funciones cronométricas:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"quant MultCycle Offset, ramp Dur P1 P2, sine Freq, range Min Max Signal"),
    (Español,"quant MultCycle Offset, ramp Dur P1 P2, sine Freq, range Min Max Signal")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English, "Functions that change the program once in time:"),
    (Español, "Funciones que modifican el programa una sola vez en el tiempo:")
    ])
  --quant
  el "ul" $ do 
    el "li" $ elClass "div" "ieRef" $ text " text \"volviéndolo a grabaren las manos epilépticas del mar\""

  --ramp

  --sine

  --range

  -- Duration
