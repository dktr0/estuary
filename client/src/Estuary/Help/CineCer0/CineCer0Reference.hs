{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.CineCer0.CineCer0Reference where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Data.Map.Strict

cineCer0Reference :: MonadWidget t m => W t m ()
cineCer0Reference = el "div" $ do
  funcionsCineCer0

funcionsCineCer0 :: MonadWidget t m => W t m ()
funcionsCineCer0 = el "div" $ do
  dynText =<< (translatableText $ fromList [
    (English,"Functions"),
    (Español,"Funciones")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Videos are played by adding an URL inside quotation marks, plus the word \"video\" before:"),
    (Español,"Para reproducir videos se debe añadir la URL dentro de comillas, más la palabra \"video\" antes:")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "video \"videos/hogweed.mov\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Same with images but with the word \"image\":"),
    (Español,"Lo mismo para las imágenes pero con la palabra \"image\":")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "image \"images/cootes/chamomile.jpg\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Transforming videos/images:"),
    (Español,"Transformar videos/images:")
    ])
  -- SIZE
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Size: setWidth [w], setHeight [h], setSize [wh] (affects both width and height); where 1 = natural video/text size"),
    (Español,"_Tamaño: setWidth [w], setHeight [h], setSize [wh] (afectando el largo y el ancho); donde 1 = tamaño natural del video/texto")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setWidth 0.5 $ setHeight 0.8 $ video \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setSize 0.5 $ image \"images/cootes/chamomile.jpg\""
  -- POSITION
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Position: setPosX [x], setPosY [y], setCoord [x] [y]; it goes from (-1) = top/left to 1 = bottom/right, 0 = centre. Negative numbers must be in parentesis."),
    (Español,"_Posición: setPosX [x], setPosY [y], setCoord [x] [y]; va de (-1) = arriba/izquierda a 1 = abajo/derecha, 0 = centro. Los números negativos deben de ir en paréntesis.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setPosX 0.0 $ setPosY (-0.5) $ video \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 (-0.5) $ image \"images/cootes/chamomile.jpg\""
    -- ROTATION
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"_Rotation: setRotate [d]; parameter in degrees"),
      (Español,"_Rotación: setRotate [d]; parametro en grados")
      ])
    el "ul" $ do
      el "li" $ elClass "div" "ieRef" $ text "setRotate 50 $ image \"images/cootes/chamomile.jpg\""
  -- CONCATENAR
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Funtions can be concatenated by using \"$\" or \"()\". They stack to the left."),
    (Español,"Las funciones puedes ser concatenadas usando \"$\" o \"()\". Éstas se acumulan a la derecha.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 (-0.3) $ setSize 0.5 $ video \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setCoord 0.0 0.3 $ setSize 0.5 $ image \"images/cootes/chamomile.jpg\""
  -- FILTERS
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Filters: setOpacity [o] (from 0 =full to 1 =none); setBlur [bl] (0 =none, 1++ =more); setBrightness [br] (0-0.99 =less, 1++ =more); setContrast [c] (0-0.99 =less, 1++ =more); setGrayscale [g] (from 0 =none to 1 =full); setSaturate [s] (0-0.99 =less, 1++ =more)"),
    (Español,"_Filtros: setOpacity [o] (0 =opacidad completa a 1 =sin); setBlur [bl] (0 =sin blur, 1++ =más); setBrightness [br] (0-0.99 =menos brillo, 1++ =más); setContrast [c] (0-0.99 =menos contraste, 1++ =más); setGrayscale [g] (de 0 =menos 1 =escala de grises); setSaturate [s] (0-0.99 =menos saturación, 1++ =más)")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setBrightness 1.5 $ setGrayscale 0.5 $ video \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text "setOpacity 0.5 $ setBlur 1.2 $ setSaturation 0.5 $ image \"images/cootes/chamomile.jpg\""
  -- MASKING
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"_Masking: circleMask [m] (the mask goes from 0.99 =smallest, growing from the centre when decreasing the number, 0 =none); circleMask' [m] [x] [y] (works similarly but allows you to set the anchor in the x and y coordenates)."),
    (Español,"_Máscaras: circleMask [m] (la máscara va desde 0.99 =más chica, crece desde el centro cuando el número decrece, 0=sin); circleMask' [m] [x] [y] (funciona similármente pero permite mover en las coordenadas x y el punto de anclaje).")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "circleMask 0.8 $ video \"images/cootes/chamomile.jpg\""
    el "li" $ elClass "div" "ieRef" $ text "circleMask' 0.5 0.1 0.2 $ image \"images/cootes/chamomile.jpg\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"sqrMask [m] (); rectMask [t] [r] [b] [l] ()"),
    (Español,"")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text " sqrMask 0.5 $ $ video \"videos/hogweed.mov\""
    el "li" $ elClass "div" "ieRef" $ text " rectMask 0 0.5 0 0.5 $ image \"images/cootes/chamomile.jpg\""
    -- Audio
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Audio: vol Val; sets volume where 1 is full volume for the video. Video's audio is raw and not processed by Estuary's engine (compression or routing will not work on it)"),
    (Español,"Audio: vol Val; control de volume donde 1 es volumen máximo. El audio del video es directo y noe stá siendo procesado por Estuary (compresión o ruteo no funcionan)")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "vol 0.3 $ \"videos/lamplight.mp4\""
  -- Text
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"\n Text functions:"),
    (Español,"\n Funciones de texto:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Text: font \"FontName\", colour \"colourName\" (accepts colour conventions like white, black, etc. and HEX), rgb Red Green Blue (rgba with a 4th value for alpha), hsv Hue Saturation Value (hsva with a 4th value for alpha, synonym: hsl), strike, bold and italic"),
    (Español,"Text: font \"NombreFont\", colour \"NombreColor\" (accepta nombres convencionales de colores en inglés como white, black, etc. y HEX), rgb Rojo Verde Azul (rgba añade un cuarto valor para alpha), hsv Hue Saturación Valor (hsva añade un cuarto argumento para alpha, sinónimo: hsl), strike, bold e italic")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text " text \"volviéndolo a grabaren las manos epilépticas del mar\""
    el "li" $ elClass "div" "ieRef" $ text " size 5 $ colour \"aqua\" $ strike $ bold $ italic $ text \"Siempre como 1 de esas lunas imprevistas\""
    el "li" $ elClass "div" "ieRef" $ text " size 3 $ rgba 0.8 0.5 0 0.5 $ text \"que provocaron el infartote los subways descuidados\""
    el "li" $ elClass "div" "ieRef" $ text " hsv 0.33 0.1 0.3 $ font \"Times New Romans\" $ size 4 $ text \"Ayer: hoy & mañana –de repente–\""
  -- Time functions
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"- Time functions -"),
    (Español,"- Funciones de tiempo -")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Functions that trigger and modify events in time:"),
    (Español,"Funciones que activan o modifican eventos en el tiempo:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English, "Change the program once in time:"),
    (Español, "Funciones que modifican el programa una sola vez en el tiempo:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"quant MultCycle Offset, ramp Dur P1 P2, fadeIn Dur, fadeOut Dur"),
    (Español,"quant MultCycle Offset, ramp Dur P1 P2, fadeIn Dur, fadeOut Dur")
    ])
  --quant
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "quant 4 0 $ hsv (ramp 0.5 0 0.66) 0.5 0.6 $ font \"Times New Romans\" $ size 5 $ text \"Ayer: hoy & mañana –de repente\" -- in the next beat multiple of 4 the change will take place, test it with this miniTidal code: every 4 (#speed \"-0.5\") $ s \"cp\"" 
  --ramp
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setSize (ramp 3 0 0.5) $ video \"videos/hogweed.mov\""
  --fadeIn and fadeOut
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "size 0.5 $ opacity (0.75 * (fadeIn 1)) $ video \"v.mp4\" -- change fadeIn for fadeOut :)"
    --
  el "div" $ dynText =<< (translatableText $ fromList [
    (English, "Change the program periodically:"),
    (Español, "Funciones que modifican el programa de manera periódica:")
    ])
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"sine Freq, range Min Max Signal"),
    (Español,"sine Freq, range Min Max Signal")
    ])
  --sine
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setPosY (sin 0.1) $ setSize 0.5 $ video \"videos/hogweed.mov\""
  --range
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Modify the amplitude of the signal with range:"),
    (Español,"Es posible modificar la amplitud de la señal con range:")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "setBlur (range 0 20 $ sin 0.1) $ size 0.5 $ video \"videos/hogweed.mov\""
  -- Duration
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Functions that modify the rate and position of the video reproduction: every Dur Offset, snap Offset, snapMetre Offset, seg StartPos EndPos Cycles, freeSeg StartPos EndPos, freeRun and rate RateVal."),
    (Español,"Funciones que modifican la velocidad y posición de reproducción del video: every Dur Offset, snap Offset, snapMetre Offset, seg StartPos EndPos Cycles, freeSeg StartPos EndPos, freeRun and rate RateVal.")
    ])
  -- every
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Function every stretches/compresses the video's speed to adapt to a duration in cycles. The offset aligns the start of the video with the start of the cycle if it is 0."),
    (Español,"La función every elonga/comprime la velocida de reproducción del video para ajustarse a una duración dada en ciclos. El offser alinea el primer cuadro del video con el inicio del ciclo si su valor es 0.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "every 3 0 $ video \"videos/hogweed.mov\""
  -- snap
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Function snap adjusts the duration of the video to the closest number of cycles. The offset aligns the start of the video with the start of the cycle if it is 0."),
    (Español,"La función snap ajusta la duración del video al número de ciclos más cercano. El offset alinea el primer cuadro del video con el inicio del ciclo si su valor es 0.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "snap 0 $ video \"videos/lamplight.mp4\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"The function snapMetre is similar to snap but adjusts the duration of the video to 2,4,8,16,32, etc. cycles, useful for 'music'-related explorations"),
    (Español,"La función snapMetre es similar a snap solo que esta ajusta la duración del video a 2,4,8,16,32, etc. ciclos, útil para exploraciones relacionadas con 'ideas musicales'.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "snapMetre 0 $ video \"videos/lamplight.mp4\""
  -- seg
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"Function seg reproduces a segment of the video (start to end in percentage) and the rate of the video segment is adjusted to the duration indicated in cycles as third argument."),
    (Español,"seg reproduce solamente el segmento del video (tiempo de in inicio y fin en porcentaje) indicado y la velocidad de reproduccion la determina la duracion en ciclos indicada en el tercer argumento.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "seg 0.25 0.75 2 $ video \"videos/lamplight.mp4\""
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"The function secs allows you to provide a position to start/end a segment in seconds rather than percentage."),
    (Español,"La funcion secs permite establecer la posicion para inciar/terminar un segmento del video en segundos en lugar de porcentaje.")
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "seg (secs 0) (secs 1) 2 $ video \"videos/lamplight.mp4\""
    ])
  -- freeSeg
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"freeSeg is similar to seg but the duration of the segment is determined by the natural rate."),
    (Español,"freeSeg es similar a seg pero la duracion del segmento esta determinada por la velocidad de reproduccion natural del video.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "freeSeg 0.25 0.75 $ video \"videos/lamplight.mp4\""
    -- need an example for (secs X)
  -- freeRun
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"freeRun allows the video to play freely without adjusting its position or rate."),
    (Español,"freeRun permite la reproduccion del video libre sin ajustar la posicion o la velocidad.")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "freeRun $ video \"videos/lamplight.mp4\""
  -- rate
  el "div" $ dynText =<< (translatableText $ fromList [
    (English,"COMING SOON rate allows to control directly the rate of the video."),
    (Español,"PRONTO DISPONIBLE rate permite ajusta la velocidad de reproduccion del video libremente. ")
    ])
  el "ul" $ do
    el "li" $ elClass "div" "ieRef" $ text "-- rate 2 $ video \"videos/lamplight.mp4\""
