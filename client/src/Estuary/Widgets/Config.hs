{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Config where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import qualified Data.Text as T
import Data.Map.Strict
import Text.Read (readMaybe)
import Data.Maybe

import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Widgets.ViewEditor
import Estuary.Types.Language
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term
import Estuary.Types.Hint

import qualified Sound.Punctual.Resolution as Punctual

configWidget :: MonadWidget t m => W t m ()
configWidget = do

  divClass "config-option primary-color ui-font" $ do
    text "Canvas: "
    elClass "label" "switch" $ do
      canvasOn >>= checkboxW >>= setCanvasOn
      elClass "span" "slider round" $ return ()
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Enable the canvas to display visual results."),
      (Español, "Habilita el lienzo (canvas) para mostrar los visuales.")
      ])

  elClass "hr" "dashed" $ return ()

  divClass "config-option primary-color ui-font" $ do
    term Term.Resolution >>= dynText
    text ": "
    let resolutions = [Punctual.QHD,Punctual.FHD,Punctual.HD,Punctual.WSVGA,Punctual.SVGA,Punctual.VGA,Punctual.QVGA]
    let resMap = fromList $ zip resolutions $ fmap (T.pack . show) resolutions
    resolution >>= dropdownW resMap >>= setResolution
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"The resolution used for rendering visuals to the canvas. Values higher on the list require a more powerful graphics processing unit (GPU) and may result in a lower frames per second (FPS)."),
      (Español, "La resolución utilizada para representar imágenes en el lienzo/canvas. Los valores más altos en la lista requieren una unidad de procesamiento de gráficos (GPU) más potente y pueden resultar en cuadros por segundo (FPS) más bajos.")
      ])

  elClass "hr" "dashed" $ return ()

  divClass "config-option primary-color ui-font" $ do
    text "FPS:"
    let fpsMap = fromList [(Just 0.0333,"30"),(Just 0.0666,"15"),(Just 0.0166,"60"),(Nothing,"unlimited")]
    fpsLimit >>= dropdownW fpsMap >>= setFpsLimit
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"By default, visuals are limited to rendering at ~30 FPS. It is possible to set other limits and to have no limits on frame rate. Higher frame rates and unlimited frame rates may increase demands on the computer's CPU and GPU."),
      (Español, "De forma predeterminada, las imágenes están limitadas a renderizar a ~ 30 FPS. Es posible establecer otros límites y no tener límites en la velocidad de fotogramas. Las velocidades de fotogramas más altas y las velocidades de fotogramas ilimitadas pueden requerir más esfuerzo del CPU y el GPU de la computadora.")
      ])

  elClass "hr" "dashed" $ return ()

  divClass "config-option primary-color ui-font" $ do
    term Term.Brightness >>= dynText
    text ": "
    let brightnessMap = fromList [(1.0,"100%"),(0.5,"50%"),(0.25,"25%"),(0.1,"10%")]
    brightness >>= dropdownW brightnessMap >>= setBrightness
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"The brightness of all rendered visuals. Can be used to make it easier to see code against bright, generated visuals."),
      (Español, "Éste es el brillo de todas las imágenes renderizadas. Éste parámetro puede usarse para que sea más fácil ver el código que está frente a imágenes brillantes.")
      ])

  elClass "hr" "dashed" $  text ""

  divClass "config-option primary-color ui-font" $ do
    text "CineCer0 z-index: "
    cineCer0ZIndex >>= intTextInputW >>= setCineCer0ZIndex

  divClass "config-option primary-color ui-font" $ do
    text "Punctual z-index: "
    punctualZIndex >>= intTextInputW >>= setPunctualZIndex

  divClass "config-option primary-color ui-font" $ do
    text "Improviz z-index: "
    improvizZIndex >>= intTextInputW >>= setImprovizZIndex

  divClass "config-option primary-color ui-font" $ do
    text "Hydra z-index: "
    hydraZIndex >>= intTextInputW >>= setHydraZIndex

  divClass "config-option primary-color ui-font" $ do
    el "div" $ dynText =<< (translatableText $ fromList [
      (English, "Z-index controls the vertical stacking order of visual renders (highest number = top layer). By default, CineCer0 is on the top, followed by Punctual, Improviz and, on the bottom, Hydra."),
      (Español, "El z-index controla el apilado vertical de capas visuales (el valor más alto = capa superior). Por defecto, CineCer0 se ubica en la capa superior seguido por Punctual e Improviz, en la capa inferior de visualiza Hydra.")
      ])

  elClass "hr" "dashed" $ return ()

  webDirtEnabledEv <- divClass "config-option primary-color ui-font" $ do
    text "WebDirt: "
    elClass "label" "switch" $ do
      webDirtOn >>= checkboxW >>= setWebDirtOn
      elClass "span" "slider round" $ return ()
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"WebDirt is the default, built-in rendering engine for sample events in Estuary. It uses the Web Audio API to play samples and thus requires no additional software to be installed."),
      (Español, "WebDirt es el motor de renderizado predeterminado para eventos relacionados con las muestras de audio (sample events) en Estuary. Utiliza la API de Web Audio para reproducir muestras y, por lo tanto, no requiere la instalación de software adicional.")
      ])

  elClass "hr" "dashed" $  text ""

  dynamicsModeEv <- divClass "config-option primary-color ui-font" $ do
    text "Dynamics: "
    let dmMap = fromList $ zip dynamicsModes (fmap (T.pack . show) dynamicsModes)
    dynamicsMode >>= dropdownW dmMap >>= setDynamicsMode
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Control of a global compressor that affects all audio produced within the browser by Estuary. The Loud preset is intended for noisy environments, and the Wide preset is intended for situations where many streams of audio are being combined and sent to further processing (eg. compression and gain) outside of the browser."),
      (Español, "Éste es el control del compresor global que afecta a todo el audio producido dentro del navegador por Estuary. El preset 'Loud' está diseñado para entornos ruidosos, y el preset 'Wide' está diseñado para situaciones en las que se combinan muchas secuencias de audio y se envían para su procesamiento posterior (p. Ej. compresión y ganancia) fuera del navegador.")
      ])

  elClass "hr" "dashed" $ text ""

  punctualAudioInputModeEv <- divClass "config-option primary-color ui-font" $ do
    text "Punctual Audio Input: "
    let paimMap = fromList $ zip punctualAudioInputModes (fmap (T.pack . show) punctualAudioInputModes)
    punctualAudioInputMode >>= dropdownW paimMap >>= setPunctualAudioInputMode
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Control of how microphone/WebDirt audio is routed to Punctual."),
      (Español,"Éste parámetro controla cómo se enruta el audio del micrófono y/o de WebDirt a Punctual.")
      ])

  elClass "hr" "dashed" $  text ""

  superDirtEnabledEv <- divClass "config-option primary-color ui-font" $ do
    text "SuperDirt: "
    elClass "label" "switch" $ do
      superDirtOn >>= checkboxW >>= setSuperDirtOn
      elClass "span" "slider round" $ return ()
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"When enabled, sample events are sent out of the browser to be processed by other software. Requires the installation and use of additional software (eg. SuperDirt and the \"superDirtSocket\")."),
      (Español, "Cuando está habilitado, los eventos de muestra se envían fuera del navegador para ser procesados por otro software. Requiere la instalación y el uso de software adicional (por ejemplo, SuperDirt y \"superDirtSocket \").")
      ])

  elClass "hr" "dashed" $  text ""

  unsafeModeEv <- divClass "config-option primary-color ui-font" $ do
    text "Unsafe Mode:"
    elClass "label" "switch" $ do
      unsafeModeOn >>= checkboxW >>= setUnsafeModeOn
      elClass "span" "slider round" $ return ()
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Activating unsafe mode may make additional functionality available at the risk of catastrophic performance problems. Use at own risk."),
      (Español, "La activación del 'unsafe mode' (modo no seguro) puede habilitar funcionalidades adicionales disponibles, siempre a riesgo de problemas de rendimiento catastróficos. Úselo bajo su propia responsabilidad.")
      ])

  elClass "hr" "dashed" $ return ()

  divClass "config-option primary-color ui-font" $ do
    el "h3" $ dynText =<< (translatableText $ fromList [
      (English, "View Editor"),
      (Español, "Editor de vistas/layouts")
      ])
    viewEditor
