{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Config where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import qualified Data.Text as T
import Data.Map.Strict
import Text.Read(readMaybe)
import Data.Maybe

import Estuary.Widgets.Editor
import Estuary.Widgets.Reflex
import Estuary.Widgets.ViewEditor
import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term
import Estuary.Types.RenderInfo

import qualified Sound.Punctual.Resolution as Punctual

configWidget :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo -> Editor t m (Event t ContextChange)
configWidget ctx ri = do

  canvasEnabledEv <- divClass "config-option primary-color ui-font" $ do
    text "Canvas: "
    canvasInput <- elClass "label" "switch" $ do
      x <- checkbox True def
      elClass "span" "slider round" (return x)
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Enable the canvas to display visual results."),
      (Español, "Habilita el lienzo (canvas) para mostrar los visuales.")
      ])
    return $ fmap (\x -> \c -> c { canvasOn = x }) $ _checkbox_change canvasInput

  elClass "hr" "dashed" $ return ()

  resolutionChangeEv <- divClass "config-option primary-color ui-font" $ do
    term Term.Resolution >>= dynText
    text ": "
    let resolutions = [Punctual.QHD,Punctual.FHD,Punctual.HD,Punctual.WSVGA,Punctual.SVGA,Punctual.VGA,Punctual.QVGA]
    let resMap = fromList $ zip resolutions $ fmap (T.pack . show) resolutions
    resChange <- _dropdown_change <$> dropdown Punctual.HD (constDyn resMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"The resolution used for rendering visuals to the canvas. Values higher on the list require a more powerful graphics processing unit (GPU) and may result in a lower frames per second (FPS)."),
      (Español, "La resolución utilizada para representar imágenes en el lienzo/canvas. Los valores más altos en la lista requieren una unidad de procesamiento de gráficos (GPU) más potente y pueden resultar en cuadros por segundo (FPS) más bajos.")
      ])
    return $ fmap (\x c -> c { resolution = x }) resChange

  elClass "hr" "dashed" $ return ()

  fpsLimitChangeEv <- divClass "config-option primary-color ui-font" $ do
    text "FPS:"
    let fpsMap = fromList [(Just 0.030,"30"),(Just 0.060,"15"),(Just 0.015,"60"),(Nothing,"unlimited")]
    fpsChange <- _dropdown_change <$> dropdown (Just 0.030) (constDyn fpsMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"By default, visuals are limited to rendering at ~30 FPS. It is possible to set other limits and to have no limits on frame rate. Higher frame rates and unlimited frame rates may increase demands on the computer's CPU and GPU."),
      (Español, "De forma predeterminada, las imágenes están limitadas a renderizar a ~ 30 FPS. Es posible establecer otros límites y no tener límites en la velocidad de fotogramas. Las velocidades de fotogramas más altas y las velocidades de fotogramas ilimitadas pueden requerir más esfuerzo del CPU y el GPU de la computadora.")
      ])
    return $ fmap (\x c -> c { fpsLimit = x }) fpsChange

  elClass "hr" "dashed" $ return ()

  brightnessChangeEv <- divClass "config-option primary-color ui-font" $ do
    term Term.Brightness >>= dynText
    text ": "
    let brightnessMap = fromList [(1.0,"100%"),(0.5,"50%"),(0.25,"25%"),(0.1,"10%")]
    brightnessChange <- _dropdown_change <$> dropdown 1.0 (constDyn brightnessMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"The brightness of all rendered visuals. Can be used to make it easier to see code against bright, generated visuals."),
      (Español, "Éste es el brillo de todas las imágenes renderizadas. Éste parámetro puede usarse para que sea más fácil ver el código que está frente a imágenes brillantes.")
      ])
    return $ fmap (\x c -> c { brightness = x }) brightnessChange

  elClass "hr" "dashed" $  text ""

  cineCer0ZIndexChangeEv <- divClass "config-option primary-color ui-font" $ do
    zIndexInput <- elClass "div" "Numeric Field with initial value" $ do
      text "CineCer0 z-index: "
      tInt <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "-1"
                              & attributes .~ constDyn ("class" =: "ui-inputMenus primary-color primary-borders ui-font")
      return $ _textInput_value tInt -- :: Dynamic t Text
    return $ fmap (\x -> \c -> c { cineCer0ZIndex = x }) (fmap (\x -> if isJust (readMaybe (T.unpack x)::Maybe Int) then (read (T.unpack x)::Int) else (-1)) zIndexInput) -- :: Dynamic t (Context -> Context)

    --(fmap (\(x) -> fromJust (readMaybe $ (T.unpack x) :: Maybe Int)) zIndexInput) -- :: Dynamic t (Context -> Context)

  punctualZIndexChangeEv <- divClass "config-option primary-color ui-font" $ do
    zIndexInput <- elClass "div" "Numeric Field with initial value" $ do
      text "Punctual z-index: "
      tInt <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "-2"
                              & attributes .~ constDyn ("class" =: "ui-inputMenus primary-color primary-borders ui-font")
      return $ _textInput_value tInt -- :: Dynamic t Text
    return $ fmap (\x -> \c -> c { punctualZIndex = x }) (fmap (\x -> if isJust (readMaybe (T.unpack x)::Maybe Int) then (read (T.unpack x)::Int) else (-2)) zIndexInput) -- :: Dynamic t (Context -> Context)

  hydraZIndexChangeEv <- divClass "config-option primary-color ui-font" $ do
    zIndexInput <- elClass "div" "Numeric Field with initial value" $ do
      text "Hydra z-index: "
      tInt <- textInput $ def & textInputConfig_inputType .~ "number"
                              & textInputConfig_initialValue .~ "-20"
                              & attributes .~ constDyn ("class" =: "ui-inputMenus primary-color primary-borders ui-font")
      return $ _textInput_value tInt -- :: Dynamic t Text
    return $ fmap (\x -> \c -> c { hydraZIndex = x }) (fmap (\x -> if isJust (readMaybe (T.unpack x)::Maybe Int) then (read (T.unpack x)::Int) else (-10)) zIndexInput) -- :: Dynamic t (Context -> Context)

  zIndexInfo <- divClass "config-option primary-color ui-font" $ do
    el "div" $ dynText =<< (translatableText $ fromList [
      (English, "Z-index controls the vertical stacking order of visual renders (highest number = top layer). By default, CineCer0 is on the top, following by Punctual and, on the bottom, Hydra."),
      (Español, "El z-index controla el apilado vertical de capas visuales (el valor más alto = capa superior). Por defecto, CineCer0 se ubica en la capa superior seguido por Punctual, en la capa inferior de visualiza Hydra.")
      ])

  elClass "hr" "dashed" $ return ()


  webDirtEnabledEv <- divClass "config-option primary-color ui-font" $ do
    text "WebDirt: "
    wdInput <-elClass "label" "switch" $ do
      x <- checkbox True def
      elClass "span" "slider round" (return x)
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"WebDirt is the default, built-in rendering engine for sample events in Estuary. It uses the Web Audio API to play samples and thus requires no additional software to be installed."),
      (Español, "WebDirt es el motor de renderizado predeterminado para eventos relacionados con las muestras de audio (sample events) en Estuary. Utiliza la API de Web Audio para reproducir muestras y, por lo tanto, no requiere la instalación de software adicional.")
      ])
    return $ fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_change wdInput

  elClass "hr" "dashed" $  text ""

  dynamicsModeEv <- divClass "config-option primary-color ui-font" $ do
    text "Dynamics: "
    let dmMap = fromList $ zip dynamicsModes (fmap (T.pack . show) dynamicsModes)
    dmChange <- _dropdown_change <$> dropdown DefaultDynamics (constDyn dmMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" <> "style" =: "background-color: transparent"))
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Control of a global compressor that affects all audio produced within the browser by Estuary. The Loud preset is intended for noisy environments, and the Wide preset is intended for situations where many streams of audio are being combined and sent to further processing (eg. compression and gain) outside of the browser."),
      (Español, "Éste es el control del compresor global que afecta a todo el audio producido dentro del navegador por Estuary. El preset 'Loud' está diseñado para entornos ruidosos, y el preset 'Wide' está diseñado para situaciones en las que se combinan muchas secuencias de audio y se envían para su procesamiento posterior (p. Ej. compresión y ganancia) fuera del navegador.")
      ])
    return $ fmap (\x c -> c { dynamicsMode = x }) dmChange -- context -> context

  elClass "hr" "dashed" $  text ""

  punctualAudioInputModeEv <- divClass "config-option primary-color ui-font" $ do
    text "Punctual Audio Input: "
    let paimMap = fromList $ zip punctualAudioInputModes (fmap (T.pack . show) punctualAudioInputModes)
    paimChange <- _dropdown_change <$> dropdown MicToPunctual (constDyn paimMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" <> "style" =: "background-color: transparent"))
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Control of how microphone/WebDirt audio is routed to Punctual."),
      (Español,"Éste parámetro controla cómo se enruta el audio del micrófono y/o de WebDirt a Punctual.")
      ])
    return $ fmap (\x c -> c { punctualAudioInputMode = x }) paimChange -- context -> context

  elClass "hr" "dashed" $  text ""

  superDirtEnabledEv <- divClass "config-option primary-color ui-font" $ do
    text "SuperDirt: "
    sdInput <- elClass "label" "switch" $ do
      x <- checkbox False def
      elClass "span" "slider round" (return x)
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"When enabled, sample events are sent out of the browser to be processed by other software. Requires the installation and use of additional software (eg. SuperDirt and the \"superDirtSocket\")."),
      (Español, "Cuando está habilitado, los eventos de muestra se envían fuera del navegador para ser procesados por otro software. Requiere la instalación y el uso de software adicional (por ejemplo, SuperDirt y \"superDirtSocket \").")
      ])
    return $ fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_change sdInput

  elClass "hr" "dashed" $  text ""

  unsafeModeEv <- divClass "config-option primary-color ui-font" $ do
    text "Unsafe Mode:"
    unsafeInput <- elClass "label" "switch" $ do
      x <- checkbox False def
      elClass "span" "slider round" (return x)
    el "div" $ dynText =<< (translatableText $ fromList [
      (English,"Activating unsafe mode may make additional functionality available at the risk of catastrophic performance problems. Use at own risk."),
      (Español, "La activación del 'unsafe mode' (modo no seguro) puede habilitar funcionalidades adicionales disponibles, siempre a riesgo de problemas de rendimiento catastróficos. Úselo bajo su propia responsabilidad.")
      ])
    return $ fmap (\x -> \c -> c { unsafeMode = x }) $ _checkbox_change unsafeInput

  elClass "hr" "dashed" $ return ()

  viewEditorChange <- divClass "config-option primary-color ui-font" $ do
    el "h3" $ dynText =<< (translatableText $ fromList [
      (English, "View Editor"),
      (Español, "Editor de vistas/layouts")
      ])
    viewEditor

  return $ mergeWith (.) [punctualAudioInputModeEv,canvasEnabledEv, superDirtEnabledEv, webDirtEnabledEv, dynamicsModeEv, resolutionChangeEv, brightnessChangeEv, (updated cineCer0ZIndexChangeEv), (updated punctualZIndexChangeEv), (updated hydraZIndexChangeEv), viewEditorChange, fpsLimitChangeEv, unsafeModeEv]
