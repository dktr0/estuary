{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Config where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import qualified Data.Text as T
import Data.Map.Strict

import Estuary.Widgets.Editor
import Estuary.Widgets.Generic
import Estuary.Types.Context
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term

import qualified Sound.Punctual.Resolution as Punctual

configWidget :: MonadWidget t m => Editor t m (Event t ContextChange)
configWidget = do

  canvasEnabledEv <- divClass "primary-color ui-font" $ do
    text "Canvas:"
    canvasInput <- elClass "label" "switch" $ do
      x <- checkbox True def
      elClass "span" "slider round" (return x)
    return $ fmap (\x -> \c -> c { canvasOn = x }) $ _checkbox_change canvasInput

  superDirtEnabledEv <- divClass "primary-color ui-font" $ do
    text "SuperDirt:"
    sdInput <- elClass "label" "switch" $ do
      x <- checkbox False def
      elClass "span" "slider round" (return x)
    return $ fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_change sdInput

  webDirtEnabledEv <- divClass "primary-color ui-font" $ do
    text "WebDirt:"
    wdInput <-elClass "label" "switch" $ do
      x <- checkbox True def
      elClass "span" "slider round" (return x)
    return $ fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_change wdInput

  dynamicsModeEv <- divClass "primary-color ui-font" $ do
    text "Dynamics:"
    let dmMap = fromList $ zip dynamicsModes (fmap (T.pack . show) dynamicsModes)
    dmChange <- _dropdown_change <$> dropdown DefaultDynamics (constDyn dmMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" <> "style" =: "background-color: transparent"))
    return $ fmap (\x c -> c { dynamicsMode = x }) dmChange -- context -> context

  resolutionChangeEv <- divClass "primary-color ui-font" $ do
    term Term.Resolution >>= dynText
    text ":"
    let resolutions = [Punctual.QHD,Punctual.FHD,Punctual.HD,Punctual.WSVGA,Punctual.SVGA,Punctual.VGA,Punctual.QVGA]
    let resMap = fromList $ zip resolutions $ fmap (T.pack . show) resolutions
    resChange <- _dropdown_change <$> dropdown Punctual.HD (constDyn resMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
    return $ fmap (\x c -> c { resolution = x }) resChange

  brightnessChangeEv <- divClass "primary-color ui-font" $ do
    term Term.Brightness >>= dynText
    text ":"
    let brightnessMap = fromList [(1.0,"100%"),(0.5,"50%"),(0.25,"25%"),(0.1,"10%")]
    brightnessChange <- _dropdown_change <$> dropdown 1.0 (constDyn brightnessMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
    return $ fmap (\x c -> c { brightness = x }) brightnessChange

  return $ mergeWith (.) [canvasEnabledEv, superDirtEnabledEv, webDirtEnabledEv, dynamicsModeEv, resolutionChangeEv, brightnessChangeEv]
