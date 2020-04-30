{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Header where

import Reflex
import Reflex.Dom hiding (Request,Response)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import Estuary.Types.Language
import qualified Estuary.Types.Term as Term
import Estuary.Render.DynamicsMode
import Estuary.Reflex.Utility
import Estuary.Widgets.ResourceUpload
import Estuary.Widgets.Editor


header :: MonadWidget t m => Editor t m (Event t ContextChange)
header = divClass "header primary-color primary-borders" $ divClass "config-toolbar" $ do

  themeChangeEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
    let styleMap =  fromList [("../css-custom/classic.css", "Classic"),("../css-custom/dark.css", "Dark" ),("../css-custom/inverse.css","Inverse"), ("../css-custom/grayscale.css","Grayscale"), ("../css-custom/bubble.css","Bubble"), ("../css-custom/memorias.css","Memorias")]
    term Term.Theme >>= dynText
    styleChange <- _dropdown_change <$> dropdown "../css-custom/classic.css" (constDyn styleMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" )) -- Event t String
    return $ fmap (\x c -> c {theme = x}) styleChange -- Event t (Context -> Context)

  langChangeEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
    term Term.Language >>= dynText
    let langMap = fromList $ zip languages (fmap (T.pack . show) languages)
    langChange <- _dropdown_change <$> dropdown English (constDyn langMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font"))
    return $ fmap (\x c -> c { language = x }) langChange

  let condigCheckboxAttrs = def & attributes .~ constDyn ("class" =: "primary-color")

  canvasEnabledEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
    text "Canvas:"
    canvasInput <- checkbox True condigCheckboxAttrs
    return $ fmap (\x -> \c -> c { canvasOn = x }) $ _checkbox_change canvasInput

  superDirtEnabledEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
    text "SuperDirt:"
    sdInput <- checkbox False condigCheckboxAttrs
    return $ fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_change sdInput

  webDirtEnabledEv <- divClass "config-entry display-inline-block primary-color ui-font" $ do
    text "WebDirt:"
    wdInput <- checkbox True condigCheckboxAttrs
    return $ fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_change wdInput

  dynamicsModeEv <- divClass "config-entry primary-color ui-font" $ do
    text "Dynamics:"
    let dmMap = fromList $ zip dynamicsModes (fmap (T.pack . show) dynamicsModes)
    dmChange <- _dropdown_change <$> dropdown DefaultDynamics (constDyn dmMap) (def & attributes .~ constDyn ("class" =: "ui-dropdownMenus primary-color primary-borders ui-font" <> "style" =: "background-color: transparent"))
    return $ fmap (\x c -> c { dynamicsMode = x }) dmChange

  -- privateSamplesChangeEv <- divClass "config-entry primary-color ui-font" resourceUploader

  divClass "config-entry display-inline-block primary-color ui-font" $ dynButton "?"

  return $ mergeWith (.) [themeChangeEv, langChangeEv, canvasEnabledEv, superDirtEnabledEv, webDirtEnabledEv, dynamicsModeEv {-, privateSamplesChangeEv -}]
