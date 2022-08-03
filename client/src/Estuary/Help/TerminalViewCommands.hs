{-# LANGUAGE OverloadedStrings #-}

module Estuary.Help.TerminalViewCommands where

import qualified Data.Map.Strict as Map

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex
import Estuary.Types.Language
import Estuary.Types.TranslatableText
import Estuary.Types.Definition
import qualified Estuary.Types.Term as Term
import Control.Monad.Fix


terminalViewCommandsHelpFile :: MonadWidget t m => W t m ()
terminalViewCommandsHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
  about
  functionRef "!localview"
  functionRef "!presetview"
  functionRef "!publishview"
  functionRef "!activeview"
  functionRef "!listviews"
  functionRef "!dumpview"
  return ()

about :: (Monad m, Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => W t m ()
about = do
  divClass "about primary-color code-font" $ dynText =<< (translatableText (Map.fromList [(English, "A flexible view system is provided in order to create layouts for a wide range of purposes."), (Español, "Se proporciona un sistema de visualización flexible para crear diseños (layouts) para una amplia gama de propósitos.")]))


exampleText :: Text -> Text
exampleText "!localview" = "!localview 1x1 [border {[label:0, text:0 0]}]"
exampleText "!presetview" = "!presetview cybernetic"
exampleText "!publishview" = "!publishview basic"
exampleText "!activeview" =  "!activeview"
exampleText "!listviews" = "!listviews"
exampleText "!dumpview" = "!dumpview"
--
referenceTextEnglish :: Text -> Text
referenceTextEnglish "!localview" = "Creates a view"
referenceTextEnglish "!presetview" = "Loads a view by name"
referenceTextEnglish "!publishview" = "Publishes local view with a name"
referenceTextEnglish "!activeview" =  "Returns the name of the current view."
referenceTextEnglish "!listviews" = "Lists all preset views."
referenceTextEnglish "!dumpview" = "Returns the current view's layout."
--
referenceTextSpanish :: Text -> Text
referenceTextSpanish "!localview" = "Crea una vista/layout."
referenceTextSpanish "!presetview" = "Carga una vista/layout usando el nombre de un preset."
referenceTextSpanish "!publishview" = "Publica la vista/layout local usando el nombre de la vista."
referenceTextSpanish "!activeview" =  "Devuelve el nombre de la vista/layout actual."
referenceTextSpanish "!listviews" = "Muestra todas las vistas/layouts preestablecidas."
referenceTextSpanish "!dumpview" = "Devuelve el diseño de la vista/layout actual."
--
functionRef :: (DomBuilder t m, Monad m, Reflex t, PostBuild t m, MonadHold t m, MonadFix m) => Text -> W t m ()
functionRef x = divClass "helpWrapper" $ do
  switchToReference <- buttonWithClass' x
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText primary-color code-font" $ text (exampleText x)
  hideableWidget referenceVisible "referenceText code-font" $ dynText =<< (translatableText (Map.fromList [(English, referenceTextEnglish x), (Español, referenceTextSpanish x)]))
  return ()
