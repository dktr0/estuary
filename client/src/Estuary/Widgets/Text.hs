{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.Trans
import GHCJS.DOM.EventM
import Control.Monad.Fix (MonadFix)
import Data.Maybe
import Data.Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Time
import Data.Bool
import Data.List

import GHCJS.Types
import GHCJS.DOM.Types (HTMLDivElement)

import Estuary.Widgets.Reflex
import Estuary.Tidal.Types
import Estuary.Widgets.GeneralPattern
import Estuary.Widgets.Reflex
import Estuary.Utility (lastOrNothing)
import Estuary.Types.Definition
import Estuary.Types.Hint
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Help.LanguageHelp
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Widgets.W
import Estuary.Types.CodeWidgetOptions


foreign import javascript unsafe "navigator.clipboard.writeText($1);" copyToClipboard :: Text -> IO ()


textWidgetClass :: Bool -> Map Text Text
textWidgetClass True = "class" =: "evalFlash textInputToEndOfLine code-font"
textWidgetClass False = "class" =: "primary-color textInputToEndOfLine code-font"

textWidgetRows :: Int -> Map Text Text
textWidgetRows 0 = Data.Map.empty
textWidgetRows x = "rows" =: T.pack (show x)


stilosEditors :: [CodeWidgetOptions] -> Text -> Map Text Text
stilosEditors vs i = do
  let s' = stilos vs i
  "style" =: ("height: auto;" <> s')

-- stilos :: [CodeWidgetOptions] -> Text -> Text
-- stilos vs i = do
--   let a = if elem Fluxus vs then (fluxusStyle i) else ""
--   let b = if elem CentreAlign vs then "text-align: center;" else ""
--   let c = if elem RightAlign vs then "text-align: right;" else ""
--   a <> b <> c

stilos :: [CodeWidgetOptions] -> Text -> Text
stilos vs i = do
  let a = fmap (cwoToText i) vs -- :: [Text]
  mconcat a

cwoToText :: Text -> CodeWidgetOptions -> Text
cwoToText i Fluxus = fluxusStyle i
cwoToText i CentreAlign = "text-align: center;"
cwoToText i RightAlign = "text-align: right;"
cwoToText i (Fontsize x) = customFontSize x
cwoToText _ _ = ""

customFontSize :: Double -> Text
customFontSize s = "font-size: " <> (T.pack $ show s) <> "em;"
-- fontsize: 10em


-- for code-box-editors
textWidget' :: MonadWidget t m => [CodeWidgetOptions] -> Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget' styles rows flash i delta = mdo
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let style' = fmap (stilosEditors styles) value
  let attrs = mconcat [class',rows', style']
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let value = _textArea_value x
  let edits = _textArea_input x
  shiftEnter <- catchKeyboardShortcut (_textArea_element x) 13 False True
  return (value,edits,shiftEnter)


fluxusStyle :: Text -> Text
fluxusStyle i
  | (T.length i <= 5) && ((L.length $ T.lines i) <= 1) = "font-size: 8em;"
  | (T.length i >= 6) && (T.length i <= 10) && ((L.length $ T.lines i) <= 1) = "font-size: 7em;"
  | (T.length i >= 11) && (T.length i <= 20) && ((L.length $ T.lines i) <= 2) || ((L.length $ T.lines i) <= 2) = "font-size: 6em;"
  | (T.length i >= 21) && (T.length i <= 30) && ((L.length $ T.lines i) <= 3) || ((L.length $ T.lines i) <= 3) = "font-size: 5em;"
  | (T.length i >= 31) && (T.length i <= 40) && ((L.length $ T.lines i) <= 4) || ((L.length $ T.lines i) <= 4) = "font-size: 4em;"
  | (T.length i >= 41) && (T.length i <= 60) && ((L.length $ T.lines i) <= 5) || ((L.length $ T.lines i) <= 5) = "font-size: 3em;"
  | (T.length i >= 61) && (T.length i <= 90) && ((L.length $ T.lines i) <= 6) || ((L.length $ T.lines i) <= 6) = "font-size: 2.5em;"
  | (T.length i >= 91) && (T.length i <= 120) && ((L.length $ T.lines i) <= 12) || (((L.length $ T.lines i) >= 7) && ((L.length $ T.lines i) <= 10)) = "font-size: 2em;"
  | (T.length i >= 121) && (T.length i <= 140) && ((L.length $ T.lines i) <= 12) || (((L.length $ T.lines i) >= 10) && ((L.length $ T.lines i) <= 13)) = "font-size: 1.5em;"
  | otherwise = "font-size: 1em;"


-- for everything that is not code-box-editors
textWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget rows flash i delta = do
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let style = constDyn $ "style" =: "height: auto"
  let attrs = mconcat [class',rows',style] -- :: Dynamic t (Map Text Text)
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let value = _textArea_value x
  let edits = _textArea_input x
  shiftEnter <- catchKeyboardShortcut (_textArea_element x) 13 False True
  return (value,edits,shiftEnter)


                                     --  Rows  Colour     EditableOrNot
textToInvisible :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textToInvisible rows invisible delta = do
  i <- sample $ current delta
--  let class' = constDyn $ "class" =: "exp-color textInputToEndOfLine code-font"
  let class' = fmap textToInvisibleClass invisible
  let rows' = constDyn $ textWidgetRows rows
  let readon = lockText <$> invisible
  let style = constDyn $ styleFunc
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style]
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits)


textWithLockAndClipBoardWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textWithLockAndClipBoardWidget rows editable delta = do
  i <- sample $ current delta
--  let class' = constDyn $ "class" =: "exp-color textInputToEndOfLine code-font"
  let class' = fmap textWithLockWidgetClass editable
  let rows' = constDyn $ textWidgetRows rows
  let readon = lockText <$> editable
  let style = constDyn $ styleFunc
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style]
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x -- Event t String
  let value = _textArea_value x -- Dynamic t String

  butt <- button "to ClipBoard"
  let xx = tag (current $ value) butt

  performEvent $ fmap (liftIO . copyToClipboard) xx

  ----- to clipboard ------

  return (value,edits)


                                     --  Rows  Colour     EditableOrNot
textWithLockWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textWithLockWidget rows editable delta = do
  i <- sample $ current delta
--  let class' = constDyn $ "class" =: "exp-color textInputToEndOfLine code-font"
  let class' = fmap textWithLockWidgetClass editable
  let rows' = constDyn $ textWidgetRows rows
  let readon = lockText <$> editable
  let style = constDyn $ styleFunc
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style]
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits)

styleFunc:: Map Text Text
styleFunc = "style" =: "height: auto; font-size: 1em;"

lockText:: Bool -> Map Text Text
lockText False = Data.Map.empty
lockText True = "readonly" =: ""

textWithLockWidgetClass :: Bool -> Map Text Text
textWithLockWidgetClass True = "class" =: "human-to-human-comm textInputToEndOfLine code-font"
textWithLockWidgetClass False = "class" =: "primary-color textInputToEndOfLine code-font"

textToInvisibleClass:: Bool -> Map Text Text
textToInvisibleClass True =  "class" =: "invisible-color textInputToEndOfLine code-font"
textToInvisibleClass False = "class" =: "primary-color textInputToEndOfLine code-font"


textNotationParsers :: [TextNotation]
textNotationParsers = [UnspecifiedNotation, Punctual, CineCer0, TimeNot, Seis8s, Hydra] ++ (fmap TidalTextNotation tidalParsers)

holdUniq :: (MonadHold t m, Monad m, Reflex t, MonadFix m, Eq a) => a -> Event t a -> m (Event t a)
holdUniq i e = holdDyn i e >>= holdUniqDyn >>= return . updated

textProgramEditor :: forall t m. MonadWidget t m => [CodeWidgetOptions] -> Int -> Dynamic t (Maybe Text)
  -> Dynamic t (Live TextProgram) -> W t m (Variable t (Live TextProgram))
textProgramEditor styles rows errorText deltasDown = divClass "textPatternChain" $ mdo -- *** TODO: change css class

  -- translate deltasDown into initial value and events that reflect remote changes that will affect local GUI
  i <- sample $ current deltasDown
  let initialParser = (\(x,_,_) -> x) $ forEditing i
  let initialText = (\(_,x,_) -> x) $ forEditing i
  let deltaFuture = fmap forEditing $ updated deltasDown

  let parserDelta = attachWithMaybe (\(x,_,_) (y,_,_) -> if x==y then Nothing else Just y) (fmap forEditing $ current $ currentValue cv) deltaFuture
  let textDelta = attachWithMaybe (\(_,x,_) (_,y,_) -> if x==y then Nothing else Just y) (fmap forEditing $ current $ currentValue cv) deltaFuture
  errorText' <- holdUniqDyn errorText

  evalTimeDyn <- holdUniqDyn $ fmap ((\(_,_,x)->x) . forRendering) $ currentValue cv
  let flashOn = True <$ updated evalTimeDyn
  flashOff <- liftM (False <$) $ delay 0.1 flashOn
  evalFlash <- holdDyn False $ leftmost [flashOff,flashOn]

  (parserEdit,evalButton) <- divClass "fullWidthDiv" $ do
    -- build dropdown menu if "nomenu" is not among the provided options
    dropdown' <- if elem Nomenu styles then (return never) else do
      let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers
      let ddAttrs = ((def :: DropdownConfig t TidalParser) & attributes .~ constDyn ("class" =: "ui-dropdownMenus code-font primary-color primary-borders")) & dropdownConfig_setValue .~ parserDelta
      d <- dropdown initialParser parserMap ddAttrs
      return $ _dropdown_change d
    -- build eval button if "noeval" is not among the provided options
    evalButton' <- if elem Noeval styles then (return never) else do
      divClass "textInputLabel" $ dynButton "\x25B6"
    -- build error text display if "noerrors" is not among the provided options
    if elem Noerrors styles then (return ()) else do
      widgetHold (return ()) $ fmap (maybe (return ()) syntaxErrorWidget) $ updated errorText'
      pure ()
    return (dropdown',evalButton')

  (_,textEdit,shiftEnter) <- divClass "labelAndTextPattern" $ textWidget' styles rows evalFlash initialText textDelta
  evalEdit <- performEvent $ fmap (liftIO . const getCurrentTime) $ leftmost [evalButton,shiftEnter]
  let c = current $ currentValue cv
  let parserEdit' = attachWith applyParserEdit c parserEdit
  let textEdit' = attachWith applyTextEdit c textEdit
  let evalEdit' = attachWith applyEvalEdit c evalEdit
  let localEdits = leftmost [parserEdit',textEdit',evalEdit']
  cv <- returnVariable deltasDown localEdits
  return cv

applyParserEdit :: Live TextProgram -> TextNotation -> Live TextProgram
applyParserEdit (Live (x,y,z) _) x' = Edited (x,y,z) (x',y,z)
applyParserEdit (Edited (x,y,z) (_,y',z')) x' = Edited (x,y,z) (x',y',z')

applyTextEdit :: Live TextProgram -> Text -> Live TextProgram
applyTextEdit (Live (x,y,z) _) y' = Edited (x,y,z) (x,y',z)
applyTextEdit (Edited (x,y,z) (x',_,z')) y' = Edited (x,y,z) (x',y',z')

applyEvalEdit :: Live TextProgram -> UTCTime -> Live TextProgram
applyEvalEdit (Live (x,y,_) _) z = Live (x,y,z) L3
applyEvalEdit (Edited _ (x,y,_)) z = Live (x,y,z) L3


labelEditor :: MonadWidget t m => Dynamic t Text -> W t m (Variable t Text)
labelEditor delta = do
  let attrs = constDyn $ ("class" =: "name-tag-textarea code-font primary-color")
  y <- divClass "labelWidgetDiv" $ do
    i <- sample $ current delta
    textInput $ def & textInputConfig_setValue .~ (updated delta) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  returnVariable delta $ _textInput_input y

syntaxErrorWidget :: (Monad m, Reflex t, PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m) => Text -> W t m ()
syntaxErrorWidget t = do
  s <- term Term.Syntax
  let wb = elClass "div" "syntaxIssue" $ dynText s
  tooltip wb (text t)
  return ()
