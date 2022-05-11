{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom hiding (getKeyEvent,preventDefault)
import Reflex.Dom.Contrib.KeyEvent
import Control.Monad
import Control.Monad.Trans
import GHCJS.DOM.EventM
import Data.Maybe
import Data.Map
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Time

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
import Estuary.Types.Context


foreign import javascript unsafe "navigator.clipboard.writeText($1);" copyToClipboard :: Text -> IO () -- ok, este import es mas bien para hacer el copy/paste en el texto, no tiene ingerencia en el tipo del textWidget ni nada


textWidgetClass :: Bool -> Map Text Text
textWidgetClass True = "class" =: "evalFlash textInputToEndOfLine codeEditor-font"
textWidgetClass False = "class" =: "primary-color textInputToEndOfLine codeEditor-font"

textWidgetRows :: Int -> Map Text Text
textWidgetRows 0 = Data.Map.empty
textWidgetRows x = "rows" =: T.pack (show x)

defOrFluxus :: Text -> Text -> Map Text Text
defOrFluxus x i
  | x == "def" = "style" =: "height: auto"
  | otherwise = "style" =: "height: auto"
  -- | x == "fluxus" = "class" =: "codeEditor-fluxusFont" <> (fluxusStyle i)


textWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget rows flash i delta = do
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let style = constDyn $ "style" =: "height: auto"
  let attrs = mconcat [class',rows',style] -- :: Dynamic t (Map Text Text)
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let e = _textArea_element x
  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if keyPressWasShiftEnter y then (preventDefault >> return True) else return False
  let evalEvent = fmap (const ()) $ ffilter (==True) e'
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits,evalEvent)
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)


textWidget' :: MonadWidget t m => Text -> Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget' style rows flash i delta = mdo
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let style' = fmap (defOrFluxus style) value
  let attrs = mconcat [class',rows', style']
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let e = _textArea_element x
  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if keyPressWasShiftEnter y then (preventDefault >> return True) else return False
  let evalEvent = fmap (const ()) $ ffilter (==True) e'
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits,evalEvent)
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)


fluxusTextWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
fluxusTextWidget rows flash i delta = mdo
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let fluxusStyle' = fmap fluxusStyle value
  let attrs = mconcat [class',rows', fluxusStyle']
  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let e = _textArea_element x
  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if keyPressWasShiftEnter y then (preventDefault >> return True) else return False
  let evalEvent = fmap (const ()) $ ffilter (==True) e'
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits,evalEvent)
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13)


fluxusStyle :: Text -> Map Text Text
fluxusStyle i
  | (T.length i <= 5) && ((L.length $ T.lines i) <= 1) = "style" =: "font-size: 8em; height: auto"

  | (T.length i >= 6) && (T.length i <= 10) && ((L.length $ T.lines i) == 1) = "style" =: "font-size: 7em; height: auto"

  | (T.length i >= 11) && (T.length i <= 20) && ((L.length $ T.lines i) <= 2) || ((L.length $ T.lines i) == 2) = "style" =: "font-size: 6em; height: auto"

  | (T.length i >= 21) && (T.length i <= 30) && ((L.length $ T.lines i) <= 3) || ((L.length $ T.lines i) == 3) = "style" =: "font-size: 5em; height: auto"

  | (T.length i >= 31) && (T.length i <= 40) && ((L.length $ T.lines i) <= 4) || ((L.length $ T.lines i) == 4) = "style" =: "font-size: 4em; height: auto"

  | (T.length i >= 41) && (T.length i <= 60) && ((L.length $ T.lines i) <= 5) || ((L.length $ T.lines i) == 5) = "style" =: "font-size: 3em; height: auto"

  | (T.length i >= 61) && (T.length i <= 90) && ((L.length $ T.lines i) <= 6) || ((L.length $ T.lines i) == 6) = "style" =: "font-size: 2.5em; height: auto"

  | (T.length i >= 91) && (T.length i <= 120) && ((L.length $ T.lines i) <= 12) || (((L.length $ T.lines i) >= 7) && ((L.length $ T.lines i) <= 10)) = "style" =: "font-size: 2em; height: auto"

  | (T.length i >= 121) && (T.length i <= 140) && ((L.length $ T.lines i) <= 12) || (((L.length $ T.lines i) >= 10) && ((L.length $ T.lines i) <= 13)) = "style" =: "font-size: 1.5em; height: auto"

  | otherwise = "style" =: "font-size: 1em; height: auto"


                               --  Rows         Colour       EditableOrNot
textToInvisible :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textToInvisible rows invisible delta = do
  i <- sample $ current delta
  let class' = fmap textToInvisibleClass invisible
  let rows' = constDyn $ textWidgetRows rows
  let readon = lockText <$> invisible
  let style = constDyn $ styleFunc
  let attrs = mconcat [class',rows', readon, style] -- :: Dynamic t (Map Text Text)
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x -- ::  Event t String
  let value = _textArea_value x -- :: Dynamic t String
  return (value,edits) -- :: (Dynamic t Text, Event t Text)


textWithLockAndClipBoardWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textWithLockAndClipBoardWidget rows editable delta = do
  i <- sample $ current delta
  let class' = fmap textWithLockWidgetClass editable
  let rows' = constDyn $ textWidgetRows rows
  let readon = lockText <$> editable
  let style = constDyn $ styleFunc
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style] -- :: Dynamic t (Map Text Text)
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x -- :: Event t String
  let value = _textArea_value x -- :: Dynamic t String
  butt <- button "to ClipBoard"
  let xx = tag (current $ value) butt
  performEvent $ fmap (liftIO . copyToClipboard) xx
  ----- to clipboard ------
  return (value,edits)


                                     --  Rows     Colour     EditableOrNot
textWithLockWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textWithLockWidget rows editable delta = do
  i <- sample $ current delta
  let class' = fmap textWithLockWidgetClass editable
  let rows' = constDyn $ textWidgetRows rows
  let readon = lockText <$> editable
  let style = constDyn $ styleFunc
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style] -- :: Dyn t (Map Text Text)
  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits) -- :: (Dynamic t Text, Event t Text)

styleFunc:: Map Text Text
styleFunc = "style" =: "height: auto; font-size: 1em;"

lockText:: Bool -> Map Text Text
lockText False = Data.Map.empty
lockText True = "readonly" =: ""

textWithLockWidgetClass :: Bool -> Map Text Text
textWithLockWidgetClass True = "class" =: "human-to-human-comm textInputToEndOfLine code-font"
textWithLockWidgetClass False = "class" =: "primary-color textInputToEndOfLine code-font"

textToInvisibleClass :: Bool -> Map Text Text
textToInvisibleClass True =  "class" =: "invisible-color textInputToEndOfLine code-font"
textToInvisibleClass False = "class" =: "primary-color textInputToEndOfLine code-font"


textNotationParsers :: [TextNotation]
textNotationParsers = [UnspecifiedNotation, Punctual, CineCer0, TimeNot, Seis8s, Hydra] ++ (fmap TidalTextNotation tidalParsers)

holdUniq :: (MonadWidget t m, Eq a) => a -> Event t a -> m (Event t a)
holdUniq i e = holdDyn i e >>= holdUniqDyn >>= return . updated


textProgramEditor :: forall t m. MonadWidget t m => Text -> Int -> Dynamic t (Maybe Text)
  -> Dynamic t (Live TextProgram) -> W t m (Variable t (Live TextProgram))
textProgramEditor style rows errorText deltasDown = divClass "textPatternChain" $ mdo -- *** TODO: change css class
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

  -- GUI elements: language selection menu, eval button, error display, and text area (textWidget)
  (parserEdit,evalButton) <- divClass "fullWidthDiv" $ do
    let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers
    d <- dropdown initialParser parserMap $ ((def :: DropdownConfig t TidalParser) & attributes .~ constDyn ("class" =: "ui-dropdownMenus code-font primary-color primary-borders")) & dropdownConfig_setValue .~ parserDelta
    evalButton' <- divClass "textInputLabel" $ dynButton "\x25B6"
    widgetHold (return ()) $ fmap (maybe (return ()) syntaxErrorWidget) $ updated errorText'
    return (_dropdown_change d,evalButton')
  (_,textEdit,shiftEnter) <- divClass "labelAndTextPattern" $ textWidget' style rows evalFlash initialText textDelta
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

syntaxErrorWidget :: MonadWidget t m => Text -> W t m ()
syntaxErrorWidget t = do
  s <- term Term.Syntax
  let wb = elClass "div" "syntaxIssue" $ dynText s
  tooltip wb (text t)
  return ()
