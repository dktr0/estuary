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
import Data.Time

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

textWidgetClass :: Bool -> Map Text Text
textWidgetClass True = "class" =: "evalFlash textInputToEndOfLine code-font"
textWidgetClass False = "class" =: "primary-color textInputToEndOfLine code-font"

textWidgetRows :: Int -> Map Text Text
textWidgetRows 0 = Data.Map.empty
textWidgetRows x = "rows" =: T.pack (show x)

textWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget rows flash i delta = do
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let style = constDyn $ "style" =: "height: auto"
  let attrs = mconcat [class',rows',style]
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
styleFunc = "style" =: "height: auto; font-size: 2em;"

lockText:: Bool -> Map Text Text
lockText False = Data.Map.empty
lockText True = "readonly" =: ""

textWithLockWidgetClass :: Bool -> Map Text Text
textWithLockWidgetClass True = "class" =: "human-to-human-comm textInputToEndOfLine code-font"
textWithLockWidgetClass False = "class" =: "primary-color textInputToEndOfLine code-font"


textNotationParsers :: [TextNotation]
textNotationParsers = [UnspecifiedNotation, Punctual, CineCer0, TimeNot, Seis8s, Hydra {--Ver, Oir--}] ++ (fmap TidalTextNotation tidalParsers)

holdUniq :: (MonadWidget t m, Eq a) => a -> Event t a -> m (Event t a)
holdUniq i e = holdDyn i e >>= holdUniqDyn >>= return . updated


textProgramEditor :: forall t m. MonadWidget t m => Int -> Dynamic t (Maybe Text)
  -> Dynamic t (Live TextProgram) -> W t m (Variable t (Live TextProgram))
textProgramEditor rows errorText deltasDown = divClass "textPatternChain" $ mdo -- *** TODO: change css class

  -- translate deltasDown into initial value and events that reflect remote changes that will affect local GUI
  i <- sample $ current deltasDown
  let initialParser = (\(x,_,_) -> x) $ forEditing i
  let initialText = (\(_,x,_) -> x) $ forEditing i
  let deltaFuture = fmap forEditing $ updated deltasDown

  let parserDelta = attachWithMaybe (\(x,_,_) (y,_,_) -> if x==y then Nothing else Just y) (fmap forEditing $ current $ currentValue cv) deltaFuture
  let textDelta = attachWithMaybe (\(_,x,_) (_,y,_) -> if x==y then Nothing else Just y) (fmap forEditing $ current $ currentValue cv) deltaFuture
  errorText' <- holdUniqDyn errorText

  -- determine whether we currently display "eval flash" or not
  evalTimeDyn <- holdUniqDyn $ fmap ((\(_,_,x)->x) . forRendering) $ currentValue cv
  let flashOn = True <$ updated evalTimeDyn -- Event t Bool, fires every time evalTime changes
  flashOff <- liftM (False <$) $ delay 0.1 flashOn -- Event t Bool, fires 0.1 seconds later
  evalFlash <- holdDyn False $ leftmost [flashOff,flashOn] -- Dynamic t Bool

  -- GUI elements: language selection menu, eval button, error display, and text area (textWidget)
  (parserEdit,evalButton) <- divClass "fullWidthDiv" $ do
    let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers
    d <- dropdown initialParser parserMap $ ((def :: DropdownConfig t TidalParser) & attributes .~ constDyn ("class" =: "ui-dropdownMenus code-font primary-color primary-borders")) & dropdownConfig_setValue .~ parserDelta
    evalButton' <- divClass "textInputLabel" $ dynButton "\x25B6"
    widgetHold (return ()) $ fmap (maybe (return ()) syntaxErrorWidget) $ updated errorText'
    return (_dropdown_change d,evalButton')
  (_,textEdit,shiftEnter) <- divClass "labelAndTextPattern" $ textWidget rows evalFlash initialText textDelta
  evalEdit <- performEvent $ fmap (liftIO . const getCurrentTime) $ leftmost [evalButton,shiftEnter]

  -- produce a Variable by combining current value of Variable (that already includes deltas down from elsewhere)
  -- with the results of local edits to that variable
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
