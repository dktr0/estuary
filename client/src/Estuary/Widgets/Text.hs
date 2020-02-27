{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom hiding (getKeyEvent,preventDefault)
import Reflex.Dom.Contrib.KeyEvent
import Control.Monad
import Control.Monad.Trans
import GHCJS.DOM.EventM
import Data.Maybe
import Data.Map (fromList)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Estuary.Tidal.Types
import Estuary.WebDirt.Foreign
import Estuary.Reflex.Container
import Estuary.Widgets.GeneralPattern
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Utility (lastOrNothing)
import Estuary.Types.Definition
import Estuary.Types.Hint
import Estuary.Types.TidalParser
import Estuary.Languages.TidalParsers
import Estuary.Types.Live
import Estuary.Types.TextNotation
import Estuary.Help.LanguageHelp
import Estuary.Reflex.Utility
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Widgets.Editor
import Estuary.Types.Context
import Estuary.Types.Variable


textWidget :: MonadWidget t m =>  Int -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget rows i delta = do
  let attrs = case rows of 0 -> constDyn $ ("class" =: "textInputToEndOfLine  primary-color code-font")
                           _ -> constDyn $ ("class" =: "textInputToEndOfLine  primary-color code-font" <> "rows" =: T.pack (show rows) <> "style" =: "height: auto")
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


textNotationParsers :: [TextNotation]
textNotationParsers = [Punctual, CineCer0, TimeNot {--Ver, Oir--}] ++ (fmap TidalTextNotation tidalParsers)

textProgramEditor :: MonadWidget t m => Int -> Dynamic t (Maybe Text) -> Dynamic t (Live TextProgram)
  -> Editor t m (Variable t (Live TextProgram))
textProgramEditor nRows errorDyn updates = do
  ctx <- askContext
  reflexWidgetToEditor updates $ textProgramWidget ctx errorDyn nRows

textProgramWidget :: forall t m. MonadWidget t m => Dynamic t Context -> Dynamic t (Maybe Text) -> Int
  -> Live TextProgram -> Event t (Live TextProgram) -> m (Event t (Live TextProgram),Event t [Hint])
textProgramWidget ctx errorText rows i delta = divClass "textPatternChain" $ do -- *** TODO: change css class

  let deltaFuture = fmap forEditing delta
  let parserFuture = fmap (\(x,_,_) -> x) deltaFuture
  let textFuture = fmap (\(_,x,_) -> x) deltaFuture
  let initialParser = (\(x,_,_) -> x) $ forEditing i
  let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers

  (d,evalButton,infoButton) <- divClass "fullWidthDiv" $ do
    d' <- dropdown initialParser parserMap $ ((def :: DropdownConfig t TidalParser) & attributes .~ constDyn ("class" =: "ui-dropdownMenus code-font primary-color primary-borders")) & dropdownConfig_setValue .~ parserFuture
    evalButton' <- divClass "textInputLabel" $ do
      x <- dynButton =<< translateDyn Term.Eval ctx
      e' <- holdUniqDyn errorText
      let y = fmap (maybe (return ()) (syntaxErrorWidget ctx)) $ updated e'
      widgetHold (return ()) y
      return x
    infoButton' <- divClass "referenceButton" $ tooltip (divClass "" $ dynButton "?") (text "test")
    return (d',evalButton',infoButton')

  (edit,eval) <- divClass "labelAndTextPattern" $ do
    let parserValue = _dropdown_value d -- Dynamic t TidalParser
    let parserEvent = _dropdown_change d
    let initialText = (\(_,x,_) -> x) $ forEditing i
    textVisible <- toggle True infoButton
    helpVisible <- toggle False infoButton
    (textValue,textEvent,shiftEnter) <- hideableWidget textVisible "width-100-percent" $ textWidget rows initialText textFuture
    languageToDisplayHelp <- (holdDyn initialParser $ updated parserValue) >>= holdUniqDyn
    deferredWidget "width-100-percent" helpVisible $ fmap parserToHelp languageToDisplayHelp

    let evalEvent = leftmost [evalButton,shiftEnter]
    let initialEvalTime = (\(_,_,x) -> x) $ forRendering i
    localEvalTime <- performEvent $ fmap (liftIO . const getCurrentTime) evalEvent
    let remoteEvalTime = fmap ( (\(_,_,x) -> x) . forRendering) delta
    evalTimeValue <- holdDyn initialEvalTime $ leftmost [localEvalTime,remoteEvalTime]

    let v' = (\x y z -> (x,y,z)) <$> parserValue <*> textValue <*> evalTimeValue
    let editEvent = tagPromptlyDyn v' $ leftmost [() <$ parserEvent,() <$ textEvent]
    let evalEvent' = tagPromptlyDyn v' $ evalEvent
    return (traceEvent "editEvent" editEvent,traceEvent "evalEvent'" evalEvent')
  let deltaPast = fmap forRendering delta
  pastValue <- holdDyn (forRendering i) $ leftmost [deltaPast,eval]
  futureValue <- holdDyn (forEditing i) $ leftmost [deltaFuture,edit]
  let value = f <$> pastValue <*> futureValue
  let deltaUpEdit = tagPromptlyDyn value edit
  let deltaUpEval = tagPromptlyDyn value eval
  let deltaUp = leftmost [deltaUpEdit,deltaUpEval]
  return (deltaUp,never)
  where
    f p x | p == x = Live p L3 -- *** TODO: this looks like it is a general pattern that should be with Live definitions
          | otherwise = Edited p x


labelEditor :: MonadWidget t m => Dynamic t Text -> Editor t m (Variable t Text)
labelEditor delta = do
  let attrs = constDyn $ ("class" =: "name-tag-textarea code-font primary-color")
  y <- liftR $  divClass "labelWidgetDiv" $ do
    i <- (sample . current) delta
    textInput $ def & textInputConfig_setValue .~ (updated delta) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  return $ Variable (_textInput_value y) (_textInput_input y)

syntaxErrorWidget :: MonadWidget t m => Dynamic t Context -> Text -> m ()
syntaxErrorWidget ctx t = do
  let hoverArea = dynButton =<< translateDyn Term.Syntax ctx
  tooltip hoverArea (text t)
  return ()
