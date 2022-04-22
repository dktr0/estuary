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


--I think this widget is only use for the Code Editors but it may be used somewhere else, check this!
textWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
textWidget rows flash i delta = do
  let class' = fmap textWidgetClass flash -- bool marca el flash en cuando evaluas -- creo es :: Dynamic t (Map Text Text)
  let rows' = constDyn $ textWidgetRows rows -- :: Dynamic t (Map Text Text) --- ya esto delimita el height por def de las text_areas
  let style = constDyn $ "style" =: "height: auto" -- creo que también es :: Dynamic t (Map Text Text), y creo que toma la altura del text-area disponible
  let attrs = mconcat [class',rows',style] -- concatena todas las funciones en una -- :: Dynamic t (Map Text Text)

  x <- textArea $ def & textAreaConfig_setValue .~ delta & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i -- i = input text, delta = the last state of that set, attrs: other style elements (height, flash, rows)
  let e = _textArea_element x -- :: HTMLTextAreaElement

  e' <- wrapDomEvent (e) (onEventName Keypress) $ do
    y <- getKeyEvent
    if keyPressWasShiftEnter y then (preventDefault >> return True) else return False -- something about the behaviour of the evaluation, maybe a boolean: if true then change, if false then do nothing
  let evalEvent = fmap (const ()) $ ffilter (==True) e' -- filter just the eval anc changing events
  let edits = _textArea_input x
  let value = _textArea_value x -- these two: grabbing information from the above textArea

  return (value,edits,evalEvent) -- return these set of info
  where keyPressWasShiftEnter ke = (keShift ke == True) && (keKeyCode ke == 13) --something about which keys to press to evaluate code


fluxusTextWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Text -> Event t Text -> m (Dynamic t Text, Event t Text, Event t ())
fluxusTextWidget rows flash i delta = mdo
  let class' = fmap textWidgetClass flash
  let rows' = constDyn $ textWidgetRows rows
  let fluxusStyle = fmap fluxusBehaviour value
  let attrs = mconcat [class',rows', fluxusStyle]
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


-- fluxusBehaviour :: Int -> Map Text Text
-- fluxusBehaviour rows
--   | rows == 1 = "style" =: "font-size: calc(8vw + 8vh)"
--   | rows == 2 = "style" =: "font-size: calc(6vw + 6vh)"
--   | rows == 3 = "style" =: "font-size: calc(4vw + 4vh)"
--   | rows == 4 = "style" =: "font-size: calc(2vw + 2vh)"
--   | otherwise = "style" =: "font-size: calc(2vw + 2vh)"

-- it is just making a pure function from Text -> Int and then applying that to the a Dynamic representation of the current text
-- fluxusBehaviour :: Text -> Map Text Text
-- fluxusBehaviour i
--   | T.length i <= 8 = "style" =: "font-size: calc(8vw + 8vh); height: auto"
--   | (T.length i >= 8) && (T.length i <= 15) = "style" =: "font-size: calc(7vw + 7vh); height: auto"
--   | (T.length i >= 16) && (T.length i <= 20) = "style" =: "font-size: calc(6vw + 6vh); height: auto"
--   | (T.length i >= 21) && (T.length i <= 30) = "style" =: "font-size: calc(5vw + 5vh); height: auto"
--   | (T.length i >= 31) && (T.length i <= 40) = "style" =: "font-size: calc(4vw + 4vh); height: auto"
--   | (T.length i >= 41) && (T.length i <= 60) = "style" =: "font-size: calc(3vw + 3vh); height: auto"
--   | (T.length i >= 61) && (T.length i <= 90) = "style" =: "font-size: calc(2.5vw + 2.5vh); height: auto"
--   | (T.length i >= 91) && (T.length i <= 120) = "style" =: "font-size: calc(2vw + 2vh); height: auto"
--   | otherwise = "style" =: "font-size: calc(1.5vw + 1.5vh); height: auto"

-- fluxusBehaviour :: Text -> Map Text Text
-- fluxusBehaviour i
--      | T.compareLength i 10 == LT = "style" =: "font-size: calc(8vw + 8vh); height: auto"
--      | T.compareLength i 10 == EQ = "style" =: "font-size: calc(4vw + 4vh); height: auto"
--      | T.compareLength i 10 == GT = "style" =: "font-size: calc(2vw + 2vh); height: auto"


-- Data.Text
-- lines :: Text -> [Text]
-- Data.List
-- length :: [a] -> Int
fluxusBehaviour :: Text -> Map Text Text
fluxusBehaviour i
     | (L.length $ T.lines i) == 1 = "style" =: "font-size: calc(8vw + 8vh); height: auto"
     | (L.length $ T.lines i) == 2 = "style" =: "font-size: calc(4vw + 4vh); height: auto"
     | (L.length $ T.lines i) == 3 = "style" =: "font-size: calc(2vw + 2vh); height: auto"
     | otherwise = "style" =: "font-size: calc(1vw + 1vh);"




-- ok, where does textToInvisible is used? look for this at the end... maybe in those parts where text can be hidden--- this code does not have the Text input
                                 --  Rows         Colour       EditableOrNot
textToInvisible :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textToInvisible rows invisible delta = do
  i <- sample $ current delta -- sample function is doing Dynamic t Text -> Text = :: Text  -- saca el estado actual del texto

--  let class' = constDyn $ "class" =: "exp-color textInputToEndOfLine code-font"
  let class' = fmap textToInvisibleClass invisible -- bool class -- creo que esto hace :: Dynamic t (Map Text Text)
  let rows' = constDyn $ textWidgetRows rows -- :: Dynamic t (Map Text Text) -- gives the height of the text_area

  let readon = lockText <$> invisible -- :: Dynamic t (Map Text Text) -- giving the bool to lockText, if True: read-only style is added
  let style = constDyn $ styleFunc -- :: Dynamic t (Map Text Text) --just adding more styling
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style] -- :: Dynamic t (Map Text Text) -- concatena las funciones let anteriores

  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i -- setting text_are with attributes connected to the current state/text and the information from attrs (styles: colour, read-only, height, etc.)
  let edits = _textArea_input x -- ::  Event t String -- tracks edits and then values of the x
  let value = _textArea_value x -- :: Dynamic t String
  return (value,edits) -- :: (Dynamic t Text, Event t Text)
  -- gives back both = resulting in a visible or non-visible read-only text -- same, I think this is used for top/bottom menus/info



--what is this for?
-- same type as textToInvisible function but the Dynamic t Bool is not "editable", it is not connected to the visibility of text
textWithLockAndClipBoardWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textWithLockAndClipBoardWidget rows editable delta = do
  i <- sample $ current delta -- :: Text  ---  current Text state
--  let class' = constDyn $ "class" =: "exp-color textInputToEndOfLine code-font"

  let class' = fmap textWithLockWidgetClass editable -- :: Dynamic t (Map Text Text) -- it gives some styling, if True then add the class human-to-human-comm which I think it is used for anything that is not meant to be eval to produce sound/visuals
  let rows' = constDyn $ textWidgetRows rows -- :: Dynamic t (Map Text Text) -- if True, height of text_area
  let readon = lockText <$> editable -- :: Dynamic t (Map Text Text) -- if true, read-only text
  let style = constDyn $ styleFunc -- :: Dynamic t (Map Text Text) -- setting other styling
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style] -- :: Dynamic t (Map Text Text) -- concatena las functiones anteriores

  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x -- :: Event t String -- something that happens
  let value = _textArea_value x -- :: Dynamic t String -- something that changes

  butt <- button "to ClipBoard" -- ok, adds a button, maybe to the text itself that allows it to be copied, therefore pasted in another place -- button :: Text -> m (Event ())
  let xx = tag (current $ value) butt -- tag :: Behavior a -> Event b -> Event a
  -- current :: Dynamic a -> Behavior a --- ok so el estado del texto actual es un behaviour que se le da un evento (butt) y resulta en otro evento. Específicamente realiza un acción en el valor del texto actual

  performEvent $ fmap (liftIO . copyToClipboard) xx -- performEvent: Use Events to trigger IO actions and to collect their results.
  -- In this case applies the imported fuction copyToClipboard to the Event

  ----- to clipboard ------

  return (value,edits) -- :: (Dynamic t Text, Event t Text)
  -- ok entonces creo que esta función se utiliza en el texto/ejemplos que se pueden copiar, puede ser que en los ejemplos de los tutoriales, lo único que no estoy segura es que si esta misma función también transfiere el texto a la caja de código, creo que no.



--what is this for?
--same type as the other two... it seems also someting related to some coulour styling derived from a Bool.
                                     --  Rows     Colour     EditableOrNot
textWithLockWidget :: MonadWidget t m => Int -> Dynamic t Bool -> Dynamic t Text -> W t m (Dynamic t Text, Event t Text)
textWithLockWidget rows editable delta = do
  i <- sample $ current delta -- Text, probably Map Text Text, or some Behaviour t

--  let class' = constDyn $ "class" =: "exp-color textInputToEndOfLine code-font"
  let class' = fmap textWithLockWidgetClass editable -- :: Dyn t (Map Text Text)
  let rows' = constDyn $ textWidgetRows rows -- :: Dyn t (Map Text Text)
  let readon = lockText <$> editable -- :: Dyn t (Map Text Text)
  let style = constDyn $ styleFunc -- :: Dyn t (Map Text Text)
--  let style = constDyn $ "style" =: ("height: auto; font-size:2em; " <> (temporaryFuncColour <$> colour))
  let attrs = mconcat [class',rows', readon, style] -- :: Dyn t (Map Text Text)
-- so far this section has the same styling: something about the human-to-human communication style, read-only text, setting a number of rows (height), and other styling.

  x <- textArea $ def & textAreaConfig_setValue .~ (updated delta) & textAreaConfig_attributes .~ attrs & textAreaConfig_initialValue .~ i
  let edits = _textArea_input x
  let value = _textArea_value x
  return (value,edits) -- :: (Dynamic t Text, Event t Text)
-- yeah it is absolutly the same function as the last one but without the copyToClipboard event/button. Maybe this is use for evetyhing that has text that is not a code editor. So, it must be use on help, menus, tutorials, etc.

styleFunc:: Map Text Text
styleFunc = "style" =: "height: auto; font-size: 1em;"

lockText:: Bool -> Map Text Text
lockText False = Data.Map.empty
lockText True = "readonly" =: "" -- doesnt let you input any text

textWithLockWidgetClass :: Bool -> Map Text Text
textWithLockWidgetClass True = "class" =: "human-to-human-comm textInputToEndOfLine code-font" -- some styling that visually divides "working-executable code" and just normal "non-executable" communication
textWithLockWidgetClass False = "class" =: "primary-color textInputToEndOfLine code-font"

textToInvisibleClass :: Bool -> Map Text Text
textToInvisibleClass True =  "class" =: "invisible-color textInputToEndOfLine code-font" -- this must affect the opacity of the text to it dissapears.
textToInvisibleClass False = "class" =: "primary-color textInputToEndOfLine code-font"



-- the following is eveything that is connected to the code editors, If I am changing something, it must be here or on the textWidget function above. Note, textWidget is used on the textProgramEditor function bellow.


textNotationParsers :: [TextNotation] -- a list of working notations, avalilable languages in Estuary
textNotationParsers = [UnspecifiedNotation, Punctual, CineCer0, TimeNot, Seis8s, Hydra] ++ (fmap TidalTextNotation tidalParsers)

holdUniq :: (MonadWidget t m, Eq a) => a -> Event t a -> m (Event t a)
holdUniq i e = holdDyn i e >>= holdUniqDyn >>= return . updated


-- so, recibe "rows" = la altura (o el número de rows por def), "errorText" = un texto maybe dinámico que creo es el error (debe de tener algún boolean para identificar cuando la syntax es incorrecta, if true then x), y por último deltasDown = es la primera vez que veo este tipo (Live TextProgram) pero debe de ser al texto dinámico que cambia constantemente al vuelo, resulta en?
textProgramEditor :: forall t m. MonadWidget t m => Int -> Dynamic t (Maybe Text)
  -> Dynamic t (Live TextProgram) -> W t m (Variable t (Live TextProgram))
textProgramEditor rows errorText deltasDown = divClass "textPatternChain" $ mdo -- *** TODO: change css class

  -- translate deltasDown into initial value and events that reflect remote changes that will affect local GUI
  i <- sample $ current deltasDown -- el texto actual -- maybe :: Live TextProgram

  let initialParser = (\(x,_,_) -> x) $ forEditing i
  let initialText = (\(_,x,_) -> x) $ forEditing i
  let deltaFuture = fmap forEditing $ updated deltasDown

  -- attachWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Behavior t a -> Event t b -> Event t c
  --Create a new Event by combining each occurence with the current value of the Behavior. The occurrence is discarded if the combining function returns Nothing
  let parserDelta = attachWithMaybe (\(x,_,_) (y,_,_) -> if x==y then Nothing else Just y) (fmap forEditing $ current $ currentValue cv) deltaFuture
  let textDelta = attachWithMaybe (\(_,x,_) (_,y,_) -> if x==y then Nothing else Just y) (fmap forEditing $ current $ currentValue cv) deltaFuture
  errorText' <- holdUniqDyn errorText

  -- determine whether we currently display "eval flash" or not
  evalTimeDyn <- holdUniqDyn $ fmap ((\(_,_,x)->x) . forRendering) $ currentValue cv
  let flashOn = True <$ updated evalTimeDyn -- Event t Bool, fires every time evalTime changes
  flashOff <- liftM (False <$) $ delay 0.1 flashOn -- Event t Bool, fires 0.1 seconds later
  evalFlash <- holdDyn False $ leftmost [flashOff,flashOn] -- Dynamic t Bool

  -- GUI elements: language selection menu, eval button, error display, and text area (textWidget)
  -- tal vez si modifico sería aquí? o igual nada mas textArea function, entonces ya tendría los cambios aquí.

  -- esta es la primera tupla que contiene el menú de opciones y el butón para evaluar
  (parserEdit,evalButton) <- divClass "fullWidthDiv" $ do

    let parserMap = constDyn $ fromList $ fmap (\x -> (x,T.pack $ textNotationDropDownLabel x)) textNotationParsers
    -- it draws the information of the available languages--- creo que funciona no únicamente para el nombre de las opciones en el dropdown menu pero también para jalar el parser y comparar text que puede o no correr

    d <- dropdown initialParser parserMap $ ((def :: DropdownConfig t TidalParser) & attributes .~ constDyn ("class" =: "ui-dropdownMenus code-font primary-color primary-borders")) & dropdownConfig_setValue .~ parserDelta
    -- creates the dropdown menu with a class (styling) and links this to the actual available information: initialParser parserMal. También setea el valor con el parser actual seleccionado (parseDelta = esto permite que se vea la opciones en múltiples computadora y no únicamente local)
    -- def :: DropdownConfig t TidalParser --- qué está haciendo ésto?, por qué tenemos este tipo aquí?

    evalButton' <- divClass "textInputLabel" $ dynButton "\x25B6"
    -- class/styling of the evaluation button

    -- widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
    -- Given an initial widget and an Event of widget-creating actions, create a widget that is recreated whenever the Event fires.
    widgetHold (return ()) $ fmap (maybe (return ()) syntaxErrorWidget) $ updated errorText'
    -- initial widget = syntaxErrorWidget que debe de identificar? y dar estilo a los syntax errors, éste widget se dispara cuando el evento = updated errorText' aparece. El primero es un maybe, entonces pues no regresa nada si no hay error, y si hay error regresa el mensaje que aparece en la parte superior.

    return (_dropdown_change d,evalButton') -- regresa el lenguaje seleccionado actual, el boyón... interesante que aquí no regresa nada sobre el syntax error message, en nada más algo que pasa pero que no se guarda, la info no se guarda

  --segunda tupla que contiene, mm algo _, no entiendo muy bien esto: (_,_,_), esta lista, dónde está?, pero bueno es básicamente la caja de texto y el evento para evaluar code
  (_,textEdit,shiftEnter) <- divClass "labelAndTextPattern" $ fluxusTextWidget rows evalFlash initialText textDelta --aquí se le da el textWidget que es el que crea la caja de texto con un styling pre-definido y al cual se le da el no. de rows qu crea la altura, el evalFlash que es lo que hace que el background hage un flash (Bool que crea un styling), el texto inicial y el texto actual.

  evalEdit <- performEvent $ fmap (liftIO . const getCurrentTime) $ leftmost [evalButton,shiftEnter] -- evento para evaluar con ambas opciones

  -- produce a Variable by combining current value of Variable (that already includes deltas down from elsewhere)
  -- with the results of local edits to that variable
  let c = current $ currentValue cv
  let parserEdit' = attachWith applyParserEdit c parserEdit
  let textEdit' = attachWith applyTextEdit c textEdit
  let evalEdit' = attachWith applyEvalEdit c evalEdit
  let localEdits = leftmost [parserEdit',textEdit',evalEdit'] -- concatenar los edits
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

-- widget con un styling que contiene un texto dinámico = nombre de los parsers disponibles
labelEditor :: MonadWidget t m => Dynamic t Text -> W t m (Variable t Text)
labelEditor delta = do
  let attrs = constDyn $ ("class" =: "name-tag-textarea code-font primary-color")
  y <- divClass "labelWidgetDiv" $ do
    i <- sample $ current delta
    textInput $ def & textInputConfig_setValue .~ (updated delta) & textInputConfig_attributes .~ attrs & textInputConfig_initialValue .~ i
  returnVariable delta $ _textInput_input y

-- widget para checar y producir syntax error messages
syntaxErrorWidget :: MonadWidget t m => Text -> W t m ()
syntaxErrorWidget t = do
  s <- term Term.Syntax
  let wb = elClass "div" "syntaxIssue" $ dynText s
  tooltip wb (text t)
  return ()
