{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.View.Parser (viewParser,dumpView) where

import Language.Haskellish as LH
import qualified Language.Haskell.Exts as Exts
import qualified Data.Text as T
import Data.List (intercalate)
import Control.Applicative
import TextShow
import Control.Monad.Identity (Identity)
import Control.Monad.Except

import Estuary.Types.View
import Estuary.Types.View.Presets
import Estuary.Types.TextNotation
import Estuary.Types.CodeWidgetOptions
import Estuary.Types.TidalParser

type H = Haskellish ()

dumpView :: View -> T.Text
dumpView EmptyView = "empty"
dumpView (Div css vs) = "div \"" <> css <> "\"" <> dumpViews vs
dumpView (Views vs) = dumpViews vs
dumpView (Columns vs) = "cols " <> dumpViews vs
dumpView (Rows vs) = "rows " <> dumpViews vs
dumpView (Paragraph vs) = "paragraph " <> dumpViews vs
dumpView (BorderDiv vs) = "border " <> dumpViews vs
dumpView (Link url vs) = "link \"" <> url <> "\" " <> dumpViews vs
dumpView (BulletPoints vs) = "bulletpoints " <> dumpViews vs
dumpView (GridView cols rows vs) = "grid " <> showInt cols <> " " <> showInt rows <> " " <> dumpViews vs
dumpView (CollapsableView v) = "collapsable" <> dumpView v
-- dumpView (Text t) = ...
dumpView (LabelView x) = "label " <> showInt x
dumpView (StructureView x) = "structure " <> showInt x
dumpView (CodeView x y vs) = "code " <> showInt x <> " " <> showInt y <> " "
dumpView (SequenceView z) = "sequence " <> showInt z
dumpView (Snippet z b tn txt) = "snippet " <> showInt z <> " " <> (boolToText b) <> " " <> (notationToText tn) <> (" \""<>(formatText txt)<>"\"")
dumpView EnsembleStatusView = "ensembleStatus"
dumpView TempoView = "tempo"
dumpView (RouletteView x rows) = "roulette " <> showInt x <> " " <> showInt rows
dumpView AudioMapView = "audiomap"
dumpView (StopWatchView z) = "stopwatch " <> showInt z
dumpView (CountDownView z) = "countDown " <> showInt z
dumpView (SandClockView z) = "sandClock " <> showInt z
dumpView (SeeTimeView z) = "timeVision " <> showInt z
dumpView (NotePadView z) = "notepad " <> showInt z
dumpView (IFrame url) = "iFrame \"" <> url <> "\""
dumpView (CalendarEventView x) = "calendarEvent " <> showInt x
dumpView (LoadView x) = "load " <> showInt x


dumpView _ = " "

dumpViews :: [View] -> T.Text
dumpViews vs = "[" <> (T.intercalate "," $ fmap dumpView vs) <> "]"

viewParser :: H View
viewParser =  EmptyView <$ reserved "empty" -- localview empty
          <|> divView
          <|> views -- localview [[label 0,code 1 0]]
          <|> columns
          <|> rows
          <|> paragraphParser
          <|> borderParser  -- localview (grid 2 1  [border [label 0,code 1 0], border [label 2,code 3 0]])
          <|> linkView
          <|> bulletpointsParser
          <|> gridViewParser
          <|> collapsableViewParser
          <|> labelParser  -- localview (grid 1 1  [border [label 0,code 1 0]])
          <|> structureParser
          <|> codeViewView  -- localview (grid 1 1 [border [label 0,code 1 0]])
          <|> sequenceParser
          <|> snippetParser
          <|> ensembleStatusView
          <|> tempoView
          <|> rouletteViewView -- localview (grid 2 2 [roulette 0 0,roulette 1 0,roulette 2 0,roulette 3 0])
          <|> audioMapView
          <|> stopwatchParser
          <|> countDownParser
          <|> sandClockParser
          <|> seeTimeParser
          <|> notePadParser
          <|> iFrameParser
          <|> calendarEventParser
          <|> genGridParser
          <|> loadVisionParser

genGridParser :: H View
genGridParser = (genGrid <$ reserved "genGrid") <*> rowsOrColumns <*> rowsOrColumns <*> trueOrFalse

rowsOrColumns :: H Int
rowsOrColumns = do
  x <- integer
  if (x >= 1 && x <= 12) then return (fromIntegral x) else throwError "rows or columns must be between 1 and 12"

--
calendarEventParser :: H View
calendarEventParser = calendarEventParser' <*> int

calendarEventParser' :: H (Int -> View)
calendarEventParser' = calendarEventFunc <$ reserved "calendarevent"

calendarEventFunc :: Int -> View
calendarEventFunc x = CalendarEventView x

--
loadVisionParser :: H View
loadVisionParser = loadVisionParser' <*> int

loadVisionParser' :: H (Int -> View)
loadVisionParser' = loadVisionFunc <$ reserved "load"

loadVisionFunc :: Int -> View
loadVisionFunc x = LoadView $ (x `mod` 3)

-- Snippet z n t

snippetParser:: H View
snippetParser = snippetParser' <*> textLiteral

snippetParser':: H (T.Text -> View)
snippetParser' = snippetParser'' <*> assignTextNotation

snippetParser'':: H (T.Text -> T.Text -> View)
snippetParser'' = snippetParser''' <*> bools

snippetParser''':: H (Bool -> T.Text -> T.Text -> View)
snippetParser''' = snippetParser'''' <*> int

snippetParser'''':: H (Int -> Bool -> T.Text -> T.Text -> View)
snippetParser'''' = snippetViewFunc <$ (reserved "snippet")

snippetViewFunc :: Int -> Bool -> T.Text -> T.Text -> View
snippetViewFunc z b tn sn = Snippet z b (textToNotation' tn) sn


--

seeTimeParser :: H View
seeTimeParser = seeTimeParser' <*> int

seeTimeParser' :: H (Int -> View)
seeTimeParser' = seeTimeFunc <$ reserved "timeVision"

seeTimeFunc :: Int -> View
seeTimeFunc z = SeeTimeView z

--
sandClockParser :: H View
sandClockParser = sandClockParser' <*> int

sandClockParser' :: H (Int -> View)
sandClockParser' = sandClockFunc <$ reserved "sandClock"

sandClockFunc :: Int -> View
sandClockFunc z = SandClockView z

--
countDownParser :: H View
countDownParser = countDownParser' <*> int

countDownParser' :: H (Int -> View)
countDownParser' = countDownFunc <$ reserved "countDown"

countDownFunc :: Int -> View
countDownFunc z = CountDownView z

--
stopwatchParser :: H View
stopwatchParser = stopwatchParser' <*> int

stopwatchParser' :: H (Int -> View)
stopwatchParser' = stopwatchFunc <$ reserved "stopwatch"

stopwatchFunc :: Int -> View
stopwatchFunc z = StopWatchView z

-- NotePad
notePadParser :: H View
notePadParser = notePadParser' <*> int

notePadParser' :: H (Int -> View)
notePadParser' = notePadFunc <$ reserved "notepad"

notePadFunc :: Int -> View
notePadFunc x = NotePadView x

-- Sequencer
sequenceParser :: H View
sequenceParser = sequenceParser' <*> int

sequenceParser' :: H (Int -> View)
sequenceParser' = sequenceFunc <$ reserved "sequence"

sequenceFunc :: Int -> View
sequenceFunc x = SequenceView x

--
structureParser :: H View
structureParser = structureParser' <*> int

structureParser' :: H (Int -> View)
structureParser' = structureFunc <$ reserved "structure"

structureFunc :: Int -> View
structureFunc x = StructureView x

--
labelParser :: H View
labelParser = labelParser' <*> int

labelParser' ::H (Int -> View)
labelParser' = labelFunc <$ reserved "label"

labelFunc :: Int -> View
labelFunc x = LabelView x

--
bulletpointsParser :: H View
bulletpointsParser = bulletpointsParser' <*> viewsParser

bulletpointsParser' :: H ([View] -> View)
bulletpointsParser' = bulletpointsFunc <$ reserved "bulletpoints"

bulletpointsFunc :: [View] -> View
bulletpointsFunc vx = BulletPoints vx

--
borderParser :: H View
borderParser = borderParser' <*> viewsParser

borderParser' :: H ([View] -> View)
borderParser' = borderFunc <$ reserved "border"

borderFunc :: [View] -> View
borderFunc vx = BorderDiv vx

--
paragraphParser :: H View
paragraphParser = paragraphParser' <*> viewsParser

paragraphParser' :: H ([View] -> View)
paragraphParser' = paragraphFunc <$ reserved "paragraph"

paragraphFunc :: [View] -> View
paragraphFunc vx = Paragraph vx

--
views :: H View
views = do
  vs <- viewsParser
  return $ Views vs
--
--
viewsParser :: H [View]
viewsParser = list viewParser

--
rows:: H View
rows = rowParser <*> viewsParser

rowParser:: H ([View] -> View)
rowParser = rowFunc <$ reserved "rows"

rowFunc:: [View] -> View
rowFunc vs = Rows vs

--
columns:: H View
columns = columnParser <*> viewsParser

columnParser:: H ([View] -> View)
columnParser = columnFunc <$ reserved "cols"

columnFunc:: [View] -> View
columnFunc vs = Columns vs

-- GridView
gridViewParser :: H View
gridViewParser =  gridViewParser' <*> viewsParser

gridViewParser' :: H ([View] -> View)
gridViewParser' = gridViewParser'' <*> int

gridViewParser'' :: H (Int -> [View] -> View)
gridViewParser'' = gridViewParser''' <*> int

gridViewParser''' :: H (Int -> Int -> [View] -> View)
gridViewParser''' = gridViewFunc <$ (reserved "grid")

gridViewFunc :: Int -> Int -> [View] -> View
gridViewFunc cols rows vx = GridView cols rows vx


-- CollapsableView
collapsableViewParser :: H View
collapsableViewParser = collapsableViewParser' <*> viewParser

collapsableViewParser' :: H (View -> View)
collapsableViewParser' = collapsableViewFunc <$ reserved "collapsable"

collapsableViewFunc :: View -> View
collapsableViewFunc v = CollapsableView v

-- div View
divView :: H View
divView = divView' <*> viewsParser

divView' :: H ([View] -> View)
divView' = divView''<*> textLiteral

divView'' :: H (T.Text -> [View] -> View)
divView'' = divViewFunc <$ reserved "div"

divViewFunc :: T.Text -> [View] -> View
divViewFunc s vx = Div s vx

-- link View
linkView :: H View
linkView = linkView' <*> viewsParser

linkView' :: H ([View] -> View)
linkView' = linkView'' <*> textLiteral

linkView'' :: H (T.Text -> [View] -> View)
linkView'' = linkViewFunc <$ reserved "link"

linkViewFunc :: T.Text -> [View] -> View
linkViewFunc s vx = Link s vx

--CodeView
codeViewView :: H View
codeViewView =  codeViewView' <*> listOfCodeWidgetOptions

codeViewView' :: H ([CodeWidgetOptions] -> View)
codeViewView' =  codeViewView'' <*> int

codeViewView'' :: H (Int -> [CodeWidgetOptions] -> View)
codeViewView'' =  codeViewView''' <*> int

codeViewView''' :: H (Int -> Int -> [CodeWidgetOptions] -> View)
codeViewView''' = codeViewViewFunc <$ (reserved "code")

codeViewViewFunc :: Int -> Int -> [CodeWidgetOptions] -> View
codeViewViewFunc x y vs = CodeView x y vs


-- Roulette View
rouletteViewView :: H View
rouletteViewView =  rouletteViewView' <*> int

rouletteViewView' :: H (Int -> View)
rouletteViewView' =  rouletteViewView'' <*> int

rouletteViewView'' :: H (Int -> Int -> View)
rouletteViewView'' = rouletteViewViewFunc <$ (reserved "roulette")

rouletteViewViewFunc :: Int -> Int -> View
rouletteViewViewFunc x y = RouletteView x y

--
-- (reserved "ensembleStatus") >> return EnsembleStatusView

ensembleStatusView :: H View
ensembleStatusView = ensembleStatusViewFunc <$ (reserved "ensembleStatus")

ensembleStatusViewFunc :: View
ensembleStatusViewFunc = EnsembleStatusView

--
tempoView :: H View
tempoView = tempoViewFunc <$ (reserved "tempo")

tempoViewFunc :: View
tempoViewFunc = TempoView

--
audioMapView :: H View
audioMapView = audioMapViewFunc <$ (reserved "audiomap")

audioMapViewFunc :: View
audioMapViewFunc = AudioMapView

--

iFrameParser :: H View
iFrameParser = (reserved "iFrame" >> return IFrame) <*> textLiteral


-- helper funcs
int :: H Int
int = fromIntegral <$> integer

bools :: H Bool
bools = trueOrFalse

showInt :: Int -> T.Text
showInt x = showtParen (x < 0) (showt x)

textLiteral :: H T.Text
textLiteral = do
  s <- string
  return $ T.pack s

listOftextLiteral :: H [T.Text]
listOftextLiteral = list textLiteral

identifierText :: H T.Text
identifierText = do
  s <- identifier
  return $ T.pack s


-----

listOfCodeWidgetOptions :: H [CodeWidgetOptions]
listOfCodeWidgetOptions = list stringToCodeWidgetOptions

stringToCodeWidgetOptions :: H CodeWidgetOptions
stringToCodeWidgetOptions =
  (reserved "nomenu" >> return Nomenu) <|>
  (reserved "noeval" >> return Noeval) <|>
  (reserved "noerrors" >> return Noerrors) <|>
  (reserved "fluxus" >> return Fluxus) <|>
  ((reserved "centre" <|> reserved "center") >> return CentreAlign) <|>
  (reserved "right" >> return RightAlign)

codeWidgetOptionsToString :: CodeWidgetOptions -> String
codeWidgetOptionsToString Nomenu = "nomenu"
codeWidgetOptionsToString Noeval = "noeval"
codeWidgetOptionsToString Noerrors = "noerrors"
codeWidgetOptionsToString Fluxus = "fluxus"
codeWidgetOptionsToString CentreAlign = "centre"
codeWidgetOptionsToString RightAlign = "right"

-----

textToNotation :: T.Text -> TextNotation
textToNotation "minitidal" = TidalTextNotation MiniTidal
textToNotation "punctual" = Punctual
textToNotation "cinecer0" = CineCer0
textToNotation "timenot" = TimeNot
textToNotation "seis8s" = Seis8s
textToNotation "hydra" = Hydra
textToNotation x = EphemeralNotation x

textToNotation':: T.Text -> TextNotation ----- this wrapper function checks for JSoLangs. Syntax is: "jsolang myNanoLang" and "ephemeral myNanoLang"
textToNotation' x
        | x == "" = UnspecifiedNotation
        | "jsolang" == (Prelude.head $ T.words x) = JSoLang (T.unwords $ Prelude.tail $ T.words x)
        | otherwise = textToNotation x

notationToText:: TextNotation -> T.Text
notationToText (TidalTextNotation x) = "minitidal"
notationToText Punctual = "punctual"
notationToText CineCer0 = "cinecer0"
notationToText TimeNot = "timenot"
notationToText Seis8s = "seis8s"
notationToText Hydra = "hydra"
notationToText (JSoLang x) = "\"" <> "jsolang " <> x <> "\""
notationToText (EphemeralNotation x) = "\"" <> x <> "\""
notationToText UnspecifiedNotation = ""

formatText:: T.Text -> T.Text
formatText x = T.replace "\"" "\\\"" x

boolToText:: Bool -> T.Text
boolToText True = "True"
boolToText False = "False"

----- parse languages as textNotation
assignTextNotation :: H T.Text
assignTextNotation =
  textLiteral <|>
  "minitidal" <$ reserved "minitidal" <|>
  "punctual" <$ reserved "punctual" <|>
  "cinecer0" <$ reserved "cinecer0" <|>
  "timenot" <$ reserved "timenot" <|>
  "seis8s" <$ reserved "seis8s" <|>
  "hydra" <$ reserved "hydra"
