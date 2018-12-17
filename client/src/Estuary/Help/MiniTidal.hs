module Estuary.Help.MiniTidal where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

--render multiple sub-help files
miniTidalHelpFile :: MonadWidget t m => m ()
miniTidalHelpFile = divClass "languageHelp" $ do
    about
    palindrome
    brak
    fast
    fit
    return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text  "MiniTidal reference"
  divClass "aboutText" $ text  "A live coding langugae that allows you to make musical patterns with text, describing sequences and ways of transforming and combining them, exploring complex interactions between simple parts."

palindrome :: MonadWidget t m => m ()
palindrome = divClass "helpWrapper" $ do
  switchToReference <- divClass "refExampleButton" $ button "palindrome:"
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  hideableWidget exampleVisible "exampleText" $ text "palindrome $sound \"bd sn kurt\"" --languageHelpWidget MiniTidal
  listenButton <- hideableWidget exampleVisible "listenButton"  $ button "♫"
  hideableWidget referenceVisible "referenceText" $ text "applies rev to a pattern every other cycle, so that the pattern alternates between forwards and backwards." --languageHelpWidget MiniTidal
  return ()

brak :: MonadWidget t m => m ()
brak = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "brak:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "brak $ sound \"[feel feel:3, hc:3 hc:2 hc:4 ho:1]\"" --languageHelpWidget MiniTidal
   listenButton <- hideableWidget exampleVisible "listenButton" $ button "♫"
   hideableWidget referenceVisible "referenceText" $ text "Make a pattern sound a bit like a breakbeat. It does this by every other cycle, squashing the pattern to fit half a cycle, and offsetting it by a quarter of a cycle." --languageHelpWidget MiniTidal
   return ()

   -- help files for functions
fast :: MonadWidget t m => m ()
fast = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "fast:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "sound fast 2 \"bd sn kurt\"" --languageHelpWidget MiniTidal
   listenButton <- hideableWidget exampleVisible "listenButton" $ button "♫"
   hideableWidget referenceVisible "referenceText" $ text "Speed up a pattern. " --languageHelpWidget MiniTidal
   return ()

   -- help files for functions
fit :: MonadWidget t m => m ()
fit = divClass "helpWrapper" $ do
   switchToReference <- divClass "refExampleButton" $ button "fit:"
   exampleVisible <- toggle True switchToReference
   referenceVisible <- toggle False switchToReference
   hideableWidget exampleVisible "exampleText" $ text "sound (fit 3 [\"bd\", \"sn\", \"arpy\", \"arpy:1\", \"casio\"] \"0 [~ 1] 2 1\")" --languageHelpWidget MiniTidal
   listenButton <- hideableWidget exampleVisible "listenButton" $ button "♫"
   hideableWidget referenceVisible "referenceText" $ text "takes a pattern of integer numbers, which are used to select values from the given list. What makes this a bit strange is that only a given number of values are selected each cycle." --languageHelpWidget MiniTidal
   return ()


-- miniTidalReference x = show x
