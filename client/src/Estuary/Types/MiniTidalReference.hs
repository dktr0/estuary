module Estuary.Types.MiniTidalReference where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic

-- import Estuary.Types.Language

--render multiple sub-help files
miniTidalHelpFile :: MonadWidget t m => m ()
miniTidalHelpFile = divClass "languageHelp" $ do
    about
    -- palindrome
    -- brak
    -- fast
    -- fit
    return ()

-- about

about :: MonadWidget t m => m ()
about = do
  divClass "about" $ text  "MiniTidal reference"
  divClass "aboutText" $ text  "A live coding langugae that allows you to make musical patterns with text, describing sequences and ways of transforming and combining them, exploring complex interactions between simple parts."
-- a list of MiniTidal samples

-- samples :: MonadWidget t m => m ()
-- samples = divClass "helpWrapper" $ do
--    switchToReference <- divClass "refExampleButton" $ button "samples:"
--    exampleVisible <- toggle True switchToReference
--    referenceVisible <- toggle False switchToReference
--    hideableWidget exampleVisible "exampleText" $ divClass "samples" $ text "flick sid can metal future gabba sn mouth co gretsch mt arp h cp cr newnotes bass crow hc tabla bass0 hh bass1 bass2 oc bass3 ho odx diphone2 house off ht tink perc bd industrial pluck trump printshort jazz voodoo birds3 procshort blip drum jvbass psr wobble drumtraks koy rave bottle kurt latibro rm sax lighter lt arpy feel less stab ul" --languageHelpWidget MiniTidal
--    hideableWidget referenceVisible "referenceText" $ text "Each one is a folder containing one or more wav files. For example when you put bd:1 in a sequence, you’re picking up the second wav file in the bd folder. If you ask for the ninth sample and there are only seven in the folder, it’ll wrap around and play the second one." --languageHelpWidget MiniTidal
--    return ()


palindrome :: MonadWidget t m => m ()
palindrome = divClass "helpWrapper" $ do
  switchToReference <- divClass "refExampleButton" $ button "palindrome:"
  exampleVisible <- toggle True switchToReference
  referenceVisible <- toggle False switchToReference
  listenButton <- hideableWidget exampleVisible "listenButton"  $ button "♫"
  hideableWidget referenceVisible "referenceText" $ text "applies rev to a pattern every other cycle, so that the pattern alternates between forwards and backwards." --languageHelpWidget MiniTidal
  return ()

-- help files for functions

   -- help files for functions
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
   hideableWidget exampleVisible "exampleText" $ text "sound (fast 2 \"bd sn kurt\")" --languageHelpWidget MiniTidal
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


-- miniTidalReference Brak = "brak: " ++ "Make a pattern sound a bit like a breakbeat. It does this by every other cycle, squashing the pattern to fit half a cycle, and offsetting it by a quarter of a cycle." ++ "\n"

-- miniTidalReference x = show x
