module Estuary.Types.MiniTidalReference where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
-- import Estuary.Types.Language

data MiniTidalReference =
  Palindrome |
  Brak
  deriving (Show,Eq)


miniTidalReference :: MiniTidalReference -> String
-- translate :: Term -> Language -> String

miniTidalReference Palindrome = "applies rev to a pattern every other cycle, so that the pattern alternates between forwards and backwards."
miniTidalReference Brak = "Make a pattern sound a bit like a breakbeat. It does this by every other cycle, squashing the pattern to fit half a cycle, and offsetting it by a quarter of a cycle."

miniTidalReference x = show x
