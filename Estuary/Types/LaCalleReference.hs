module Estuary.Types.LaCalleReference where

import Reflex
import Reflex.Dom
import GHCJS.DOM.EventM
-- import Estuary.Types.Language

data LaCalleReference =
  HolaChoche |
  UnasChelas
  deriving (Show,Eq)


laCalleReference :: LaCalleReference -> String
-- translate :: Term -> Language -> String

laCalleReference HolaChoche =  "hola choche: " ++ "returns the Tidal sample 'sitar'" ++ "\n"
laCalleReference UnasChelas =  "unas chelas: " ++ "returns the Tidal sample 'ifdrums'" ++ "\n"

laCalleReference x = show x
