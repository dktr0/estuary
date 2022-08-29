module Estuary.Types.Name where


import Language.Haskellish as LH
import Data.Text as T
import Data.Char
import Control.Monad.Except


type H = Haskellish ()

type Name = Text
type Password = Text

-- names can start with any character, number, or symbol
-- and consist of any character, number, or symbol
-- and must not contain any spaces, tabs, newlines

nameIsLegal :: Text -> Bool
nameIsLegal x = not (T.any isControl x || T.any isSpace x )


nameOrPassword :: H Text
nameOrPassword = do
  x <- textLiteral
  if nameIsLegal x then return x else return x -- else throwError "names/passwords cannot contain spaces or control characters"


-- helper funcs
textLiteral :: H Text
textLiteral = do
  s <- LH.string
  return $ T.pack s
