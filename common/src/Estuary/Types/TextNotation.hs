{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Estuary.Types.TextNotation where

import GHC.Generics
import Data.Aeson
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Control.Monad.Except

type TextNotation = Text

