module Estuary.Languages.TiempoEspacio.Escuchar (escuchar, EscucharSpec) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time

import Estuary.Languages.ExpParser
import Estuary.Languages.CineCer0.VideoSpec

type EscucharSpec = IntMap VideoSpec

escuchar :: String -> Either String EscucharSpec
escuchar s = (f . parseExp) $ ( "do {" ++ s ++ "}" )
  where
    f (ParseOk x) = runExpParser escucharSpec x
    f (ParseFailed l s) = Left s

escucharSpec :: ExpParser EscucharSpec
escucharSpec = fmap (fromList . zip [0..]) $ listOfDoStatements sentence

sentence :: ExpParser VideoSpec
sentence = literalVideoSpec

literalVideoSpec :: ExpParser VideoSpec
literalVideoSpec =
  fmap stringToVideoSpec string <|>
  fmap emptyVideoSpec string
