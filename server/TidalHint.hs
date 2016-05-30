module TidalHint where

import Sound.Tidal.Context

import Language.Haskell.Interpreter as Hint

hintParamPattern  :: String -> IO (Either InterpreterError ParamPattern)
hintParamPattern x = Hint.runInterpreter $ do
  Hint.set [languageExtensions := [OverloadedStrings]]
  Hint.setImports ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]
  Hint.interpret x (Hint.as::ParamPattern)
