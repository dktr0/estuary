import Language.Haskell.Interpreter
import Sound.Tidal.Context

imports = ["Prelude","Sound.Tidal.Context","Sound.OSC.Type","Sound.OSC.Datum","Data.Map"]

simple x = runInterpreter $ do
  setImports imports
  interpret x (as::OscPattern)
