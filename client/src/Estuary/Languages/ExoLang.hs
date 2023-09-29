{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.ExoLang
  (ExoLang,
  exoLang,
  ExoResult,
  exoResultToMaybe,
  exoResultToEither,
  define,
  clear,
  preRender,
  render,
  postRender,
  setTempo)
  where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad (void,when)
import GHCJS.DOM.Types hiding (Text)
import Data.JSVal.Promise
import Control.Exception.Base (throwIO)

import Estuary.Types.AsyncValue
import Estuary.Types.JSException
import Estuary.Types.Tempo
import Estuary.Render.ForeignTempo
import Estuary.Types.NoteEvent


foreign import javascript safe
  "$r = importExoLang($1);"
  exoLangPromise :: Text -> IO Promise

newtype ExoLangClass = ExoLangClass JSVal

instance PToJSVal ExoLangClass where pToJSVal (ExoLangClass x) = x

instance PFromJSVal ExoLangClass where pFromJSVal = ExoLangClass

foreign import javascript safe
  "$r = $2.exoLang($1);"
  exoLangClass :: HTMLCanvasElement -> JSVal -> IO ExoLangClass

_loadExoLang :: HTMLCanvasElement -> Text -> IO ExoLangClass
_loadExoLang canvas path = do
  p <- exoLangPromise path
  r <- await p
  exoLangModule <- case r of
    Left j -> throwIO (JSException j)
    Right j -> pure j
  putStrLn $ "loaded exolang from " ++ unpack path
  exoLangClass canvas exoLangModule


type ExoLang = AsyncValue ExoLangClass

exoLang :: HTMLCanvasElement -> Text -> IO ExoLang
exoLang canvas path = asyncValue $ _loadExoLang canvas path

withExoLang :: ExoLang -> (ExoLangClass -> IO a) -> IO a
withExoLang x f = blocking x >>= f

foreign import javascript unsafe
  "typeof ($2.$1)"
  _typeOfProperty :: Text -> ExoLangClass -> Text

hasFunction :: Text -> ExoLangClass -> Bool
hasFunction x elc = _typeOfProperty x elc == "Function"


define :: ExoLang -> Int -> Text -> UTCTime -> IO (Either Text Text)
define e z txt eTime = withExoLang e $ \elc -> do
  let eTime' = utcTimeToWhenPOSIX eTime
  case hasFunction "define" elc of
    True -> exoResultToEither <$> _define z txt eTime' elc
    False -> do
      case hasFunction "evaluate" elc of
        True -> exoResultToEither <$> _evaluate z txt eTime' elc
        False -> pure $ Left "ERROR: an exolang (language defined externally to Estuary) lacks defineZone (previously known as evaluate)"

foreign import javascript safe
  "$4.define({zone: $1, text: $2, time: $3})"
  _define :: Int -> Text -> Double -> ExoLangClass -> IO ExoResult

foreign import javascript safe -- DEPRECATED
  "$4.evaluate($1,$2,$3)"
  _evaluate :: Int -> Text -> Double -> ExoLangClass -> IO ExoResult


clear :: ExoLang -> Int -> IO ()
clear e z = withExoLang e $ \elc -> do
  case hasFunction "clear" elc of
    True -> _clear z elc
    False -> do
      case hasFunction "clearZone" elc of
        True -> _clearZone z elc
        False -> pure ()

foreign import javascript safe
  "$2.clear({zone: $1})"
  _clear :: Int -> ExoLangClass -> IO ()

foreign import javascript safe -- DEPRECATED
  "$2.clearZone($1)"
  _clearZone :: Int -> ExoLangClass -> IO ()


preRender :: ExoLang -> Bool -> UTCTime -> IO ()
preRender e canDraw tNow = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  case hasFunction "preRender" elc of 
    True -> _preRender canDraw tNow' elc
    False -> do
      case hasFunction "preAnimate" elc of
        True -> _preAnimate canDraw tNow' elc
        False -> pure ()
        
foreign import javascript unsafe
  "$3.preRender({canDraw: $1, time: $2})"
  _preRender :: Bool -> Double -> ExoLangClass -> IO ()

foreign import javascript unsafe -- DEPRECATED
  "$3.preAnimate($1,$2)"
  _preAnimate :: Bool -> Double -> ExoLangClass -> IO ()
  
  
render :: ExoLang -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
render e tNow wStart wEnd canDraw z = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  let wStart' = utcTimeToWhenPOSIX wStart
  let wEnd' = utcTimeToWhenPOSIX wEnd
  case hasFunction "render" elc of
    True -> _render tNow' wStart' wEnd' canDraw z elc >>= jsValToNoteEvents
    False -> do
      case hasFunction "animateZone" elc of
        True -> _animateZone z canDraw elc >>= jsValToNoteEvents
        False -> pure []

foreign import javascript unsafe
  "$6.render({tNow: $1, wStart: $2, wEnd: $3, canDraw: $4, zone: $5})"
  _render :: Double -> Double -> Double -> Bool -> Int -> ExoLangClass -> IO JSVal
  
foreign import javascript unsafe -- DEPRECATED
  "$3.animateZone($1,$2)"
  _animateZone :: Int -> Bool -> ExoLangClass -> IO JSVal

jsValToNoteEvents :: JSVal -> IO [NoteEvent]
jsValToNoteEvents x = do
  x' <- fromJSVal x
  let jsEvents = case x' of
                   Just ns -> ns
                   Nothing -> []
  pure $ fmap pFromJSVal jsEvents -- TODO: confirm that double time translation is done elsewhere (ie. in RenderEngine)


postRender :: ExoLang -> Bool -> UTCTime -> IO ()
postRender e canDraw tNow = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  case hasFunction "postRender" elc of 
    True -> _postRender canDraw tNow' elc
    False -> do
      case hasFunction "postAnimate" elc of
        True -> _postAnimate canDraw tNow' elc
        False -> pure ()
        
foreign import javascript unsafe
  "$3.postRender({canDraw: $1, time: $2})"
  _postRender :: Bool -> Double -> ExoLangClass -> IO ()

foreign import javascript unsafe -- DEPRECATED
  "$3.postAnimate($1,$2)"
  _postAnimate :: Bool -> Double -> ExoLangClass -> IO ()



setTempo :: ExoLang -> Tempo -> IO ()
setTempo e t = void $ withExoLang e $ \elc -> do
  case hasFunction "setTempo" elc of
    True -> _setTempo (toForeignTempo t) elc
    False -> pure ()
    
foreign import javascript unsafe
  "$2.setTempo($1)"
  _setTempo :: ForeignTempo -> ExoLangClass -> IO ()


newtype ExoResult = ExoResult JSVal

instance PToJSVal ExoResult where pToJSVal (ExoResult x) = x

instance PFromJSVal ExoResult where pFromJSVal = ExoResult

foreign import javascript safe
  "$1.success"
  _exoResultSuccess :: ExoResult -> Bool

foreign import javascript safe
  "$1.error"
  _exoResultError :: ExoResult -> Text
  
foreign import javascript safe
  "$1.info"
  _exoResultInfo :: ExoResult -> Text

exoResultToMaybe :: ExoResult -> Maybe Text
exoResultToMaybe x = case _exoResultSuccess x of
  True -> Nothing
  False -> Just $ _exoResultError x

exoResultToEither :: ExoResult -> Either Text Text
exoResultToEither x = case _exoResultSuccess x of
  True -> Right $ _exoResultInfo x
  False -> Left $ _exoResultError x

utcTimeToWhenPOSIX :: UTCTime -> Double
utcTimeToWhenPOSIX = realToFrac . utcTimeToPOSIXSeconds

