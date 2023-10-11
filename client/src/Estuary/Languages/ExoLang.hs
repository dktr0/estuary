{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.ExoLang
  (ExoLang,
  exoLangRenderer,
  ExoResult,
  exoResultToMaybe,
  exoResultToEither,
  utcTimeToWhenPOSIX,
  jsValToNoteEvents
  ) where

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
import Estuary.Render.Renderer (Renderer)
import qualified Estuary.Render.Renderer as Renderer
import Estuary.Types.TextNotation
import Estuary.Types.Definition


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


preRender :: ExoLang -> Bool -> UTCTime -> UTCTime -> IO ()
preRender e canDraw tNow tPrevDraw = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  let tPrevDraw' = utcTimeToWhenPOSIX tPrevDraw
  case hasFunction "preRender" elc of 
    True -> _preRender canDraw tNow' tPrevDraw' elc
    False -> do
      case hasFunction "preAnimate" elc of
        True -> _preAnimate canDraw tNow' tPrevDraw' elc
        False -> pure ()
        
foreign import javascript unsafe
  "$3.preRender({canDraw: $1, nowTime: $2, previousDrawTime: $3})"
  _preRender :: Bool -> Double -> Double -> ExoLangClass -> IO ()

foreign import javascript unsafe -- DEPRECATED
  "$3.preAnimate($1,$2,$3)"
  _preAnimate :: Bool -> Double -> Double -> ExoLangClass -> IO ()
  
  
render :: ExoLang -> UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
render e tNow tPrevDraw wStart wEnd canDraw z = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  let tPrevDraw' = utcTimeToWhenPOSIX tPrevDraw
  let wStart' = utcTimeToWhenPOSIX wStart
  let wEnd' = utcTimeToWhenPOSIX wEnd
  case hasFunction "render" elc of
    True -> _render tNow' tPrevDraw' wStart' wEnd' canDraw z elc >>= jsValToNoteEvents
    False -> do
      case hasFunction "animateZone" elc of
        True -> _animateZone z canDraw tNow' tPrevDraw' wStart' wEnd' elc >>= jsValToNoteEvents
        False -> pure []

foreign import javascript unsafe
  "$6.render({nowTime: $1, previousDrawTime: $2, windowStartTime: $3, windowEndTime: $4, canDraw: $5, zone: $6})"
  _render :: Double -> Double -> Double -> Double -> Bool -> Int -> ExoLangClass -> IO JSVal
  
foreign import javascript unsafe -- DEPRECATED
  "$3.animateZone($1,$2,$3,$4,$5,$6)"
  _animateZone :: Int -> Bool -> Double -> Double -> Double -> Double -> ExoLangClass -> IO JSVal

jsValToNoteEvents :: JSVal -> IO [NoteEvent]
jsValToNoteEvents x = do
  x' <- fromJSVal x
  let jsEvents = case x' of
                   Just ns -> ns
                   Nothing -> []
  pure $ fmap pFromJSVal jsEvents


postRender :: ExoLang -> Bool -> UTCTime -> UTCTime -> IO ()
postRender e canDraw tNow tPrevDraw = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  let tPrevDraw' = utcTimeToWhenPOSIX tPrevDraw
  case hasFunction "postRender" elc of 
    True -> _postRender canDraw tNow' tPrevDraw' elc
    False -> do
      case hasFunction "postAnimate" elc of
        True -> _postAnimate canDraw tNow' tPrevDraw' elc
        False -> pure ()
        
foreign import javascript unsafe
  "$3.postRender({canDraw: $1, nowTime: $2, previousDrawTime: $3})"
  _postRender :: Bool -> Double -> Double -> ExoLangClass -> IO ()

foreign import javascript unsafe -- DEPRECATED
  "$3.postAnimate($1,$2,$3)"
  _postAnimate :: Bool -> Double -> Double -> ExoLangClass -> IO ()



setTempo :: ExoLang -> Tempo -> IO ()
setTempo e t = void $ withExoLang e $ \elc -> do
  case hasFunction "setTempo" elc of
    True -> _setTempo (toForeignTempo t) elc
    False -> pure ()
    
{- setTempo e t = withExoLang e $ \elo -> do
  putStrLn $ "t " ++ show t
  -- let t' = toForeignTempo t
  t' <- toForeignTempoDebug t
  putStrLn $ "t' " ++ unpack (showForeignTempo t')
  _setTempo t' elo -}

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


exoLangRenderer :: TextNotation -> HTMLCanvasElement -> Text -> IO Renderer
exoLangRenderer tn canvas url = exoLangToRenderer tn <$> exoLang canvas url

exoLangToRenderer :: TextNotation -> ExoLang -> Renderer
exoLangToRenderer tn e = Renderer.emptyRenderer {
  Renderer.define = define' tn e,
  Renderer.clear = clear e,
  Renderer.preRender = preRender e,
  Renderer.render = render e,
  Renderer.postRender = postRender e,
  Renderer.setTempo = setTempo e  
  -- TODO: open pathways between ExoLang and the remaining setters of Renderer
  }

define' :: TextNotation -> ExoLang -> Int -> Definition -> IO (Either Text Text)
define' tn exoLang z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> pure $ Left $ "internal error in Estuary.Render.Renderer: defineZone called for a definition that doesn't pertain to a text program passed to exolang for text notation " <> tn
    Just (_,txt,eTime) -> define exoLang z txt eTime

