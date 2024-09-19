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
import GHCJS.Types (isUndefined)
import GHCJS.DOM.Types hiding (Text)
import GHCJS.Foreign.Callback
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
  elc <- exoLangClass canvas exoLangModule
  putStrLn $ " hasFunction define: " ++ show (hasFunction "define" elc)
  putStrLn $ " hasFunction evaluate: " ++ show (hasFunction "evaluate" elc)
  putStrLn $ " hasFunction clear: " ++ show (hasFunction "clear" elc)
  putStrLn $ " hasFunction clearZone: " ++ show (hasFunction "clearZone" elc)
  putStrLn $ " hasFunction preRender: " ++ show (hasFunction "preRender" elc)
  putStrLn $ " hasFunction preAnimate: " ++ show (hasFunction "preAnimate" elc)
  putStrLn $ " hasFunction render: " ++ show (hasFunction "render" elc)
  putStrLn $ " hasFunction animateZone: " ++ show (hasFunction "animateZone" elc)
  putStrLn $ " hasFunction postRender: " ++ show (hasFunction "postRender" elc)
  putStrLn $ " hasFunction postAnimate: " ++ show (hasFunction "postAnimate" elc)
  putStrLn $ " hasFunction setTempo: " ++ show (hasFunction "setTempo" elc)
  putStrLn $ " hasFunction setBrightness: " ++ show (hasFunction "setBrightness" elc)
  putStrLn $ " hasFunction setOutputChannelCount: " ++ show (hasFunction "setOutputChannelCount" elc)
  pure elc  
  
  
type ExoLang = AsyncValue ExoLangClass

exoLang :: HTMLCanvasElement -> Text -> IO ExoLang
exoLang canvas path = asyncValue $ _loadExoLang canvas path

withExoLang :: ExoLang -> (ExoLangClass -> IO a) -> IO a
withExoLang x f = blocking x >>= f

foreign import javascript unsafe
  "typeof ($2[$1])"
  _typeOfProperty :: Text -> ExoLangClass -> Text

hasFunction :: Text -> ExoLangClass -> Bool
hasFunction x elc = _typeOfProperty x elc == "function"

define :: FromJSVal j1 => FromJSVal j2 => (j1 -> IO ()) -> (j2 -> IO ()) -> ExoLang -> Int -> Text -> UTCTime -> IO ()
define okCb errorCb e z txt eTime = withExoLang e $ \elc -> do
  let eTime' = utcTimeToWhenPOSIX eTime
  p <- case hasFunction "define" elc of
    True -> _define z txt eTime' elc
    False -> do
      case hasFunction "evaluate" elc of
        True -> _evaluate z txt eTime' elc
        False -> _noDefineError
  p' <- setDefineOkayCallback p (\x -> fromJSValUnchecked x >>= okCb) 
  setDefineErrorCallback p' (\e -> fromJSValUnchecked e >>= errorCb)
  pure ()

foreign import javascript safe
  "exolangDefinePromise($4.define({zone: $1, text: $2, time: $3}))"
  _define :: Int -> Text -> Double -> ExoLangClass -> IO Promise

foreign import javascript safe -- DEPRECATED
  "exolangDefinePromise($4.evaluate($1,$2,$3))"
  _evaluate :: Int -> Text -> Double -> ExoLangClass -> IO Promise

foreign import javascript unsafe
  "new Promise(function(resolve,reject) { reject(new Error('ERROR: an exolang (language defined externally to Estuary) lacks define'))})"
  _noDefineError :: IO Promise

foreign import javascript safe
  "$1.then(function(x) {$2(x)})"
  _setDefineOkayCallback :: Promise -> Callback (JSVal -> IO ()) -> IO Promise

setDefineOkayCallback :: Promise -> (JSVal -> IO ()) -> IO Promise
setDefineOkayCallback p cb = do
  cb' <- asyncCallback1 cb
  _setDefineOkayCallback p cb'

foreign import javascript safe
  "$1['catch'](function (e) {$2(e.toString())})"
  _setDefineErrorCallback :: Promise -> Callback (JSVal -> IO ()) -> IO Promise

setDefineErrorCallback :: Promise -> (JSVal -> IO ()) -> IO Promise
setDefineErrorCallback p cb = do
  cb' <- asyncCallback1 cb
  _setDefineErrorCallback p cb'
  

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
  "$4.preRender({canDraw: $1, nowTime: $2, previousDrawTime: $3})"
  _preRender :: Bool -> Double -> Double -> ExoLangClass -> IO ()

foreign import javascript unsafe -- DEPRECATED
  "$4.preAnimate($1,$2,$3)"
  _preAnimate :: Bool -> Double -> Double -> ExoLangClass -> IO ()
  
  
render :: ExoLang -> UTCTime -> UTCTime -> UTCTime -> UTCTime -> Bool -> Int -> IO [NoteEvent]
render e tNow tPrevDraw wStart wEnd canDraw z = withExoLang e $ \elc -> do
  let tNow' = utcTimeToWhenPOSIX tNow
  let tPrevDraw' = utcTimeToWhenPOSIX tPrevDraw
  let wStart' = utcTimeToWhenPOSIX wStart
  let wEnd' = utcTimeToWhenPOSIX wEnd
  ns <- case hasFunction "render" elc of
    True -> _render tNow' tPrevDraw' wStart' wEnd' canDraw z elc >>= jsValToNoteEvents
    False -> do
      case hasFunction "animateZone" elc of
        True -> _animateZone z canDraw tNow' tPrevDraw' wStart' wEnd' elc >>= jsValToNoteEvents
        False -> pure []
  let rp = diffUTCTime wEnd wStart
  case rp of -- if render period was 0, discard any events that an exolang (nonsensically) calculated
    0 -> pure []
    _ -> pure ns  

foreign import javascript unsafe
  "$7.render({nowTime: $1, previousDrawTime: $2, windowStartTime: $3, windowEndTime: $4, canDraw: $5, zone: $6})"
  _render :: Double -> Double -> Double -> Double -> Bool -> Int -> ExoLangClass -> IO JSVal
  
foreign import javascript unsafe -- DEPRECATED
  "$7.animateZone($1,$2,$3,$4,$5,$6)"
  _animateZone :: Int -> Bool -> Double -> Double -> Double -> Double -> ExoLangClass -> IO JSVal

jsValToNoteEvents :: JSVal -> IO [NoteEvent]
jsValToNoteEvents x = do
  case isUndefined x of
    True -> pure []
    False -> do
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
  "$4.postRender({canDraw: $1, nowTime: $2, previousDrawTime: $3})"
  _postRender :: Bool -> Double -> Double -> ExoLangClass -> IO ()

foreign import javascript unsafe -- DEPRECATED
  "$4.postAnimate($1,$2,$3)"
  _postAnimate :: Bool -> Double -> Double -> ExoLangClass -> IO ()



setTempo :: ExoLang -> Tempo -> IO ()
setTempo e t = void $ withExoLang e $ \elc -> when (hasFunction "setTempo" elc) $ _setTempo (toForeignTempo t) elc

foreign import javascript unsafe
  "$2.setTempo($1)"
  _setTempo :: ForeignTempo -> ExoLangClass -> IO ()


setBrightness :: ExoLang -> Double -> IO ()
setBrightness e x = void $ withExoLang e $ \elc -> when (hasFunction "setBrightness" elc) $ _setBrightness x elc

foreign import javascript safe
  "$2.setBrightness($1)"
  _setBrightness :: Double -> ExoLangClass -> IO ()


setOutputChannelCount :: ExoLang -> Int -> IO ()
setOutputChannelCount e x = void $ withExoLang e $ \elc -> when (hasFunction "setOutputChannelCount" elc) $ _setOutputChannelCount x elc

foreign import javascript safe
  "$2.setOutputChannelCount($1)"
  _setOutputChannelCount :: Int -> ExoLangClass -> IO ()


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
  Renderer.setTempo = setTempo e,
  Renderer.setBrightness = setBrightness e,
  Renderer.setNchnls = setOutputChannelCount e
  -- TODO: open pathways between ExoLang and the remaining setters of Renderer
  }

define' :: TextNotation -> ExoLang -> (Int -> Text -> IO ()) -> (Int -> Text -> IO ()) -> Int -> Definition -> IO ()
define' tn exoLang okCb errorCb z d = do
  case definitionToRenderingTextProgram d of 
    Nothing -> errorCb z $ "internal error in Estuary.Render.Renderer: defineZone called for a definition that doesn't pertain to a text program passed to exolang for text notation " <> tn
    Just (_,txt,eTime) -> define (okCb z) (errorCb z) exoLang z txt eTime

