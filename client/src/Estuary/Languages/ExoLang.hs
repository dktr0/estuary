{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Languages.ExoLang
  (ExoLang,exoLang,ExoResult,exoResultToErrorText,utcTimeToWhenPOSIX,
  evaluate,render,clearZone,preAnimate,animateZone,postAnimate,setTempo) where

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


-- Nothing values represent successful evaluation
evaluate :: ExoLang -> Int -> Text -> IO (Maybe Text)
evaluate e z txt = do
  exoResult <- withExoLang e $ _evaluate z txt
  case exoResultToErrorText exoResult of
    Just err -> pure (Just err)
    Nothing -> pure Nothing

foreign import javascript safe
  "$3.evaluate($1,$2)"
  _evaluate :: Int -> Text -> ExoLangClass -> IO ExoResult


render :: ExoLang -> Int -> UTCTime -> UTCTime -> IO [JSVal]
render e z wStart wEnd = do
  let wStart' = utcTimeToWhenPOSIX wStart
  let wEnd' = utcTimeToWhenPOSIX wEnd
  jsVal <- withExoLang e $ \elc -> if hasRender elc then (_render z wStart' wEnd' elc) else emptyList
  jsVal' <- fromJSVal jsVal
  case jsVal' of
    Just ns -> pure ns
    Nothing -> pure []

foreign import javascript unsafe
  "[]"
  emptyList :: IO JSVal

foreign import javascript unsafe
  "typeof ($2.$1)"
  _typeOfProperty :: Text -> ExoLangClass -> Text

hasRender :: ExoLangClass -> Bool
hasRender elo = _typeOfProperty "render" elo == "Function"

foreign import javascript safe
  "$4.render($1,$2,$3)"
  _render :: Int -> Double -> Double -> ExoLangClass -> IO JSVal


clearZone :: ExoLang -> Int -> IO ()
clearZone e z = void $ withExoLang e $ _clearZone z

foreign import javascript safe
  "$2.clearZone($1)"
  _clearZone :: Int -> ExoLangClass -> IO ()


preAnimate :: ExoLang -> IO ()
preAnimate e = void $ withExoLang e _preAnimate

foreign import javascript unsafe
  "$1.preAnimate()"
  _preAnimate :: ExoLangClass -> IO ()


animateZone :: ExoLang -> Int -> IO ()
animateZone e z = void $ withExoLang e $ _animateZone z

foreign import javascript unsafe
  "$2.animateZone($1)"
  _animateZone :: Int -> ExoLangClass -> IO ()


postAnimate :: ExoLang -> IO ()
postAnimate e = void $ withExoLang e _postAnimate

foreign import javascript unsafe
  "$1.postAnimate()"
  _postAnimate :: ExoLangClass -> IO ()


setTempo :: ExoLang -> Tempo -> IO ()
setTempo e t = void $ withExoLang e $ _setTempo $ toForeignTempo t
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

exoResultToErrorText :: ExoResult -> Maybe Text
exoResultToErrorText x = case _exoResultSuccess x of
  True -> Nothing
  False -> Just $ _exoResultError x


utcTimeToWhenPOSIX :: UTCTime -> Double
utcTimeToWhenPOSIX = realToFrac . utcTimeToPOSIXSeconds

