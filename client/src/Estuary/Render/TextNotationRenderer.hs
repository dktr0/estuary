{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Render.TextNotationRenderer
  (
  TextNotationRenderer(..),
  emptyTextNotationRenderer,
  exoLangToRenderer
  ) where

import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.State.Strict

import Estuary.Types.NoteEvent
import Estuary.Render.R
import qualified Sound.Tidal.Context as Tidal
import GHCJS.Types
import GHCJS.DOM.Types hiding (Text)
import Estuary.Languages.ExoLang
import Estuary.Types.TextNotation as TextNotation
import Estuary.Types.Tempo
import Estuary.Render.R hiding (setTempo)
import Estuary.Render.ForeignTempo
import Estuary.Types.RenderState
import Estuary.Languages.ExoLang


data TextNotationRenderer = TextNotationRenderer {
  parseZone :: Int -> Text -> UTCTime -> R (),
  scheduleNoteEvents :: Int -> R [NoteEvent],
  scheduleWebDirtEvents :: Int -> R [JSVal], -- deprecated/temporary
  clearZone' :: Int -> R (),
  zoneAnimationFrame :: UTCTime -> Int -> R (),
  preAnimationFrame :: R (),
  postAnimationFrame :: R ()
}

emptyTextNotationRenderer :: TextNotationRenderer
emptyTextNotationRenderer = TextNotationRenderer {
  parseZone = \_ _ _ -> return (),
  scheduleNoteEvents = \_ -> return [],
  scheduleWebDirtEvents = \_ -> return [],
  clearZone' = \_ -> return (),
  zoneAnimationFrame = \_ _ -> return (),
  preAnimationFrame = return (),
  postAnimationFrame = return ()
  }

exoLangToRenderer :: ExoLang -> TextNotationRenderer
exoLangToRenderer exolang = emptyTextNotationRenderer {
  parseZone = parseZone' exolang,
  scheduleWebDirtEvents = scheduleWebDirtEvents' exolang,
  clearZone' = clearZone'' exolang,
  preAnimationFrame = preAnimationFrame' exolang,
  zoneAnimationFrame = zoneAnimationFrame' exolang,
  postAnimationFrame = postAnimationFrame' exolang
  }

parseZone' :: ExoLang -> Int -> Text -> UTCTime -> R ()
parseZone' exoLang z txt eTime = do
  r <- liftIO $ evaluate exoLang z txt
  case r of
    Just err -> do
      setBaseNotation z TextNotation.LocoMotion
      setZoneError z err
    Nothing -> do
      setBaseNotation z TextNotation.LocoMotion
      setEvaluationTime z eTime
      clearZoneError z

scheduleWebDirtEvents' :: ExoLang -> Int -> R [JSVal]
scheduleWebDirtEvents' exoLang z = do
  s <- get
  let wStart = renderStart s
  let wEnd = renderEnd s
  liftIO $ render exoLang z wStart wEnd

clearZone'' :: ExoLang -> Int -> R ()
clearZone'' exoLang z = liftIO $ clearZone exoLang z

preAnimationFrame' :: ExoLang -> R ()
preAnimationFrame' exoLang = liftIO $ preAnimate exoLang

zoneAnimationFrame' :: ExoLang -> UTCTime -> Int -> R ()
zoneAnimationFrame' exoLang _ z = do
  s <- get
  liftIO $ setTempo exoLang (tempoCache s)
  liftIO $ animateZone exoLang z

postAnimationFrame' :: ExoLang -> R ()
postAnimationFrame' exoLang = liftIO $ postAnimate exoLang

{-

Thought experiment: how might a complete TextNotationRenderer be provided as an ExoLang (a JS module)?

What is really great about the approach below is that it moves a given language almost entirely out of the
RenderState - all we need to hold on to is a single cached object for the language as a whole, in all cases.

// a constructor that takes no arguments and which will only be called once by Estuary
// ie. to produce and hold on to an object for that language the first time an evaluation in that
// language happens.

export default const SomeExoLang = function () {
}

// a function that is called whenever code is evaluated in a numbered zone
// it receives the numbered zone, the evaluated text, and a standardized 'context' object
// that contains other additional, potentially relevant info/objects
// (for example, context might contain a ForeignTempo, the audio context, a canvas or canvases, etc)
// typically it will either update some properties of the ExoLang object or
// just pass appropriate info to some other embedded object (depending on how the language
// is implemented) AND return an ExoResult (a record containing a success field and an error field)

SomeExoLang.prototype.parseZone = function( text :: String, zone :: Int, evaluationTime :: Object, context :: Object ) {
  // evaluationTime is an object so that it can be provided in multiple formats with some future-proofing
  // not completely sure Int for zone is future-proof though, given plans for sub-rendering of JSoLangs that target multiple languages, etc.
  // a string that can have any level of "sub-versioning" might work, eg. "3.2" means the third sub-zone of top-level zone 3, etc
}

// called once per slow render block if parseZone has ever succeeded with this language in this zone
// returns an array of NoteEvents, ie. JavaScript objects nominally ready for consumption by WebDirt
// Estuary will convert such events to its internal format NoteEvent (and, for example, resolve s + n fields
// to appropriate buffers) and then direct them where they need to go depending on current configuration.
// the 'window' argument contains the rendering window in multiple formats/epochs.
// context contains the same stuff as above in parseZone.
// (if a language does not need to schedule note events, it simply doesn't define this prototype - Estuary
// will not call it if it does not exist)
SomeExoLang.prototype.scheduleNoteEvents = function (zone :: Int, window :: Object, context :: Object ) {
  //  would scheduleNoteEvents ever need to signal an error, and if so, how would it do so? (throw an exception, perhaps)
}


// called to signal that a given zone is no longer occupied by code for this language
// for example, graphical languages might use this to clear out/make transparent a canvas
// an in general, many languages will take this moment to delete/release resources connected to a zone
// (see caveats above though about whether Int is really the right type for indicating a zone)
// no return value. not really sure if the context would ever be necessary, but can't hurt to provide it
// anyway as a simple form of future-proofing.
SomeExoLang.prototype.clearZone = function (zone :: Int, context :: Object) {
}

// called at the beginning of the requestAnimationFrame response if there are
// ANY zones for this language to be rendered
SomeExoLang.prototype.preAnimationFrame = function (context :: Object) {
}

// called for each zone to be rendered during the requestAnimationFrame response
// could return an ExoResult? as a way of passing on, for example, WebGL errors?
// (but we would want to somehow distinguish ephemeral WebGL resource errors that will
// likely resolve by themselves inconsequentially from syntax errors, which could be about
// using the ExoResult type but not doing the same thing with it that we would do with parseZone)
SomeExoLang.prototype.zoneAnimationFrame = function (zone :: Int, context :: Object) {
}

// called after zoneAnimationFrame above has been called for each active zone
SomeExoLang.prototype.postAnimationFrame = function (context :: Object) {
}

-}
