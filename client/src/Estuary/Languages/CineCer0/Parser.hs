module Estuary.Languages.CineCer0.Parser (cineCer0) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time
import Language.Haskellish
import Data.Bifunctor
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack)

import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Spec
import Estuary.Languages.CineCer0.Signal

type H = Haskellish ()

cineCer0 :: UTCTime -> String -> Either String Spec
cineCer0 eTime x = do
  let sourceAsList = "[" ++ (intercalate "," $ fmap (++ " _0") $ splitOn ";" x) ++ "\n]"
  sourceAsExp <- case parseExp sourceAsList of
    ParseFailed l e -> Left e
    ParseOk e -> Right e
  (theSpec,_) <- runHaskellish (spec eTime) () sourceAsExp
  return theSpec

-- borrowing a trick from Punctual to resolve an issue with comments and newlines...
-- the "redundant" argument _0 applied to something just yields that same thing
_0Arg :: H a -> H a
_0Arg p = p <|> fmap fst (functionApplication p $ reserved "_0")

spec :: UTCTime -> H Spec
spec eTime = do
  os <- fmap (fromList . zip [0..] . catMaybes) $ list maybeLayerSpec
  return $ Spec {
    evalTime = eTime,
    layerSpecMap = os
  }

maybeLayerSpec :: H (Maybe LayerSpec)
maybeLayerSpec = _0Arg $
  (Just <$> layerSpec) <|>
  Nothing <$ reserved "_0"

layerSpec :: H LayerSpec
layerSpec = _0Arg $
  (vs_vs <*> layerSpec) <|>
  fmap videoToLayerSpec text <|>
  (layerSpecFunc <*> text)

layerSpecFunc :: H (Text -> LayerSpec)
layerSpecFunc =
  videoToLayerSpec <$ reserved "video" <|>
  imageToLayerSpec <$ reserved "image" <|>
  textToLayerSpec <$ reserved "text"

-- //////////////

int :: H Int
int = fromIntegral <$> integer

ndt :: H NominalDiffTime
ndt = fromRational <$> rationalOrInteger

text :: H Text
text = pack <$> string

-- //////////////

sigMayRat :: H (Signal (Maybe Rational))
sigMayRat =
  rat_sigMayRat <*> rationalOrInteger <|>
 -- ndt_sigMayRat <*> ndt <|>
  sigRat_sigMayRat <*> sigRat <|>
--  sigMayRat_sigMayRat <*> sigMayRat <|>
  (constantSignal . Just) <$> rationalOrInteger

rat_sigMayRat :: H (Rational -> Signal (Maybe Rational))
rat_sigMayRat = rat_rat_sigMayRat <*> rationalOrInteger

rat_rat_sigMayRat :: H (Rational -> Rational -> Signal (Maybe Rational))
rat_rat_sigMayRat = ndt_rat_rat_sigMayRat <*> ndt

ndt_rat_rat_sigMayRat :: H (NominalDiffTime -> Rational -> Rational -> Signal (Maybe Rational))
ndt_rat_rat_sigMayRat = rampMaybe <$ reserved "ramp"

sigInt :: H (Signal Int)
sigInt = constantSignal <$> int

sigText :: H (Signal Text)
sigText = constantSignal <$> text

sigRat :: H (Signal Rational)
sigRat =
  rat_sigRat <*> rationalOrInteger <|>
 -- ndt_sigRat <*> ndt <|>
  sigRat_sigRat <*> sigRat <|>
  constantSignal <$> rationalOrInteger

rat_sigRat :: H (Rational -> Signal Rational)
rat_sigRat = rat_rat_sigRat <*> rationalOrInteger

rat_rat_sigRat :: H (Rational -> Rational -> Signal Rational)
rat_rat_sigRat = ndt_rat_rat_sigRat <*> ndt

ndt_rat_rat_sigRat :: H (NominalDiffTime -> Rational -> Rational -> Signal Rational)
ndt_rat_rat_sigRat = ramp <$ reserved "ramp"

--list :: Haskellish st a -> Haskellish st [a]

-- rats :: H [Rational]
-- rats = list $ rationalOrInteger

-- ndts :: H [NominalDiffTime]
-- ndts = list $ fromRational <$> rationalOrInteger

-- rats_sigRat :: H ([Rational] -> Signal Rational)
-- rats_sigRat = ndts_rats_sigRat <*> ndts

-- ndts_rats_sigRat :: H ([NominalDiffTime] -> [Rational] -> Signal Rational)
-- ndts_rats_sigRat = ramps <$ reserved "ramps"

---- sine

sigRat_sigRat:: H (Signal Rational -> Signal Rational)
sigRat_sigRat =
  fadeIn <$ reserved "fadeIn" <|>
  fadeOut <$ reserved "fadeOut" <|>
  sine <$ reserved "sin" <|>
  secsToPercen <$ reserved "secs" <|>
  sigRat_sigRat_sigRat <*> sigRat


sigRat_sigRat_sigRat:: H (Signal Rational -> Signal Rational -> Signal Rational)
sigRat_sigRat_sigRat =
  reserved "*" >> return multi <|>
  sigRat_sigRat_sigRat_sigRat <*> sigRat


sigRat_sigRat_sigRat_sigRat:: H (Signal Rational -> Signal Rational -> Signal Rational -> Signal Rational)
sigRat_sigRat_sigRat_sigRat =
  range <$ reserved "range"

--  maybe sine

sigRat_sigMayRat:: H (Signal Rational -> Signal (Maybe Rational))
sigRat_sigMayRat =
  fadeIn2 <$ reserved "fadeIn" <|>
  fadeOut2 <$ reserved "fadeOut" <|>
  sineMaybe <$ reserved "sin" <|>
  sigRat_sigRat_sigMayRat <*> sigRat


sigRat_sigRat_sigMayRat:: H (Signal Rational -> Signal Rational -> Signal (Maybe Rational))
sigRat_sigRat_sigMayRat =
  reserved "*" >> return multi' <|>
  sigRat_sigRat_sigRat_sigMayRat <*> sigRat

sigRat_sigRat_sigRat_sigMayRat:: H (Signal Rational -> Signal Rational -> Signal Rational -> Signal (Maybe Rational))
sigRat_sigRat_sigRat_sigMayRat =
  rangeMaybe <$ reserved "range"


-- //////////////

vs_vs :: H (LayerSpec -> LayerSpec)
vs_vs =
  sigRat_vs_vs <*> sigRat <|>
  sigMayRat_vs_vs <*> sigMayRat <|>
  rat_vs_vs <*> rationalOrInteger <|>
  sigText_vs_vs <*> sigText <|>
  sigInt_vs_vs <*> sigInt <|>
  (reserved "strike" >> return setStrike) <|>
  (reserved "bold" >> return setBold) <|>
  (reserved "italic" >> return setItalic) <|>
  (reserved "border" >> return setBorder) <|>
  (reserved "freeRun" >> return freerun)
 -- <|>
 -- (reserved "mute" >> return setMute) <|>
 -- (reserved "unmute" >> return setUnmute)

sigMayRat_vs_vs :: H (Signal (Maybe Rational) -> LayerSpec -> LayerSpec)
sigMayRat_vs_vs =
  setOpacity <$ reserved "setOpacity" <|>
  shiftOpacity <$ reserved "opacity" <|>
  setBlur <$ reserved "setBlur" <|>
  shiftBlur <$ reserved "blur" <|>
  setBrightness <$ reserved "setBrightness" <|>
  shiftBrightness <$ reserved "brightness" <|>
  setContrast <$ reserved "setContrast" <|>
  shiftContrast <$ reserved "contrast" <|>
  setGrayscale <$ reserved "setGrayscale" <|>
  shiftGrayscale <$ reserved "grayscale" <|>
  setSaturate <$ reserved "setSaturate" <|>
  shiftSaturate <$ reserved "saturate"

sigRat_vs_vs :: H (Signal Rational -> LayerSpec -> LayerSpec)
sigRat_vs_vs =
  setPosX <$ reserved "setPosX" <|>
  shiftPosX <$ reserved "posX" <|>
  setPosY <$ reserved "setPosY" <|>
  shiftPosY <$ reserved "posY" <|>
  setWidth <$ reserved "setWidth" <|>
  shiftWidth <$ reserved "width" <|>
  setHeight <$ reserved "setHeight" <|>
  shiftHeight <$ reserved "height" <|>
  setSize <$ reserved "setSize" <|>
  shiftSize <$ reserved "size" <|>
  setRotate <$ reserved "setRotate" <|>
  shiftRotate <$ reserved "rotate" <|>
  circleMask <$ reserved "circleMask" <|>
  sqrMask <$ reserved "sqrMask" <|>
  setVolume <$ reserved "vol" <|>
  sigRat_sigRat_vs_vs <*> sigRat

sigInt_vs_vs :: H (Signal Int -> LayerSpec -> LayerSpec)
sigInt_vs_vs =
  setZIndex <$ reserved "z"

sigText_vs_vs :: H (Signal Text -> LayerSpec -> LayerSpec)
sigText_vs_vs =
  setColourStr <$ reserved "colour" <|>
  setFontFamily <$ reserved "font"

sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> LayerSpec -> LayerSpec)
sigRat_sigRat_vs_vs =
  setCoord <$ reserved "setCoord" <|>
  shiftCoord <$ reserved "coord" <|>
  playChop' <$ reserved "freeSeg" <|>
  sigRat_sigRat_sigRat_vs_vs <*> sigRat

sigRat_sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> Signal Rational -> LayerSpec -> LayerSpec)
sigRat_sigRat_sigRat_vs_vs =
  circleMask' <$ reserved "circleMask'" <|>
  setRGB <$ reserved "rgb" <|>
  setHSL <$ reserved "hsl" <|>
  setHSL <$ reserved "hsv" <|>
  playChop <$ reserved "seg" <|>
  sigRat_sigRat_sigRat_sigRat_vs_vs <*> sigRat

sigRat_sigRat_sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> Signal Rational -> Signal Rational -> LayerSpec -> LayerSpec)
sigRat_sigRat_sigRat_sigRat_vs_vs =
  rectMask <$ reserved "rectMask" <|>
  setRGBA <$ reserved "rgba" <|>
  setHSLA <$ reserved "hsla" <|>
  setHSLA <$ reserved "hsva"

-- ////

rat_vs_vs :: H (Rational -> LayerSpec -> LayerSpec)
rat_vs_vs =
 playNatural <$ reserved "natural" <|>
 playSnap <$ reserved "snap" <|>
 playSnapMetre <$ reserved "snapMetre" <|>
 playRate <$ reserved "rate" <|>
 rat_rat_vs_vs <*> rationalOrInteger -- <|>
 -- ndt_rat_vs_vs <*> ndt

rat_rat_vs_vs :: H (Rational -> Rational -> LayerSpec -> LayerSpec)
rat_rat_vs_vs =
  playEvery <$ reserved "every" <|>
  quant <$ reserved "quant"
--  rat_rat_rat_vs_vs <*> rationalOrInteger <|>
--  ndt_rat_rat_vs_vs <*> ndt <|>
