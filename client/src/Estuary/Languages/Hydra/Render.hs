{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.Hydra.Render (Hydra(..),newHydra,setResolution,evaluate,tick) where

import GHCJS.Types
import GHCJS.DOM.Types (HTMLCanvasElement)
import GHCJS.Marshal.Pure

import Estuary.Languages.Hydra.Types

newtype Hydra = Hydra JSVal
instance PToJSVal Hydra where pToJSVal (Hydra x) = x
instance PFromJSVal Hydra where pFromJSVal = Hydra

foreign import javascript safe
  "new Hydra({ canvas:$1, autoLoop:false, makeGlobal:false})"
  newHydra :: HTMLCanvasElement -> IO Hydra

foreign import javascript safe
  "$1.setResolution($2,$3);"
  setResolution :: Hydra -> Int -> Int -> IO ()

evaluate :: Hydra -> [Statement] -> IO ()
evaluate h = mapM_ (evaluateStatement h)

foreign import javascript safe
  "$1.tick($2);"
  tick :: Hydra -> Double -> IO () -- second argument is time in millis since last tick

newtype JSParameters = JSParameters JSVal
instance PToJSVal JSParameters where pToJSVal (JSParameters x) = x
instance PFromJSVal JSParameters where pFromJSVal = JSParameters

parametersToJS :: Parameters -> JSParameters
parametersToJS (Parameters []) = _list0
parametersToJS (Parameters (x:[])) = _list1 x
parametersToJS (Parameters (x:y:[])) = _list2 x y
parametersToJS (Parameters (x:y:z:_)) = _list3 x y z
parametersToJS (Fast [] xs) = _fast1 (parametersToJS xs) -- "$1.fast()"
parametersToJS (Fast (x:_) xs) = _fast2 (parametersToJS xs) (pToJSVal x) -- "$1.fast($2)"
parametersToJS (Smooth [] xs) = _smooth1 (parametersToJS xs)
parametersToJS (Smooth (x:_) xs) = _smooth2 (parametersToJS xs) (pToJSVal x)


foreign import javascript safe "[]" _list0 :: JSParameters
foreign import javascript safe "[$1]" _list1 :: Double -> JSParameters
foreign import javascript safe "[$1,$2]" _list2 :: Double -> Double -> JSParameters
foreign import javascript safe "[$1,$2,$3]" _list3 :: Double -> Double -> Double -> JSParameters
foreign import javascript safe "$1.fast()" _fast1 :: JSParameters -> JSParameters
foreign import javascript safe "$1.fast($2)" _fast2 :: JSParameters -> JSVal -> JSParameters
foreign import javascript safe "$1.smooth()" _smooth1 :: JSParameters -> JSParameters
foreign import javascript safe "$1.smooth($2)" _smooth2 :: JSParameters -> JSVal -> JSParameters


newtype JSSource = JSSource JSVal
instance PToJSVal JSSource where pToJSVal (JSSource x) = x
instance PFromJSVal JSSource where pFromJSVal = JSSource


sourceToJS :: Hydra -> Source -> JSSource
sourceToJS h (OutputAsSource x) = outputToJSSource h x
sourceToJS h (Osc []) = _osc0 h
sourceToJS h (Osc (x:[])) = _osc1 h (parametersToJS x)
sourceToJS h (Osc (x:y:[])) = _osc2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Osc (x:y:z:_)) = _osc3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Solid []) = _solid0 h
sourceToJS h (Solid (x:[])) = _solid1 h (parametersToJS x)
sourceToJS h (Solid (x:y:[])) = _solid2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Solid (x:y:z:[])) = _solid3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Solid (w:x:y:z:_)) = _solid4 h (parametersToJS w) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Gradient []) = _gradient0 h
sourceToJS h (Gradient (x:_)) = _gradient1 h (parametersToJS x)
sourceToJS h (Noise []) = _noise0 h
sourceToJS h (Noise (x:[])) = _noise1 h (parametersToJS x)
sourceToJS h (Noise (x:y:_)) = _noise2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Shape []) = _shape0 h
sourceToJS h (Shape (x:[])) = _shape1 h (parametersToJS x)
sourceToJS h (Shape (x:y:[])) = _shape2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Shape (x:y:z:_)) = _shape3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Voronoi []) = _voronoi0 h
sourceToJS h (Voronoi (x:[])) = _voronoi1 h (parametersToJS x)
sourceToJS h (Voronoi (x:y:[])) = _voronoi2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Voronoi (x:y:z:_)) = _voronoi3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Brightness [] s) = _brightness0 (sourceToJS h s)
sourceToJS h (Brightness (x:_) s) = _brightness1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Contrast [] s) = _contrast0 (sourceToJS h s)
sourceToJS h (Contrast (x:_) s) = _contrast1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Colorama [] s) = _colorama0 (sourceToJS h s)
sourceToJS h (Colorama (x:_) s) = _colorama1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Color [] s) = _color0  (sourceToJS h s)
sourceToJS h (Color (x:[]) s) = _color1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Color (x:y:[]) s) = _color2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Color (x:y:z:[]) s) = _color3 (sourceToJS h s) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Color (w:x:y:z:_) s) = _color4 (sourceToJS h s) (parametersToJS w) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Invert [] s) = _invert0 (sourceToJS h s)
sourceToJS h (Invert (x:_) s) = _invert1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Luma [] s) = _luma0 (sourceToJS h s)
sourceToJS h (Luma (x:[]) s) = _luma1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Luma (x:y:_) s) = _luma2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Posterize [] s) = _posterize0 (sourceToJS h s)
sourceToJS h (Posterize (x:[]) s) = _posterize1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Posterize (x:y:_) s) = _posterize2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Saturate [] s) = _saturate0 (sourceToJS h s)
sourceToJS h (Saturate (x:_) s) = _saturate1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Shift [] s) = _shift0  (sourceToJS h s)
sourceToJS h (Shift (x:[]) s) = _shift1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Shift (x:y:[]) s) = _shift2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Shift (x:y:z:[]) s) = _shift3 (sourceToJS h s) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Shift (w:x:y:z:_) s) = _shift4 (sourceToJS h s) (parametersToJS w) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Thresh [] s) = _thresh0 (sourceToJS h s)
sourceToJS h (Thresh (x:[]) s) = _thresh1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Thresh (x:y:_) s) = _thresh2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Kaleid [] s) = _kaleid0 (sourceToJS h s)
sourceToJS h (Kaleid (x:_) s) = _kaleid1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Pixelate [] s) = _pixelate0 (sourceToJS h s)
sourceToJS h (Pixelate (x:[]) s) = _pixelate1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Pixelate (x:y:_) s) = _pixelate2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Repeat [] s) = _repeat0  (sourceToJS h s)
sourceToJS h (Repeat (x:[]) s) = _repeat1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Repeat (x:y:[]) s) = _repeat2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Repeat (x:y:z:[]) s) = _repeat3 (sourceToJS h s) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Repeat (w:x:y:z:_) s) = _repeat4 (sourceToJS h s) (parametersToJS w) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (RepeatX [] s) = _repeatX0 (sourceToJS h s)
sourceToJS h (RepeatX (x:[]) s) = _repeatX1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (RepeatX (x:y:_) s) = _repeatX2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (RepeatY [] s) = _repeatY0 (sourceToJS h s)
sourceToJS h (RepeatY (x:[]) s) = _repeatY1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (RepeatY (x:y:_) s) = _repeatY2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Rotate [] s) = _rotate0 (sourceToJS h s)
sourceToJS h (Rotate (x:[]) s) = _rotate1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Rotate (x:y:_) s) = _rotate2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Scale [] s) = _scale0  (sourceToJS h s)
sourceToJS h (Scale (x:[]) s) = _scale1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Scale (x:y:[]) s) = _scale2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Scale (x:y:z:[]) s) = _scale3 (sourceToJS h s) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Scroll [] s) = _scroll0  (sourceToJS h s)
sourceToJS h (Scroll (x:[]) s) = _scroll1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Scroll (x:y:[]) s) = _scroll2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Scroll (x:y:z:[]) s) = _scroll3 (sourceToJS h s) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Scroll (w:x:y:z:_) s) = _scroll4 (sourceToJS h s) (parametersToJS w) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (ScrollX [] s) = _scrollX0 (sourceToJS h s)
sourceToJS h (ScrollX (x:[]) s) = _scrollX1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (ScrollX (x:y:_) s) = _scrollX2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (ScrollY [] s) = _scrollY0 (sourceToJS h s)
sourceToJS h (ScrollY (x:[]) s) = _scrollY1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (ScrollY (x:y:_) s) = _scrollY2 (sourceToJS h s) (parametersToJS x) (parametersToJS y)
sourceToJS h (Modulate y [] x) = _modulate0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Modulate y (a:_) x) = _modulate1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateHue y [] x) = _modulateHue0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateHue y (a:_) x) = _modulateHue1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateKaleid y [] x) = _modulateKaleid0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateKaleid y (a:_) x) = _modulateKaleid1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulatePixelate y [] x) = _modulatePixelate0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulatePixelate y (a:[]) x) = _modulatePixelate1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulatePixelate y (a:b:_) x) = _modulatePixelate2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateRepeat y [] x) = _modulateRepeat0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateRepeat y (a:[]) x) = _modulateRepeat1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateRepeat y (a:b:_) x) = _modulateRepeat2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateRepeat y (a:b:c:_) x) = _modulateRepeat3 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b) (parametersToJS c)
sourceToJS h (ModulateRepeat y (a:b:c:d:_) x) = _modulateRepeat4 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b) (parametersToJS c) (parametersToJS d)
sourceToJS h (ModulateRepeatX y [] x) = _modulateRepeatX0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateRepeatX y (a:[]) x) = _modulateRepeatX1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateRepeatX y (a:b:_) x) = _modulateRepeatX2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateRepeatY y [] x) = _modulateRepeatY0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateRepeatY y (a:[]) x) = _modulateRepeatY1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateRepeatY y (a:b:_) x) = _modulateRepeatY2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateRotate y [] x) = _modulateRotate0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateRotate y (a:[]) x) = _modulateRotate1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateRotate y (a:b:_) x) = _modulateRotate2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateScale y [] x) = _modulateScale0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateScale y (a:[]) x) = _modulateScale1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateScale y (a:b:_) x) = _modulateScale2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateScrollX y [] x) = _modulateScrollX0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateScrollX y (a:[]) x) = _modulateScrollX1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateScrollX y (a:b:_) x) = _modulateScrollX2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (ModulateScrollY y [] x) = _modulateScrollY0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (ModulateScrollY y (a:[]) x) = _modulateScrollY1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (ModulateScrollY y (a:b:_) x) = _modulateScrollY2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)
sourceToJS h (Add y [] x) = _add0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Add y (a:_) x) = _add1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (Mult y [] x) = _mult0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Mult y (a:_) x) = _mult1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (Blend y [] x) = _blend0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Blend y (a:_) x) = _blend1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (Diff y x) = _diff0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Layer y x) = _layer0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Mask y [] x) = _mask0 (sourceToJS h x) (sourceToJS h y)
sourceToJS h (Mask y (a:[]) x) = _mask1 (sourceToJS h x) (sourceToJS h y) (parametersToJS a)
sourceToJS h (Mask y (a:b:_) x) = _mask2 (sourceToJS h x) (sourceToJS h y) (parametersToJS a) (parametersToJS b)


foreign import javascript safe "$1.synth.osc()" _osc0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.osc($2)" _osc1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.osc($2,$3)" _osc2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.osc($2,$3,$4)" _osc3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid()" _solid0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.solid($2)" _solid1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid($2,$3)" _solid2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid($2,$3,$4)" _solid3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid($2,$3,$4,$5)" _solid4 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.gradient()" _gradient0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.gradient($2)" _gradient1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.noise()" _noise0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.noise($2)" _noise1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.noise($2,$3)" _noise2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.shape()" _shape0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.shape($2)" _shape1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.shape($2,$3)" _shape2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.shape($2,$3,$4)" _shape3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.voronoi()" _voronoi0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.voronoi($2)" _voronoi1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.voronoi($2,$3)" _voronoi2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.voronoi($2,$3,$4)" _voronoi3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.brightness()" _brightness0 :: JSSource -> JSSource
foreign import javascript safe "$1.brightness($2)" _brightness1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.contrast()" _contrast0 :: JSSource -> JSSource
foreign import javascript safe "$1.contrast($2)" _contrast1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.colorama()" _colorama0 :: JSSource -> JSSource
foreign import javascript safe "$1.colorama($2)" _colorama1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.color()" _color0 :: JSSource -> JSSource
foreign import javascript safe "$1.color($2)" _color1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.color($2,$3)" _color2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.color($2,$3,$4)" _color3 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.color($2,$3,$4,$5)" _color4 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.invert()" _invert0 :: JSSource -> JSSource
foreign import javascript safe "$1.invert($2)" _invert1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.luma()" _luma0 :: JSSource -> JSSource
foreign import javascript safe "$1.luma($2)" _luma1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.luma($2,$3)" _luma2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.posterize()" _posterize0 :: JSSource -> JSSource
foreign import javascript safe "$1.posterize($2)" _posterize1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.posterize($2,$3)" _posterize2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.saturate()" _saturate0 :: JSSource -> JSSource
foreign import javascript safe "$1.saturate($2)" _saturate1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.shift()" _shift0 :: JSSource -> JSSource
foreign import javascript safe "$1.shift($2)" _shift1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.shift($2,$3)" _shift2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.shift($2,$3,$4)" _shift3 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.shift($2,$3,$4,$5)" _shift4 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.thresh()" _thresh0 :: JSSource -> JSSource
foreign import javascript safe "$1.thresh($2)" _thresh1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.thresh($2,$3)" _thresh2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.kaleid()" _kaleid0 :: JSSource -> JSSource
foreign import javascript safe "$1.kaleid($2)" _kaleid1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.pixelate()" _pixelate0 :: JSSource -> JSSource
foreign import javascript safe "$1.pixelate($2)" _pixelate1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.pixelate($2,$3)" _pixelate2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.repeat()" _repeat0 :: JSSource -> JSSource
foreign import javascript safe "$1.repeat($2)" _repeat1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.repeat($2,$3)" _repeat2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.repeat($2,$3,$4)" _repeat3 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.repeat($2,$3,$4,$5)" _repeat4 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.repeatX()" _repeatX0 :: JSSource -> JSSource
foreign import javascript safe "$1.repeatX($2)" _repeatX1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.repeatX($2,$3)" _repeatX2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.repeatY()" _repeatY0 :: JSSource -> JSSource
foreign import javascript safe "$1.repeatY($2)" _repeatY1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.repeatY($2,$3)" _repeatY2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.rotate()" _rotate0 :: JSSource -> JSSource
foreign import javascript safe "$1.rotate($2)" _rotate1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.rotate($2,$3)" _rotate2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scale()" _scale0 :: JSSource -> JSSource
foreign import javascript safe "$1.scale($2)" _scale1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.scale($2,$3)" _scale2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scale($2,$3,$4)" _scale3 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scroll()" _scroll0 :: JSSource -> JSSource
foreign import javascript safe "$1.scroll($2)" _scroll1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.scroll($2,$3)" _scroll2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scroll($2,$3,$4)" _scroll3 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scroll($2,$3,$4,$5)" _scroll4 :: JSSource -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scrollX()" _scrollX0 :: JSSource -> JSSource
foreign import javascript safe "$1.scrollX($2)" _scrollX1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.scrollX($2,$3)" _scrollX2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.scrollY()" _scrollY0 :: JSSource -> JSSource
foreign import javascript safe "$1.scrollY($2)" _scrollY1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.scrollY($2,$3)" _scrollY2 :: JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulate($2)" _modulate0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulate($2,$3)" _modulate1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateHue($2)" _modulateHue0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateHue($2,$3)" _modulateHue1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateKaleid($2)" _modulateKaleid0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateKaleid($2,$3)" _modulateKaleid1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulatePixelate($2)" _modulatePixelate0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulatePixelate($2,$3)" _modulatePixelate1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulatePixelate($2,$3,$4)" _modulatePixelate2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeat($2)" _modulateRepeat0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateRepeat($2,$3)" _modulateRepeat1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeat($2,$3,$4)" _modulateRepeat2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeat($2,$3,$4,$5)" _modulateRepeat3 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeat($2,$3,$4,$5,$6)" _modulateRepeat4 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeatX($2)" _modulateRepeatX0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateRepeatX($2,$3)" _modulateRepeatX1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeatX($2,$3,$4)" _modulateRepeatX2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeatY($2)" _modulateRepeatY0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateRepeatY($2,$3)" _modulateRepeatY1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRepeatY($2,$3,$4)" _modulateRepeatY2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRotate($2)" _modulateRotate0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateRotate($2,$3)" _modulateRotate1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateRotate($2,$3,$4)" _modulateRotate2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateScale($2)" _modulateScale0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateScale($2,$3)" _modulateScale1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateScale($2,$3,$4)" _modulateScale2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateScrollX($2)" _modulateScrollX0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateScrollX($2,$3)" _modulateScrollX1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateScrollX($2,$3,$4)" _modulateScrollX2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateScrollY($2)" _modulateScrollY0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.modulateScrollY($2,$3)" _modulateScrollY1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.modulateScrollY($2,$3,$4)" _modulateScrollY2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.add($2)" _add0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.add($2,$3)" _add1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.mult($2)" _mult0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.mult($2,$3)" _mult1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.blend($2)" _blend0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.blend($2,$3)" _blend1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.diff($2)" _diff0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.layer($2)" _layer0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.mask($2)" _mask0 :: JSSource -> JSSource -> JSSource
foreign import javascript safe "$1.mask($2,$3)" _mask1 :: JSSource -> JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.mask($2,$3,$4)" _mask2 :: JSSource -> JSSource -> JSParameters -> JSParameters -> JSSource

newtype JSOutput = JSOutput JSVal
instance PToJSVal JSOutput where pToJSVal (JSOutput x) = x
instance PFromJSVal JSOutput where pFromJSVal = JSOutput

outputToJSSource :: Hydra -> Output -> JSSource
outputToJSSource h O0 = _oToJSs0 h
outputToJSSource h O1 = _oToJSs1 h
outputToJSSource h O2 = _oToJSs2 h
outputToJSSource h O3 = _oToJSs3 h

foreign import javascript safe "$1.synth.src($1.o[0])" _oToJSs0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.src($1.o[1])" _oToJSs1 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.src($1.o[2])" _oToJSs2 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.src($1.o[3])" _oToJSs3 :: Hydra -> JSSource

outputToJS :: Hydra -> Output -> JSVal
outputToJS h O0 = _oToJSo0 h
outputToJS h O1 = _oToJSo1 h
outputToJS h O2 = _oToJSo2 h
outputToJS h O3 = _oToJSo3 h

foreign import javascript safe "$1.o[0]" _oToJSo0 :: Hydra -> JSVal
foreign import javascript safe "$1.o[1]" _oToJSo1 :: Hydra -> JSVal
foreign import javascript safe "$1.o[2]" _oToJSo2 :: Hydra -> JSVal
foreign import javascript safe "$1.o[3]" _oToJSo3 :: Hydra -> JSVal

newtype JSInput = JSInput JSVal
instance PToJSVal JSInput where pToJSVal (JSInput x) = x
instance PFromJSVal JSInput where pFromJSVal = JSInput

inputToJS :: Hydra -> Input -> JSInput
inputToJS h S0 = _iToJS0 h
inputToJS h S1 = _iToJS1 h
inputToJS h S2 = _iToJS2 h
inputToJS h S3 = _iToJS3 h

foreign import javascript safe "$1.s[0]" _iToJS0 :: Hydra -> JSInput
foreign import javascript safe "$1.s[2]" _iToJS1 :: Hydra -> JSInput
foreign import javascript safe "$1.s[3]" _iToJS2 :: Hydra -> JSInput
foreign import javascript safe "$1.s[4]" _iToJS3 :: Hydra -> JSInput


evaluateStatement :: Hydra -> Statement -> IO ()
evaluateStatement h (Out s o) = _out (sourceToJS h s) (outputToJS h o)
evaluateStatement h (Speed x) = _speed h (parametersToJS x)
evaluateStatement h (Render o) = _render h (maybeOutputToJS h o)
evaluateStatement h (InitScreen i) = _initScreen h (inputToJS h i)
-- evaluateStatement h (InitCam i) = _initCam h (inputToJS h i)

maybeOutputToJS :: Hydra -> Maybe Output -> JSVal
maybeOutputToJS h Nothing = nullRef
maybeOutputToJS h (Just x) = outputToJS h x

foreign import javascript safe
  "$1.out($2);"
  _out :: JSSource -> JSVal -> IO ()

foreign import javascript safe
  "$1.synth.render($2);"
  _render :: Hydra -> JSVal -> IO ()

foreign import javascript safe
  "$1.synth.speed=$2;"
  _speed :: Hydra -> JSParameters -> IO ()

--h.synth.s0.initScreen()
foreign import javascript safe
  "$1.initScreen();" --- check thisss
  _initScreen :: JSInput -> IO ()

-- foreign import javascript safe
--   "$1.synth.$2.initCam();"
--   _initCam :: Hydra -> JSInput -> IO ()
