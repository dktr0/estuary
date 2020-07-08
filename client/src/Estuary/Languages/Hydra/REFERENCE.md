# HYDRA-Estuary Reference

## Parameters
double <br />
list of doubles = [] <br />
  .fast(amount) -- as a transformer of list <br />


## Sources

osc(fequency, sync, rgb-offset) // defaults: 60.0, 0.1, 0.0 <br />
solid(r, g, b, a) // defaults: 0.0, 0.0, 0.0, 1.0 <br />
gradient(speed) // default: 0.0 <br />
noise(scale, offset) // defaults: 10.0, 0.1 <br />
shape(sides, radius, smoothing) // defaults: 3.0, 0.3, 0.01 <br />
voronoi(scale, speed, blending) // defaults: 5.0, 0.3, 0.3 <br />


## Outputs
.out(buffer) // osc: o0, o1, o2, o3 // src: s0, s1, s2, s3 <br />
render(buffer) // default: o0 <br />


## Transformations
.brightness(amount) // default: 0.4 <br />
.contrast(amount) // default: 1.6 <br />
.color(r, g, b) // vec4 <br />
.colorama(amount) // default: 0.005 -- shifts HSV values <br />
.invert(amount) // default:1.0 <br />
.luma(threshold, tolerance) // defaults: 0.5, 0.1 <br />
.posterize(bins, gamma) // defaults: 3.0, 0.6 <br />
.saturate(amount) // default: 2.0 <br />
.shift(r, g, b, a) // defaults: 0.5 for all <br />
.thresh(threshold, tolerance) // defaults: 0.5, 0.04 <br />
.kaleid(#sides) // default: 4.0 <br />
.pixelate(x, y) // defaults: 20.0, 20.0 <br />
.repeat(repeatX, repeatY, offsetX, offsetY ) // defaults: 3.0, 3.0, 0.0, 0.0 <br />
.repeatX(reps, offset) <br />
.repeatY(reps, offset) <br />
.rotate(angle, speed) // defaults: 10.0, 0.0 <br />
.scale(size, xMult, yMult) // defaults: 1.5, 1.0, 1.0 <br />
.scrollX(scrollX, speed) // defaults: 0.5, 0.0 <br />
.scrollX(scrollY, speed) // defaults: 0.5, 0.0 <br />


## Modulators

// t = texture = source => color, osc, src, noise, shape… <br />

.modulate(t, amount) // amount’s default: 0.1 <br />
.modulateHue(t, amount) // default: 0.4 <br />
.modulateKaleid(t, #Sides) // default: 4.0 <br />
.modulatePixelate(t, multiple, offset) // defaults: 10.0, 3.0 <br />
.modulateRepeat (t, repeatX, repeatY, offsetX, offsetY) //defaults: 3.0, 3.0, 0.5, 0.5 <br />
.modulateRepeatX (t, repeatX, offsetX) <br />
.modulateRepeatY (t, repeatY, offsetY) <br />
.modulateRotate(t, multiple, offset) // defaults: 1.0, 0.0 <br />
.modulateScale(t, multiple, offset) // defaults: 1.0, 1.0 <br />
.modulateScrollX(t, scrollX, speed) // defaults: 0.5, 0.0 <br />
.modulateScrollY(t, scrollY, speed) // defaults: 0.5, 0.0 <br />


## Operators

.add(t, amount) // default: 0.5 <br />
.mult(t, amount) // default: 0.5 <br />
.blend(t, amount) // default: 0.5 <br />
.diff(t) <br />
.layer(t) <br />
.mask(t, reps, offset) // defaults: 3.0, 0.5 <br />
