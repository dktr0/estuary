# Cinecer0 Reference

The CineCer0 mini-language (pronounced “sin–ay–ser-oh”) language allows video files to be projected temporally and geometrically, targeting similar functionality to that of [CineVivo](https://github.com/essteban/CineVivo), again with an economical Haskell-like notation. Additional functions that enable the audio of the video and allow the player to add text on top of the video (or without any relationship with the video) have been implemented.  

## Playing Videos, Images, or Text

video "myVideo.extension" or image "myImage.extension"  --videos/images play as a string <br />
video "videoURL" <br />
image "imageURL" <br />
text "This is not a text" -- the string represents the text that will be displayed in the canvas <br />
"" --empty state

## Position on Videos, Images, or Text
setPosX [x] $ -- from left (-1) to right 1 <br />
setPosY [y] $ -- from bottom (-1) to top 1 <br />
setCoord [x] [y] $ <br />

## Videos with Audio

vol 0.5 $ "myVideo.extension" -- videos play with unmuted audio

## Video/Image functions

setWidth [w] $ -- 1 = natural video width <br/>
setHeight [h] $ -- 1 = natural video height <br />
setSize [wh] $ --one parameter will affect both width and heigh proportionally <br />
setRotate [d] $ <br /> -- parameter in degrees
setOpacity [o] $ -- from 0 - 1 (no opacity) <br />
setBlur [bl] $ -- 0 = no blur (1++ = more) <br />
setBrightness [br] $ --  0-0.9 = less, 1++ = more <br />
setContrast [c] $ -- 0-0.9 = less, 1++ = more <br />
setGrayscale [g] $ -- 0 = no grayscale, 1 = full grayscale <br />
setSaturate [s] $ -- 1 = natural video saturation (1++ = more, 1-- =less) <br />
circleMask [m] $ -- 0 is no mask, 0-0.99 makes the mask appear from biggest to smallest (it grows/decrease from the centre)
circleMask' [m] [x] [y] $ -- similar to circleMask but with two more parameters that move the center of the circleMask
sqrMask [m] $ -- 0 is no mask, 0-0.99 makes the mask appear from biggest to smallest (it grows/decrease from the centre)
rectMask [t] [r] [b] [l] $ -- accepts four parameters: top right bottom left, which are the amount of reduction in each side
z [n] -- changes the depth of the video being reproduced

## Text Function

size [n] --change the size of the text, grows from 1 <br />
font "fontType" -- fonts available depending on browser characteristics <br />
colour "colour" -- adds colour by name or by hexacolor (all colour funcs will be applied to the text) <br />
rgb [r] [g] [b] -- adds colour by rgb, normalised from 0 to 1 <br />
rgb' [r] [g] [b] [a] -- adds colour by rgb and alpha <br />
hsl [h] [s] [l] -- adds colour by hue, saturation and lightness, parameters are normalised from 0 to 1 <br />
hsl' [h] [s] [l] [a] -- adds colours by hsl and alpha <br />
strike -- strikes the text with a white line <br />
bold -- the weight of the font becomes heavier <br />
italic -- the style of the font changes to italic <br />
z [n] -- changes the depth of the text, z 0 is the top layer  <br />

## ramp

(ramp [Dur_In_Cycles] [Initial_Value] [End_Value]) <br />

ramp can be use in style functions. Example <br />
width (ramp x y z) <br />

fadeIn [Dur_In_Cycles]

fadeOut [Dur_In_Cycles]

## quant

$ quant [Cicle_Multiplier] [Offset] <br />

quant function has two values. Cycle multiplier aligns the anchor time with multiples of the given value. Offset is a value from 0 to 1 that will shift the starting position of the quantisation <br />

quant can be use with any style functions with or without a ramp. Example: <br />
opacity (ramp x y z) $ quant x y <br />

## Time Functions (for Videos)

natural [shift] $ -- aligns the starting time of the video with the first beat of the first measure <br />

every [cycles] [shift] $ -- adjusts the length to a given number of cycles <br />

round [shift] $ -- adjusts the length to the nearest number of measures in Estuary's tempo. <br />

roundMetre [shift] $ -- adjusts the length to the nearest number of measures multiple of 2,4,8,16,etc. in order to maintain the video synchronised with Estuary's tempo. <br />

chop [startPos] [endPos] [cycles] [shift] $ -- plays the video from the starting position (0-1) to the end position (0-1) stretching or compressing the length to adjust it to the number of cycles provided as a parameter. <br />

chop' [startPos] [cycles] [shift] $ --  plays the video from the starting position (0-1) stretching or compressing its length to adjust it to the number of cycles provided as a parameter. <br />

chopSecs [startPos] [endPos] [cycles] [shift] $ -- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. This function does not have the start and end positions normalized from 0 to 1. <br />
