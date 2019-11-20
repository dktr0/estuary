# Cinecer0 Reference

## Playing Videos

"video.extension" --videos play as a string
"videoURL" --you can add the URL to play videos

## Geometric Functions

width [h] $ -- changes height (h) of the video, 1 = natural width
height [w] $ -- changes width (w) of the video, 1 = natural height
size [h] [w] $ -- changes both height and width
posX [x] $ -- moves the video in the X-coordinate, from left (-1) to right 1
posY [y] $ -- moves the video in the Y-coordinate from bottom (-1) to top 1
pos [x] [y] $ -- moves in both coordinates

## Style Functions

opacity [o] $ -- 100 = no opacity, 0-99 = less opacity
blur [bl] $ -- 0 = no blur (1++ = more)
brightness [br] $ --  100 = video brightness (0-99 = less, 100++ = more)
contrast [c] $ -- in which 100 = video contrast (0-99 = less, 100++ = more)
grayscale [g] $ -- in which 0 = no grayscale, 100 = full grayscale
saturate [s] $ -- in which 1.0 = video saturation (1++ = more)

## Time Functions

natural sh $         -- sh shifts the starting point of the video. 0 start of the video (0%), 0.5 starts at 50% of the video, etc.
every c sh $         -- adjusts/aligns length to the given number of cycles (c).
round sh $           -- adjusts/aligns length to the nearest amount of cycles of the natural length. 
roundMetre sh $      -- adjusts/aligns length to the nearest power of 2 number of cycles (metre in musical terms).
chop sP eP c sh      -- adjusts/aligns length to the given number of cycles (c) from starting position (sP) to ending position (eP). sP and eP in percentage normalised from 0.0 to 1.0.
chopSecs sP eP c sh  -- sP and eP in seconds.
chop' sP c sh        -- adjusts/aligns length to the given number of cycles (c) from starting position (sP). sP in percentage normalised from 0.0 to 1.0.



<!-- # This is the documentation for CineCer0
The CineCer0 mini-language (pronounced “sin–ay–ser-oh”) language allows video files to b e projected temporally and geometrically, targeting similar functionality to that of [Cine Vivo](https://github.com/essteban/CineVivo), again with an economical Haskell-like notation.

## VIDEOS

### load/play

There are two ways to call a video. The first one is to write/evaluate, as a string, the url of the video.

```
"https://raw.githubusercontent.com/jac307/memoriasVideoSamples/master/_/agua.mov"  

```

When your running your Estuary locally, the second option is to place your videos on estuary/dev-staging/Estuary.jsexe then run the videos with the name plus the extension as a string.

```
"agua.mov"  

```

The videos will run in loop. The video extensions tested and working are .mov and .mp4, .flv is not currently working.


## TRANSFORMATIONS

When adding any kind fo transformation, these need to be written before the video string.
  **transformation "video.mov"**
You can also add the symbol "$" between the transformation and the video string.
  **transformation $ "video.mov"**
You can add multiple transformation with the same logic
  **transformation $ transformation $ transformation $ transformation $ "video.mov"**
  or
  **transformation transformation transformation transformation "video.mov"**

The videos can we transform in three ways: by changing their geometry, changing their style, and time function that change the rate and time of each video.

Negative parameters must be place in parenthesis.


### Geometric Functions

#### position
Parameters: X-position ((-1) the right to 1 the left) Y-position ((-1) the bottom to 1 the top)
```
pos 0.0 0.0 "agua.mov"  
-- video placed at the center.
```
You can also change the X-position and Y-position separately
```
posX 0.5 "agua.mov"
-- video halfway to the left on the X coordinate
posY (-0.5) "agua.mov"
-- video halfway to the bottom on the Y coordinate
```

#### size
Parameters: width height (1 would be the video size)
```
size 0.5 0.5 "agua.mov"  
-- video half size in both width and height
```
You can also change the width and height separately
```
w 0.5 "agua.mov"
-- just video width half size
h 0.5 "agua.mov"
-- just video height half size
```

### Style Functions

#### opacity
Parameters: opacity (from 0-1, 1= the maximum opacity)
```
opacity 0.5 "agua.mov"  
-- video will be half of its opacity.
```

### Time Functions

The temporal dimensions of the video can be manipulated through a series of functions that stretch or compress the video rate and provide a specific starting position in order to align the video playback to the tempo and cycle count of Estuary's time-keeping infrastructure often also considering the evaluation time of the player.

#### natural   

Parameters: shift

```
natural 0.5 "agua.mov"  
-- plays the video aligning the 0:00" of the video with the first beat of the first measure and with a rate of 1.

```

#### every

Parameters: cycles shift

```
every 4 0 "agua.mov"  
-- plays the video adjusting its length to the given number of cycles in the first parameter.

```

#### round

Parameters: shift

```
round 0 "agua.mov"  
-- plays the video adjusting its length to the nearest number of measures in Estuary's tempo in order to maintain the video synchronised with Estuary's tempo and altering its rate minimally.

```

#### roundMetre

Parameters: shift

```
roundMetre 0 "agua.mov"  
-- plays the video adjusting its length to the nearest number of measures multiple of 2,4,8,16,etc. in order to maintain the video synchronised with Estuary's tempo, altering its rate minimally and relating its length to musical periods that are in the power of two.

```

#### chop

Parameters: startPos endPos cycles shift

```
chop 0.3 0.7 2 0 "agua.mov"  
-- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. The start and end positions are normalised from 0 to 1. In the present example the video will start at 30% from the start and will end at 70% of the video.

```

#### chop'

Parameters: startPos cycles shift

```
chop' 0.3 2 0 "agua.mov"  
-- plays the video from the starting position stretching or compressing its length to adjust it to the number of cycles provided as a parameter.

```

#### chopSecs

Parameters: startPos endPos cycles shift

```
chopSecs 3.5 10.0 2 0 "agua.mov"  
-- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. This function does not have the start and end positions normalised from 0 to 1.

```

#### now -->
