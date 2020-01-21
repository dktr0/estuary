# Cinecer0 Reference

The CineCer0 mini-language (pronounced “sin–ay–ser-oh”) language allows video files to b e projected temporally and geometrically, targeting similar functionality to that of [CineVivo](https://github.com/essteban/CineVivo), again with an economical Haskell-like notation.

## Playing Videos

"video.extension" --videos play as a string <br />
"videoURL" --you can add the URL to play videos <br />

## Image

width [w] $ -- changes height (h) of the video, 1 = natural width <br/>
height [h] $ -- changes width (w) of the video, 1 = natural height <br />
size [w] [h] $ -- changes both height and width <br />
posX [x] $ -- moves the video in the X-coordinate, from left (-1) to right 1 <br />
posY [y] $ -- moves the video in the Y-coordinate from bottom (-1) to top 1 <br />
pos [x] [y] $ -- moves in both coordinates <br />
opacity [o] $ -- opacity goes from 0 - 1 (no opacity) <br />
blur [bl] $ -- 0 = no blur (1++ = more) <br />
brightness [br] $ --  100 = video brightness (0-99 = less, 100++ = more) <br />
contrast [c] $ -- in which 100 = video contrast (0-99 = less, 100++ = more) <br />
grayscale [g] $ -- in which 0 = no grayscale, 100 = full grayscale <br />
saturate [s] $ -- in which 1.0 = video saturation (1++ = more) <br />


## Time Functions

<<<<<<< HEAD
natural sh $         -- sh shifts the starting point of the video. 0 start of the video (0%), 0.5 starts at 50% of the video, etc.
every c sh $         -- adjusts/aligns length to the given number of cycles (c).
round sh $           -- adjusts/aligns length to the nearest amount of cycles of the natural length. 
roundMetre sh $      -- adjusts/aligns length to the nearest power of 2 number of cycles (metre in musical terms).
chop sP eP c sh      -- adjusts/aligns length to the given number of cycles (c) from starting position (sP) to ending position (eP). sP and eP in percentage normalised from 0.0 to 1.0.
chopSecs sP eP c sh  -- sP and eP in seconds.
chop' sP c sh        -- adjusts/aligns length to the given number of cycles (c) from starting position (sP). sP in percentage normalised from 0.0 to 1.0.

=======
natural [shift] $ -- aligns the starting time of the video with the first beat of the first measure <br />
>>>>>>> 5187e1fb51a0e84cd3e71784b9f071fccf700b11

every [cycles] [shift] $ -- adjusts the length to a given number of cycles <br />

round [shift] $ -- adjusts the length to the nearest number of measures in Estuary's tempo. <br />

roundMetre [shift] $ -- adjusts the length to the nearest number of measures multiple of 2,4,8,16,etc. in order to maintain the video synchronised with Estuary's tempo. <br />

chop [startPos] [endPos] [cycles] [shift] $ -- plays the video from the starting position (0-1) to the end position (0-1) stretching or compressing the length to adjust it to the number of cycles provided as a parameter. <br />

chop' [startPos] [cycles] [shift] $ --  plays the video from the starting position (0-1) stretching or compressing its length to adjust it to the number of cycles provided as a parameter. <br />

chopSecs [startPos] [endPos] [cycles] [shift] $ -- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. This function does not have the start and end positions normalised from 0 to 1. <br />
