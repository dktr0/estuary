# This is the documentation for CineCer0
The CineCer0 mini-language (pronounced “sin–ay–ser-oh”) language allows video files to b e projected temporally and geometrically, targeting similar functionality to that of [Cine Vivo](https://github.com/essteban/CineVivo), again with an economical Haskell-like notation.

## GENERAL FUNCTIONS


## TIME FUNCTIONS

The temporal dimensions of the video can be manipulated through a series of functions that stretch or compress the video rate and provide a specific starting position in order to align the video playback to the tempo and cycle count of Estuary's time-keeping infrastructure often also considering the evaluation time of the player.

### playNatural   

Parameters: shift

Example: 

```
playNatural 0.5 "agua.mov"  
-- plays the video aligning the 0:00" of the video with the first beat of the first measure and with a rate of 1. 

```

### playEvery

Parameters: cycles shift 

Example: 

```
playEvery 4 0 "agua.mov"  
-- plays the video adjusting its length to the given number of cycles in the first parameter. 

```

### playRound

Parameters: shift 

Example: 

```
playRound 0 "agua.mov"  
-- plays the video adjusting its length to the nearest number of measures in Estuary's tempo in order to maintain the video synchronised with Estuary's tempo and altering its rate minimally. 

```

### playRoundMetre

Parameters: shift 

Example: 

```
playRoundMetre 0 "agua.mov"  
-- plays the video adjusting its length to the nearest number of measures multiple of 2,4,8,16,etc. in order to maintain the video synchronised with Estuary's tempo, altering its rate minimally and relating its length to musical periods that are in the power of two. 

```

### playChop

Parameters: startPos endPos cycles shift 

Example: 

```
playChop 0.3 0.7 2 0 "agua.mov"  
-- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. The start and end positions are normalised from 0 to 1. In the present example the video will start at 30% from the start and will end at 70% of the video. 

```

### playChop'

Parameters: startPos cycles shift 

Example: 

```
playChop' 0.3 2 0 "agua.mov"  
-- plays the video from the starting position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. 

```

### playChopSecs

Parameters: startPos endPos cycles shift 

Example: 

```
playChop 3.5 10.0 2 0 "agua.mov"  
-- plays the video from the starting position to the end position stretching or compressing its length to adjust it to the number of cycles provided as a parameter. This function does not have the start and end positions normalised from 0 to 1. 

```

### playNow