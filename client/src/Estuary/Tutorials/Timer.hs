{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.Timer (timerTutorial) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq

import Estuary.Types.Tutorial
import Estuary.Types.View
import Estuary.Types.TranslatableText
import Estuary.Types.Language
import Estuary.Types.Definition
import Estuary.Types.TextNotation

timerTutorial :: Tutorial
timerTutorial = Tutorial {
  tutorialTitle = Map.fromList [
    (English,"Timer tutorial"),
    (Español,"Tutorial del Timer (temporizador)")
  ],
  tutorialPages = Seq.fromList [
    -- ///////////////////////////////////////////////////////////////
    --page 1: intro
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
      (English,"Intro"),
      (Español,"Intro")
      ],
      tutorialPageView = GridView 1 2 [
           Views [
          
              Paragraph [ Text $ Map.fromList[
                (English,"The Timer widget will allow players to count arbitrary time intervals from specified points in time. This is useful for some common live coding practices like: marking a prescribed time for a live coding act; communicate with the audience or other players the different sections of a performance, where different intentions are explored; synchronise precise gestures in an ensemble; or any other use we have not imagined of a (set of) count-down(s)."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"You can call the timer with the command in the View system ‘timer Int’. For example, ‘timer 10’ will generate a timer that occupies the index 10 in the view system. The timer has an interface based on clickable areas. So, if you hoover over the timer you will get indications of what would happen if you click on that area. As you can see, the right area will change the visualisation mode, the centre up will reset/start the program, the centre down will pause or resume it and the left will flip the widget to editing mode."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "To begin, activate the timer (by pressing the reset or resume/pause areas) and change the visualisation mode. Observe the different ways in which we are representing ellapsing time. Some have labels for each section, some are more visual, some display numbers and text. The visualisers are: progress bar with labels, progress bar, sand-clock with labels, sand-clock, numeric with label, numeric, only labels, circular, and stack."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "The stack visualiser has a special behaviour; it will show you all segments of your program at the same time and a line elapsing throughout the layers."),
                (Español,"")
              ]]
              ],
          TimerView 0
      ]
    }),
    --page 2: editing mode
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
      (English,"Edit Mode"),
      (Español,"Modo de Edición")
      ],
      tutorialPageView = GridView 1 2 [
           Views [
          
              Paragraph [ Text $ Map.fromList[
                (English,"If you click on the left side of the timer display you will flip the widget into its ‘edit’ mode. Take a moment to see how different it is from the ‘display’ mode. On the top right you will see a ‘peek’ to the state of the timer display. This is useful because the flipping between ‘edit’ and ‘display’ mode is not networked; this means that some players might be still seeing the timer's display while someone else edits. "),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"Under the ‘peek’ area you will see an icon for loop, click on it to unloop (icon is grey in classic theme) and loop (icon is green in classic theme) the program. Under that loop icon you will see an icon of a metronome; click on it to change the time units from BPM/CPS (when icon is metronome) to seconds (when icon is clock). On the top left side you will see a botton called ‘display’ and a botton to run programs. The ‘display’ button will flip the widget back to display mode. The ▶ button will update the program as written in the text area below."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "Compare the timer’s behaviour when modifying these different aspects: looped and unlooped as well as using BPMs as time units or seconds. Remember that, in order to start playing a program you need to flip into display mode and push the reset or pause/resume areas. There is no way to start a program in the ‘edit’ mode."),
                (Español,"")
              ]]
              ],
          TimerView 0
      ]
    }),
    --page 3: write programs
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
      (English,"Write your own Timer program"),
      (Español,"Escribe tu propio programa de temporizador")
      ],
      tutorialPageView = GridView 1 2 [
           Views [
          
              Paragraph [ Text $ Map.fromList[
                (English,"In order to change a program of the timer we made a very simple text interface. The default program is: a=5, b=7, c=3. This can be broken down into: Section ‘a’ will have a duration of 5 time units; section ‘b’ will have a duration of 7 time units and section ‘c’ will have a duration of 3 time units. So, as you can see, the way to write a program is: nameOfSection1 = duration1, nameOfNextSection2 = duration2, etc..."),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"In order to update the program, the ▶ button needs to be pressed, only then the changes will be reflected in the widget’s state and behaviour. This widget was inspired by a performance of the Cybernetic Orchestra where a prescribed set of durations allow performers to change tempo (or rhythmic figures or metre) in a synchronised manner. The Cybernetic Orchestra, if in need to play a similar piece, could now use this timer program: triplets = 120, quintuplets = 150, eight notes = 100, free rhythms = 60"),
                (Español,"")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "Paste or re-write the program above into the timer text interface and run it as an example of how to make your own programs. Then, make your own program that responds to your own performance ideas. :) Happy coding! "),
                (Español,"")
              ]]
              ],
          TimerView 0
      ]
    })
  ]
}
