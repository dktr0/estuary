{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.Punctual (punctualTutorial) where

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

punctualTutorial :: Tutorial
punctualTutorial = Tutorial {
  tutorialTitle = Map.fromList [
    (English,"Punctual tutorial")
  ],
  tutorialPages = Seq.fromList [
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"First Steps")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
          Paragraph [ Text $ Map.fromList [
            (English,"This tutorial will cover some of the basics of making sound and visuals with the Punctual live coding language. You can copy any of the code examples in this tutorial to the editor at the bottom of the page, make sure that Punctual is selected in the drop-down menu, and click Evaluate, to see/hear what they do. You can also click directly on any code examples to copy and evaluate (\"make it go\") automatically. Changing things in the examples is highly recommended! This first set of examples will draw a white circle in various locations. Click on the various examples to try them out (there will be a small delay before each example takes effect):")
            ]],
          Snippet 1 True Punctual "circle [0,0] 0.25 >> rgb",
          Snippet 1 True Punctual "circle [0.5,0] 0.5 >> rgb",
          Snippet 1 True Punctual "circle [-0.5,0.8] 0.75 >> rgb",
          Paragraph [ Text $ Map.fromList [
            (English,"In the above, the first number after the word 'circle' is the left-to-right position of the centre of the circle, with the left edge of the screen being -1, the centre being 0 and the right edge being 1. The second number after the word circle is the bottom-to-top position of the centre of the circle, with the bottom of the screen being -1, the middle being 0, and the top being 1. The third number after the word circle is how big the circle is (its radius). We can also use Punctual to make sound:")
            ]],
          Snippet 1 True Punctual "tri (60m) * (-20)db >> splay",
          Snippet 1 True Punctual "tri (48m) * (-20)db >> splay",
          Snippet 1 True Punctual "tri (72m) * (-20)db >> splay",
          Paragraph [ Text $ Map.fromList [
            (English,"In the above, the word 'tri' refers to a 'triangle wave'. The '60m' '48m' and '72m' change the frequency of the oscillator resulting in different pitches. You can make it quieter by decreasing the -20db to lower values such as -30db or -40db. We can also use Punctual to make sound and visuals at the same time, using similar language. In the example below note how one line ends with => rgb and is drawing a circle on the screen, while the other line ends with => splay and is causing some sound to happen. Notice also that the two lines have a semicolon (;) between them. You can even put the two statements on the same line but you must have a semicolon (;) between them. When you are finished playing with these examples, you can click Next (above) to go to the next page of this tutorial.")
            ]],
          Snippet 1 True Punctual "circle [0.5,-0.5] 0.1 >> rgb;\ntri (63m) * (-20)db >> splay"
          ],
        CodeView 1 0
      ]
    }),
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"More drawing examples")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
          Paragraph [ Text $ Map.fromList [
            (English,"Here are some of the simple shapes we can make using Punctual, apart from the circle introduced on the previous page. Copying the examples and then changing the numbers to other numbers will help you figure out what the numbers mean:")
            ]],
          Snippet 1 True Punctual "vline 0 0.002 >> rgb",
          Snippet 1 True Punctual "hline 0 0.002 >> rgb",
          Snippet 1 True Punctual "rect [0,0] [0.5,0.5] >> rgb",
          Snippet 1 True Punctual "point [0,0] >> rgb",
          Paragraph [ Text $ Map.fromList [
            (English,"We can replace any individual number with a set of multiple numbers. When we do this we get different results on different \"channels\" of the visual output - red, green, and blue:")
            ]],
          Snippet 1 True Punctual "vline [-0.25,0,0.25] 0.002 >> rgb",
          Snippet 1 True Punctual "circle [-0.25,0,0.25,0,0,0] 0.25 >> rgb",
          Paragraph [ Text $ Map.fromList [
            (English,"We can also replace any number with an oscillator with a low frequency so that things move around:")
            ]],
          Snippet 1 True Punctual "hline (tri 1) 0.002 >> rgb",
          Snippet 1 True Punctual "hline (tri 2) 0.002 >> rgb",
          Snippet 1 True Punctual "hline (tri 0.25) 0.002 >> rgb",
          Paragraph [ Text $ Map.fromList [
            (English,"Notice the brackets in the preceding examples - they are very important as they make sure that the number gets applied to the oscillator:")
            ]]
          ],
        CodeView 1 0
      ]
    })


  ]
}
