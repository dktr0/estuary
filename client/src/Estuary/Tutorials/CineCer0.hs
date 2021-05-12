{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.CineCer0 (cineCer0Tutorial) where

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

cineCer0Tutorial :: Tutorial
cineCer0Tutorial = Tutorial {
  tutorialTitle = Map.fromList [
    (English,"CineCer0 tutorial"),
    (Español,"Tutorial de CineCer0")
  ],
  --page 0
  tutorialPages = Seq.fromList [
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: import"),
        (Español,"Video: importar")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Evaluate the url of a video inside quotation marks"),
              (Español,"Evalúa la url de un video dentro de comillas")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/9/9e/Carpenter_bee_on_Abelia_flowers.webm/Carpenter_bee_on_Abelia_flowers.webm.480p.webm\"",
            Example CineCer0 "\"https://github.com/jac307/videoTextures/blob/master/mariposa/07.mov?raw=true\"",
            Example CineCer0 "\"https://cdn.videvo.net/videvo_files/video/free/2020-05/small_watermarked/3d_ocean_1590675653_preview.webm\""
        ],
        CodeView 1 0
      ]
    })
    --page 0
  ]
}
