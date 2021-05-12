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
  tutorialPages = Seq.fromList [
    --page 0
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: import + audio volume"),
        (Español,"Video: importar + volumen de audio")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Evaluate the url of a video inside quotation marks"),
              (Español,"Evalúa la url de un video dentro de comillas")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/9/9e/Carpenter_bee_on_Abelia_flowers.webm/Carpenter_bee_on_Abelia_flowers.webm.480p.webm\"",
            Example CineCer0 "\"https://cdn.videvo.net/videvo_files/video/free/2020-05/small_watermarked/3d_ocean_1590675653_preview.webm\"",
            Paragraph [ Text $ Map.fromList [
              (English,"You can apply volume with: \"vol #\" $ -- value: 0 to 1"),
              (Español,"Puedes aplicar volumen con: \"vol #\" $ -- valor: 0 a 1")
            ]],
            Example CineCer0 "vol 0.5 $ \"https://github.com/jac307/videoTextures/blob/master/walter/amor.mov?raw=true\"",
            Example CineCer0 "vol 1 $ \"https://github.com/jac307/videoTextures/blob/master/walter/horoscopo/leo.mov?raw=true\""
        ],
        CodeView 1 0
      ]
    }),
    --page 1
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: size and concatenation of functions"),
        (Español,"Video: tamaño y concatenación de funciones")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Change size: \"setWidth #\" and \"setHeight #\" --value grows from 0. Additionlly: \"setSize #\" --requires just one value and affects both width and height"),
              (Español,"Cambiar tamaño (ancho y alto): \"setWidth #\" y \"setHeight #\" --el valor crece desde 0. Adicionalmente: \"setSize #\" --requiere únicamente de un valor y afecta tanto ancho como alto")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"Default values: width:1 height:1 = natural aspect ratio of the video."),
              (Español,"Los valores de arranque son: ancho:1 alto:1 = el ratio-aspect natural del video.")
            ]],
            Example CineCer0 "setWidth 0.2 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Example CineCer0 "setHeight 2.1 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Example CineCer0 "setSize 0.8 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Paragraph [ Text $ Map.fromList[
              (English,"As with volumen and size, all functions must be applied before the video-url and be concatenated with \"$\" or \"()\"."),
              (Español,"Así como con el volmen y tamaño, todas las funciones deben ir aplicadas antes de la url del video y concatenadas ya sea por \"$\" o \"()\".")
            ]],
            Example CineCer0 "setSize 0.5 $ vol 0.5 $ \"https://github.com/jac307/videoTextures/blob/master/walter/amor.mov?raw=true\"",
            Example CineCer0 "vol 1 (setSize 0.8 (\"https://github.com/jac307/videoTextures/blob/master/walter/horoscopo/leo.mov?raw=true\"))"
        ],
        CodeView 1 0
      ]
    }),
      --page 2
      (TutorialPage {
        tutorialPageTitle = Map.fromList [
          (English,"Video: position"),
          (Español,"Video: posición")
        ],
        tutorialPageView = GridView 1 2 [
          Views [
              Paragraph [ Text $ Map.fromList[
                (English,"Change position: \"setPosX #\", \"setPosY #\" and \"setCoord # #\" --to modify both values"),
                (Español,"Cambiar posición: \"setPosX #\", \"setPosY\" # y \"setCoord # #\" --para modificar ambos valores")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"Values on the screen: it goes from (-1) top/left to 1 to bottom/right, 0 = centre. Negative numbers must be inside \"()\". Anchor point on the center."),
                (Español,"Valores con relación a la pantalla:. Los números negativos deben ir dentro de \"()\". Punto de anclaje en el centro.")
              ]],
              Example CineCer0 "setPosX 0.5 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
              Example CineCer0 "setPosY (-0.5) (\"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\")",
              Example CineCer0 "setCoord 0.2 (-0.8) $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
              Paragraph [ Text $ Map.fromList[
                (English,"Concatenated examples:"),
                (Español,"Ejemplos de concatenación:")
              ]],
              Example CineCer0 "setPosX 0.5 $ setSize 0.5 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
              Example CineCer0 "setCoord 0 (-0.5) $ setSize 0.5 $ vol 0.5 $ \"https://github.com/jac307/videoTextures/blob/master/walter/amor.mov?raw=true\""
          ],
          CodeView 1 0
        ]
    })
  ]
}
