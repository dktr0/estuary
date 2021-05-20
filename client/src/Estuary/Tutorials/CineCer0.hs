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
    -- ///////////////////////////////////////////////////////////////
    --page 0: intro
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
      (English,"Intro"),
      (Español,"Intro")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"CineCer0 is a language for displaying and transforming videos and text in the browser. It can be used, for example, in the performance of live coded cinema, kinetic typography, VJ-ing, etc."),
              (Español,"CineCer0 es un lenguaje para reproducir y transformar video, así como renderizar y transformar texto en el navegador. Puede ser usado, por ejemplo, para performance de live cinema con programación al vuelo, animación tipográfica, VJ-ing, etc.")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"In this tutorial, we will review CineCer0 functions step-by-step. On each page, you will find executable examples. You can evaluate and modify them on the fly. These are two examples of the possibilities of CineCer0:"),
              (Español,"En este tutorial revisaremos las funciones de CineCer0 paso a paso. En cada página encontrarán ejemplos ejcutables. Pueden evaluarlos y modificarlos. Estos son dos ejemplos de las posibilidades de CineCer0:")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"In the first example, a video with audio and in a shape of a circle appears."),
              (Español,"En el primer ejemplo, un video con audio y con la forma de un círclo aparecerá con un tamaño modificado.")
            ]],
            Example CineCer0 "circleMask 0.5 $ vol 0.5 $ \"https://github.com/jac307/videoTextures/blob/master/walter/horoscopo/leo.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,"In the second example, a text appears; the size has been modified, as well as the position."),
              (Español,"En el segundo ejemplo, un texto aparece, éste tiene tanto el tamaño como la posición modificados.")
            ]],
            Example CineCer0 "fontSize 10 $ setPosY (-0.8) $ text \"This is a text\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 1: importing video
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Play Video"),
        (Español,"Reproducir Video")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"(1) Evaluate the URL of a video inside quotation marks \"\", or (2) Evaluate the word \"video\" + the URL of a video inside quotation marks \"\"."),
              (Español,"(1) Evalúa la URL de un video dentro de comillas \"\", o Evalúa la palabra \"video\" + la URL de un video dentro de comillas \"\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"The URL for videos must point directly to a video file that the web browser can play (not a URL of a video streaming service). Often, such URLS would end in \".mov\", \".mp4\" or \".m4v\"."),
              (Español,"La URL de los videos debe direccionar al archivo de video que el navegador puede reproducir (no a la URL de un video que es parte de un servidor de streaming). Usualmente, estas URLS terminan en \".mov\", \".mp4\" or \".m4v\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"The following examples are using the two different methods to import/play videos. Later on we will modify the appearance (position, size, blur, birghtness, etc). To erase the video: erase eveything and evaluate the empty code-box. To change the video: replace the url and evaluate again."),
              (Español,"En los estos ejemplos se están usando ambos métodos para importar/reproducir videos. Más adelante veremos cómo modificar la apariencia (posición, tamaño, blur, brillo, etc). Para borrar el video: borra todo y evalua la caja de código vacía. Para cambiar el video: reemplaza la url y evalúa de nuevo.")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/9/9e/Carpenter_bee_on_Abelia_flowers.webm/Carpenter_bee_on_Abelia_flowers.webm.480p.webm\"",
            Example CineCer0 "video \"https://cdn.videvo.net/videvo_files/video/free/2020-05/small_watermarked/3d_ocean_1590675653_preview.webm\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 2: visualizing text
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Visualize Text"),
        (Español,"Visualizar Texto")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Evaluate the word \"text\" + the text you want to visualize inside quotation marks \"\"."),
              (Español,"Evalúa la palabra \"text\" + el texto que quieras visualizar entre comillas \"\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"Different senteces are being evaluating in the following examples. These can be as short or long as you want. Later on, we will check how to modify the appearance (position, font-size, colour, font-type, etc). To erase the text: erase eveything and evaluate the empty code-box. To change the text: replace it and evaluate again."),
              (Español,"En los ejemplos, diferentes oraciones son evaluadas. Éstas pueden ser cortar o largas. Más adelante veremos cómo modificar la apariencia (posición, tamaño, color, tipo de fuente, etc). Para borrar el texto: borra todo y evalua la caja de código vacía. Para cambiar el texto: reemplazalo y evalúa de nuevo.")
            ]],
            Example CineCer0 "text \"Hello word\"",
            Example CineCer0 "text \"Lorem ipsum dolor sit amet, consectetur adipiscing elit.\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 3: Position of both text and video
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Change position (axis X,Y) of video and text"),
        (Español,"Cambiar posición (ejes X,Y) de video y texto")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"The anchor point for both video and text is at the centre of these objects. By default, both coordenates (x,y) are in position \"0,0\" = the centre of the screen."),
              (Español,"El punto de anclaje del video y del texto es al centro de estos objetos. Por defecto, ambas coordenadas (x,y) están en posición \"0,0\" = centro de la pantalla.")
            ]],
            Example CineCer0 "\"https://github.com/jac307/videoTextures/blob/master/mariposa/20.mov?raw=true\"",
            Example CineCer0 "text \"This is my text. Este es mi texto. Ceci est mon texte. Este é o meu texto. Das ist mein Text\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Functions to change position: \"setPosX #\", \"setPosY #\" and \"setCoord # #\" --to modify both values. Values go from \"(-1)\" (top/left-corner) to \"1\" (bottom/right-corner). Negative numbers must be inside \"()\"."),
              (Español,"Funciones para cambiar posición: \"setPosX #\", \"setPosY\" # y \"setCoord # #\" --para modificar ambos valores. Los valores van de \"(-1)\" (esquina superior izquierda) a \"1\" (esquina inferior derecha). Los números negativos deben ir dentro de \"()\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"In the following examples, the position is being changed in different ways, using: \"setPosX #\", \"setCoord # #\", \"setPosY #\", or both \"setPosX #\" and \"setPosY #\" at the same time."),
              (Español,"En los ejemplos, la posición está cambiado de diferentes maneras, usando: \"setPosX #\", \"setCoord # #\", \"setPosY #\", o ambos \"setPosX #\" y \"setPosY #\" al mismo tiempo.")
            ]],
            Example CineCer0 "setPosX 0.5 $ video \"https://github.com/jac307/videoTextures/blob/master/mariposa/20.mov?raw=true\"",
            Example CineCer0 "setCoord (-0.5) 0.1 $ video \"https://github.com/jac307/videoTextures/blob/master/mariposa/20.mov?raw=true\"",
            Example CineCer0 "setPosY (-0.5) $ text \"This is my text. Este es mi texto. Ceci est mon texte. Este é o meu texto. Das ist mein Text\"",
            Example CineCer0 "setPosY (-0.3) $ text setPosX 0.8 $ \"This is my text. Este es mi texto. Ceci est mon texte. Este é o meu texto. Das ist mein Text\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 4: video-size
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: modify size"),
        (Español,"Video: modificar el tamaño")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"The default values for video-size are: \"width=1\" and \"height=1\". These plays the video at its natural aspect-ratio, with the video-height fitting the screen-height."),
              (Español,"Los valores por defecto para el tamaño de video son: \"ancho=1\" and \"alto=1\". Éstos reproducen el video es su aspec-ratio (relación de aspecto) natural; eso con la altura del video siendo la misma que la altura de la pantalla.")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Functions to change size: \"setWidth #\", \"setHeight #\", and \"setSize #\" --requires just one value (affects proportionally width and height). Values bellow \"1\" make the video smaller (can down up to \"0\"). Values above \"1\" makes the video bigger."),
              (Español,"Funciones para cambiar tamaño: \"setWidth #\", \"setHeight #\", y \"setSize #\" --requiere únicamente de un valor (afecta proporcionalmente ancho y alto). Los valores por debajo de \"1\" hacen el video más pequeño (puede reducirse hasta \"0\"). Los valores arriba de \"1\" hacen el video más grande.")
            ]],
            Example CineCer0 "setWidth 0.2 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Example CineCer0 "setHeight 2.1 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Example CineCer0 "setSize 0.8 $ \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 5: text-size
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: modify font-size"),
        (Español,"Text: modificar el tamaño de fuente")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"By default, the value of font-size is \"1\". This is equivalent to the normal size of the editor's text in a session of Estuary"),
              (Español,"Por defecto, el valor de tamaño de la fuente es \"1\". Ésto equivale al tamaño del texto de los editores de una sesión de Estuary")
            ]],
            Example CineCer0 "text setPosX 0.8 $ \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Function to change font-size: \"fontSize 10 #\". Values bellow \"1\" make the text smaller (can down up to \"0\"). Values above \"1\" makes the text bigger."),
              (Español,"Función para cambiar el tamño de fuente: \"fontSize 10 #\". Los valores por debajo de \"1\" hacen el texto más pequeño (puede reducirse hasta \"0\"). Los valores arriba de \"1\" hacen el texto más grande.")
            ]],
            Example CineCer0 "fontSize 15 $ text setPosX 0.8 $ \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Example CineCer0 "fontSize 100 $ text setPosX 0.8 $ \"My text. Mi texto. Mon texte. Meu texto. Mein Text\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 6: multiple functions
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Combining functions"),
        (Español,"Combinar funciones")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Functions can be combine to create multiple transformations in either video or text. One of the options for this combination is to add teh symbol \"$\" between functions; more options will be checked later."),
              (Español,"Las funciones se pueden combinar para creat multiples transformationes ya sea en el video o en el texto. Una de las opciones para combinar es agregar el símbolo \"$\" entre funciones; se revisarán otras opciones más adelante.")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"In this example, position in axis-X and size are combined to modify the apperance of a video:"),
              (Español,"En este ejemplo, la posición en el eje-X y el tamaño se combinaron para modificar la apariencia del video:")
            ]],
            Example CineCer0 "setPosX 0.5 $ setSize 0.5 $ video \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Paragraph [ Text $ Map.fromList[
              (English,"In this example, the coordenates (in axis X,Y) and the font-size are combined to modify the apperance of text:"),
              (Español,"En este ejemplo, las coordenadas (en los ejes X,Y) y el tamaño de fuente son combinadas para modificar la apariencia del texto:")
            ]],
            Example CineCer0 "setCoord 0 (-0.5) $ fontSize 2.5 $ text \"Mentira es la palabra. La palabra mentira,\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 7 to 13: Video filters
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: volume"),
        (Español,"Video: volumen")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"By default, audio gain of a video is set to \"0\". If your video has audio, \"vol #\" is the function to modify the gain. Values can go up to \"1\", which it is the natural audio gain of the video."),
              (Español,"Por defecto, la ganancia de sonido de un video es \"0\". Si tu video tiene audio, \"vol #\" es la función para modificar su ganancia. Los valores puede ir hasta \"1\", que sería la ganancia natural del video.")
            ]],
            Example CineCer0 "vol 0.5 $ \"https://github.com/jac307/videoTextures/blob/master/walter/amor.mov?raw=true\"",
            Example CineCer0 "vol 1 $ \"https://github.com/jac307/videoTextures/blob/master/walter/horoscopo/leo.mov?raw=true\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 8
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: opacity"),
        (Español,"Video: opacidad")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setOpacity #\" -- Default value: \"1\"."),
              (Español,"\"setOpacity #\" -- Valor por default: \"1\".")
            ]],
            Example CineCer0 "\"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/cigarras.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Modifying the value of opacity changes how opaque (visible) a video is (in relation to what is on the background). The value goes from \"1\" (totally visible) to \"0\" (invisible), everything in the middle will produce a transparency. In the following examples the opacity is modify. Since the background is black, you will notice a change in the intensity of the video."),
              (Español,"Cuando se modifica el valor de la opacidad, se cambia que tan opaco (visible) es el video (con relación a su fondo). El valor va de \"1\" (totalmente visible) to \"0\" (invisible), cualquier cosa en el medio producirá una transparencia. En los siguientes ejemplos se modifica la opacidad. Ya que el fondo es negro, notarás un cambio en la intensidad del video.")
            ]],
            Example CineCer0 "setOpacity 0.6 $ \"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/cellos.mov?raw=true\"",
            Example CineCer0 "setOpacity 0.2 $ \"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/cellos.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,"In this third example, two videos are playing at the same position. The one of the top has an opacity (or transparency) of 0.5. The result is a combination of both videos:"),
              (Español,"En este tercer ejemplo, dos videos son reproducidos en la misma posición. La opacidad (o transparencia) es de 0.5 en el video que se encuentra en la capa superior. El resultado es la combinación de ambos videos:")
            ]],
            Example CineCer0 "\"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/cigarras.mov?raw=true\"; setOpacity 0.5 $ \"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/cellos.mov?raw=true\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 9
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: brightness"),
        (Español,"Video: brillo")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setBrightness #\" -- Default value: \"1\"."),
              (Español,"\"setBrightness #\" -- Valor por default: \"1\".")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/40/Reaction_Diffusion_Varying.webm/Reaction_Diffusion_Varying.webm.480p.vp9.webm\"",
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "setBrightness 0.5 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/40/Reaction_Diffusion_Varying.webm/Reaction_Diffusion_Varying.webm.480p.vp9.webm\"",
            Example CineCer0 "setBrightness 2.5 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/40/Reaction_Diffusion_Varying.webm/Reaction_Diffusion_Varying.webm.480p.vp9.webm\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 10
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: blur"),
        (Español,"Video: difuminación")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setBlur #\" -- Default value: \"0\"."),
              (Español,"\"setBlur #\" -- Valor por default: \"0\".")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/4d/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm.360p.vp9.webm\"",
            Paragraph [ Text $ Map.fromList[
              (English,"unfocus"),
              (Español,"desenfocar")
            ]],
            Example CineCer0 "setBlur 20 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/4d/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm.360p.vp9.webm\"",
            Example CineCer0 "setBlur 2 $ setBrightness 1.5 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/4d/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm.360p.vp9.webm\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 11
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: contrast"),
        (Español,"Video: contraste")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setContrast #\" -- Default value: \"1\"."),
              (Español,"\"setContrast #\" -- Valor por default: \"1\".")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/0/02/Bobbing_downy_woodpecker_at_Prospect_Park.webm/Bobbing_downy_woodpecker_at_Prospect_Park.webm.480p.vp9.webm\"",
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "setContrast 0.5 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/0/02/Bobbing_downy_woodpecker_at_Prospect_Park.webm/Bobbing_downy_woodpecker_at_Prospect_Park.webm.480p.vp9.webm\"",
            Example CineCer0 "setContrast 2.5 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/0/02/Bobbing_downy_woodpecker_at_Prospect_Park.webm/Bobbing_downy_woodpecker_at_Prospect_Park.webm.480p.vp9.webm\"",
            Example CineCer0 "setContrast 1.1 $ setBrightness 1.2 $ setSize 0.6 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/0/02/Bobbing_downy_woodpecker_at_Prospect_Park.webm/Bobbing_downy_woodpecker_at_Prospect_Park.webm.480p.vp9.webm\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 12
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: saturation"),
        (Español,"Video: saturación")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setSaturate #\" -- Default value: \"1\"."),
              (Español,"\"setSaturate #\" -- Valor por default: \"1\".")
            ]],
            Example CineCer0 "\"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\"",
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "setSaturate 0.6 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\"",
            Example CineCer0 "setSaturate 3 $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\"",
            Example CineCer0 "setSaturate 12 $ setSize 0.5 $ setCoord 0 (-0.5) $ \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 13
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: grayscale"),
        (Español,"Video: escala de grises")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setGrayscale #\" -- Default value: \"0\"."),
              (Español,"\"setGrayscale #\" -- Valor por default: \"0\".")
            ]],
            Example CineCer0 "\"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/recuerdos.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "setGrayscale 0.5 $ \"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/recuerdos.mov?raw=true\"",
            Example CineCer0 "setGrayscale 1 $ setBrightness 0.5 \"https://github.com/jac307/memoriasSamples/blob/master/videoSamples/recuerdos.mov?raw=true\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 14 to 20: Text functions
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: colour"),
        (Español,"Texto: color")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"colour #\" -- Default value: \"white\"."),
              (Español,"\"colour #\" -- Valor por default: \"white\".")
            ]],
            Example CineCer0 "fontSize 4 $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "colour \"red\" $ fontSize 4 $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Example CineCer0 "colour \"blue\" $ fontSize 6 $ text \"entre silencios cojo\"",
            Example CineCer0 "colour \"magenta\" $ fontSize 4 $ setPosY (-0.4) $ text \"un ganglio de luz que se ha vuelto loco\""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 15
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: color RGB"),
        (Español,"Text: color RGB")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 16
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: color HSL"),
        (Español,"Text: color HSL")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 17
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: font type"),
        (Español,"Text: tipo de fuente")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 18
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: bold"),
        (Español,"Text: negritas")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 19
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: italics"),
        (Español,"Text: italicas")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 20
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: strike"),
        (Español,"Text: tachado")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 21 to 23: Dynamic functions
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Dynamic Functions: ramp"),
        (Español,"Funciones dinámicas: ramp")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 22
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Dynamic Functions: range"),
        (Español,"Funciones dinámicas: range")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 23
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Dynamic Functions: sin"),
        (Español,"Funciones dinámicas: sin")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 24 to 27: Video masks
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: circle mask -option 1"),
        (Español,"Video: máscara circular -opción 1")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 25
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: circle mask -option 2"),
        (Español,"Video: máscara circular -opción 2")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 26
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: square mask"),
        (Español,"Video: máscara cuadrada")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 27
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: rectangular mask"),
        (Español,"Video: máscara rectangular")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 28: Multiple statements
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Multiple statements"),
        (Español,"Declaraciones múltiples")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 29: Position of both text and video
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Change position (axis Z) of video and text"),
        (Español,"Cambiar posición (ejes Z) de video y texto")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,""),
              (Español,"")
            ]],
            Example CineCer0 "",
            Example CineCer0 "",
            Example CineCer0 ""
        ],
        CodeView 1 0
      ]
    })
    -- end
  ]
}
