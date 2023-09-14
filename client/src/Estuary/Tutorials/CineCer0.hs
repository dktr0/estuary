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
              (English,"CineCer0 is a language for displaying and transforming videos, images and text in the browser. It can be used, for example, in the performance of live coded cinema, kinetic typography, VJ-ing, etc."),
              (Español,"CineCer0 es un lenguaje para reproducir y transformar video e imágenes así como renderizar y transformar texto en el navegador. Puede ser usado, por ejemplo, para performance de live cinema con programación al vuelo, animación tipográfica, VJ-ing, etc.")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"We will review CineCer0 functions step-by-step. On each page, you will find executable examples. You can evaluate and modify them on the fly."),
              (Español,"Revisaremos las funciones de CineCer0 paso a paso. En cada página encontrarán ejemplos ejcutables. Pueden evaluarlos y modificarlos.")
            ]],
            Snippet 1 True "CineCer0" "circleMask 0.5 $ vol 0.5 $ video \"https://github.com/jac307/videoTextures/blob/master/otros/river.mov?raw=true\"",
            Snippet 1 True "CineCer0" "setSize 0.8 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/1/1d/European_honey_bee_extracts_nectar.jpg/640px-European_honey_bee_extracts_nectar.jpg\"",
            Snippet 1 True "CineCer0" "size 10 $ setPosY (-0.8) $ text \"I am a river\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --importing video
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Play Video"),
        (Español,"Reproducir Video")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Evaluate the word \"video\" + the URL of a video inside quotation marks \"\"."),
              (Español,"Evalúa la palabra \"video\" + la URL de un video dentro de comillas \"\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"The URL for videos must point directly to a video file that the web browser can play (not a URL of a video streaming service). Often, such URLs would end in \".mov\", \".mp4\" or \".m4v\"."),
              (Español,"La URL de los videos debe direccionar al archivo de video que el navegador puede reproducir (no a la URL de un video que es parte de un servidor de streaming). Usualmente, estas URLs terminan en \".mov\", \".mp4\" or \".m4v\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"The following examples are using the two different methods to import/play videos. Later on, we will modify the appearance (position, size, blur, brightness, etc). To erase the video: erase everything and evaluate the empty code-box. To change the video: replace the URL and evaluate again."),
              (Español,"En los estos ejemplos se están usando ambos métodos para importar/reproducir videos. Más adelante veremos cómo modificar la apariencia (posición, tamaño, blur, brillo, etc). Para borrar el video: borra todo y evalua la caja de código vacía. Para cambiar el video: reemplaza la url y evalúa de nuevo.")
            ]],
            Snippet 1 True "CineCer0" "video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/9/9e/Carpenter_bee_on_Abelia_flowers.webm/Carpenter_bee_on_Abelia_flowers.webm.480p.webm\"",
            Snippet 1 True "CineCer0" "video \"https://cdn.videvo.net/videvo_files/video/free/2020-05/small_watermarked/3d_ocean_1590675653_preview.webm\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --importing image
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Play Image"),
        (Español,"Reproducir Imagen")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Evaluate the word \"image\" + the URL of a video inside quotation marks \"\"."),
              (Español,"Evalúa la palabra \"image\" + la URL de un video dentro de comillas \"\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"The URL for images must point directly to an image file that the web browser can play. Often, such URLs would end in \".jpg\", \".jpeg\" or \".png\"."),
              (Español,"La URL de las imágenes debe direccionar al archivo de la imagen que el navegador puede reproducir. Usualmente, estas URLs terminan en \".jpg\", \".jpeg\" o \".png\".")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"The following examples are using the two different methods to import/play images. Later on, we will modify the appearance (position, size, blur, brightness, etc). To erase the image: erase everything and evaluate the empty code-box. To change the video: replace the URL and evaluate again."),
              (Español,"En los estos ejemplos se están usando ambos métodos para importar/reproducir imágenes. Más adelante veremos cómo modificar la apariencia (posición, tamaño, blur, brillo, etc). Para borrar la imagen: borra todo y evalua la caja de código vacía. Para cambiar el video: reemplaza la url y evalúa de nuevo.")
            ]],
            Snippet 1 True "CineCer0" "image \"https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Edinburgh_Union_Canal_SR.jpg/1024px-Edinburgh_Union_Canal_SR.jpg\"",
            Snippet 1 True "CineCer0" "image \"https://upload.wikimedia.org/wikipedia/commons/f/fe/Royal_emblem_of_Paramara.jpg\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --visualizing text
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
              (English,"The string of text you evaluate can be as short or long as you want. Later on, we will check how to modify the appearance (position, font size, colour, font type, etc). To erase the text: erase everything and evaluate the empty code-box. To change the text: replace it and evaluate again."),
              (Español,"El string de texto que se evalúa pueden ser corto o largo. Más adelante veremos cómo modificar la apariencia (posición, tamaño, color, tipo de fuente, etc). Para borrar el texto: borra todo y evalua la caja de código vacía. Para cambiar el texto: reemplazalo y evalúa de nuevo.")
            ]],
            Snippet 1 True "CineCer0" "text \"Hello word\"",
            Snippet 1 True "CineCer0" "text \"This is my text - Este es mi texto\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --Position of both text and video
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Change position (axis X,Y) of video, image and text"),
        (Español,"Cambiar posición (ejes X,Y) de video, imagen y texto")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"The anchor point for both video and text is at the centre of these objects. By default, both coordinates (x,y) are in position \"0,0\" = the centre of the screen."),
              (Español,"El punto de anclaje del video y del texto es al centro de estos objetos. Por defecto, ambas coordenadas (x,y) están en posición \"0,0\" = centro de la pantalla.")
            ]],
            Snippet 1 True "CineCer0" "video \"https://github.com/jac307/videoTextures/blob/master/mariposa/20.mov?raw=true\"",
            Snippet 1 True "CineCer0" "text \"This is my text. Este es mi texto. Ceci est mon texte. Este é o meu texto. Das ist mein Text\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Functions to change position: \"setPosX #\", \"setPosY #\" and \"setCoord # #\" --to modify both values. Values go from \"(-1)\" (top/left-corner) to \"1\" (bottom/right-corner). Negative numbers must be inside \"()\"."),
              (Español,"Funciones para cambiar posición: \"setPosX #\", \"setPosY\" # y \"setCoord # #\" --para modificar ambos valores. Los valores van de \"(-1)\" (esquina superior izquierda) a \"1\" (esquina inferior derecha). Los números negativos deben ir dentro de \"()\".")
            ]],
            Snippet 1 True "CineCer0" "setPosX 0.5 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/a/a5/Red-flowers-at-cerro-pelon.jpg/800px-Red-flowers-at-cerro-pelon.jpg\"",
            Snippet 1 True "CineCer0" "setCoord 0.2 (-0.5) $ text \"This is my text. Este es mi texto. Ceci est mon texte. Este é o meu texto. Das ist mein Text\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --video-size
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: modify size"),
        (Español,"Video/Imagen: modificar el tamaño")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"The default values for video/image-size are: \"width=1\" and \"height=1\". These plays the video/image at its natural aspect-ratio, with the video-height fitting the screen-height."),
              (Español,"Los valores por defecto para el tamaño de video/imagen son: \"ancho=1\" and \"alto=1\". Éstos reproducen el video/imagen es su aspec-ratio (relación de aspecto) natural; eso con la altura del video siendo la misma que la altura de la pantalla.")
            ]],
            Snippet 1 True "CineCer0" "video \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Functions to change size: \"setWidth #\", \"setHeight #\", and \"setSize #\" --requires just one value (affects proportionally width and height). Values bellow \"1\" make the video smaller (can down up to \"0\"). Values above \"1\" makes the video bigger."),
              (Español,"Funciones para cambiar tamaño: \"setWidth #\", \"setHeight #\", y \"setSize #\" --requiere únicamente de un valor (afecta proporcionalmente ancho y alto). Los valores por debajo de \"1\" hacen el video más pequeño (puede reducirse hasta \"0\"). Los valores arriba de \"1\" hacen el video más grande.")
            ]],
            Snippet 1 True "CineCer0" "setHeight 4.1 $ video \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Snippet 1 True "CineCer0" "setSize 0.8 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/b/ba/Tricyrtis_hirta_-_blossom_side_%28aka%29.jpg/640px-Tricyrtis_hirta_-_blossom_side_%28aka%29.jpg\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --text-size
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
            Snippet 1 True "CineCer0" "text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Function to change font size: \"size 3 #\". Values bellow \"1\" make the text smaller (can down up to \"0\"). Values above \"1\" makes the text bigger."),
              (Español,"Función para cambiar el tamño de fuente: \"size 2 #\". Los valores por debajo de \"1\" hacen el texto más pequeño (puede reducirse hasta \"0\"). Los valores arriba de \"1\" hacen el texto más grande.")
            ]],
            Snippet 1 True "CineCer0" "size 2 $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Snippet 1 True "CineCer0" "size 4 $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --Multiple functions
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Combining functions 1"),
        (Español,"Combinar funciones 1")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Functions can be combined to create multiple transformations in either video or text. One of the options for this combination is to add the symbol \"$\" between functions; more options will be checked later."),
              (Español,"Las funciones se pueden combinar para creat multiples transformationes ya sea en el video o en el texto. Una de las opciones para combinar es agregar el símbolo \"$\" entre funciones; se revisarán otras opciones más adelante.")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"In this example, position in axis-X and size are combined to modify the appearance of a video:"),
              (Español,"En este ejemplo, la posición en el eje-X y el tamaño se combinaron para modificar la apariencia del video:")
            ]],
            Snippet 1 True "CineCer0" "setPosX 0.5 $ setSize 0.5 $ video \"https://upload.wikimedia.org/wikipedia/commons/9/9a/Time_Lapse_of_New_York_City.ogv\"",
            Paragraph [ Text $ Map.fromList[
              (English,"In this example, the coordinates (in axis X,Y) and the font size are combined to modify the appearance of text:"),
              (Español,"En este ejemplo, las coordenadas (en los ejes X,Y) y el tamaño de fuente son combinadas para modificar la apariencia del texto:")
            ]],
            Snippet 1 True "CineCer0" "setCoord 0 (-0.5) $ size 2.5 $ text \"Mentira es la palabra. La palabra mentira,\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --Video Audio
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: volume"),
        (Español,"Video: volumen")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"By default, the audio gain of a video is set to \"0\". If your video has audio, \"vol #\" is the function to modify the gain. Values can go up to \"1\", which is the natural audio gain of the video."),
              (Español,"Por defecto, la ganancia de sonido de un video es \"0\". Si tu video tiene audio, \"vol #\" es la función para modificar su ganancia. Los valores puede ir hasta \"1\", que sería la ganancia natural del video.")
            ]],
            Snippet 1 True "CineCer0" "vol 0.3 $ video \"https://github.com/jac307/videoTextures/blob/master/otros/street.mov?raw=true\"",
            Snippet 1 True "CineCer0""vol 0.7 $ video \"https://github.com/jac307/videoTextures/blob/master/otros/colombia.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --Video and Image Rotate
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: rotation"),
        (Español,"Video/Imagen: rotación")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setRotate #\" -- Default value: \"0\", value on degrees."),
              (Español,"\"setRotate #\" -- Valor por default: \"0\", valor en grados.")
            ]],
            Snippet 1 True "CineCer0" "image \"https://upload.wikimedia.org/wikipedia/commons/thumb/7/7c/Leaf_epidermis.jpg/640px-Leaf_epidermis.jpg\"",
            Snippet 1 True "CineCer0" "setRotate 50 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/7/7c/Leaf_epidermis.jpg/640px-Leaf_epidermis.jpg\"",
            Snippet 1 True "CineCer0" "setRotate 180 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/7/7c/Leaf_epidermis.jpg/640px-Leaf_epidermis.jpg\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --Video and Image filters
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: opacity"),
        (Español,"Video/Imagen: opacidad")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setOpacity #\" -- Default value: \"1\"."),
              (Español,"\"setOpacity #\" -- Valor por default: \"1\".")
            ]],
            Snippet 1 True "CineCer0" "video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/puertas.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Modifying the value of opacity changes how opaque (visible) is a video. The value goes from \"1\" (totally visible) to \"0\" (invisible), everything in the middle will produce a transparency. In the following examples the opacity is modify. Since the background is black, you will notice a change in the intensity of the video."),
              (Español,"Cuando se modifica el valor de la opacidad, se cambia que tan opaco (visible) es el video (con relación a su fondo). El valor va de \"1\" (totalmente visible) to \"0\" (invisible), cualquier cosa en el medio producirá una transparencia. En los siguientes ejemplos se modifica la opacidad. Ya que el fondo es negro, notarás un cambio en la intensidad del video.")
            ]],
            Snippet 1 True "CineCer0" "setOpacity 0.7 $ video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/puertas.mov?raw=true\"",
            Snippet 1 True "CineCer0" "setOpacity 0.2 $ video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/puertas.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 9
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: brightness"),
        (Español,"Video/Imagen: brillo")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setBrightness #\" -- Default value: \"1\"."),
              (Español,"\"setBrightness #\" -- Valor por default: \"1\".")
            ]],
            Snippet 1 True "CineCer0" "image \"https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Edinburgh_Union_Canal_SR.jpg/1024px-Edinburgh_Union_Canal_SR.jpg\"",
            Paragraph [ Text $ Map.fromList[
              (English,"If the value is less than \"1\", the video will appear less bright; it can go down until \"0\" (totally black). If the value is higher than \"1\", the video will increase its brightness until becoming completly white (this top value depends on the colour of each video)."),
              (Español,"Si el valor es menor a \"1\", el video aparecerá con menos brillo; este valor puede bajar hasta \"0\" (totalmente negro). Si el valor es mayor a \"1\", el video incrementará su brillo hasta convertirse en totalmente blanco (el valor máximo depende del color de cada video).")
            ]],
            Snippet 1 True "CineCer0" "setBrightness 0.5 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Edinburgh_Union_Canal_SR.jpg/1024px-Edinburgh_Union_Canal_SR.jpg\"",
            Snippet 1 True "CineCer0" "setBrightness 2.5 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Edinburgh_Union_Canal_SR.jpg/1024px-Edinburgh_Union_Canal_SR.jpg\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 10
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: blur"),
        (Español,"Video/Imagen: difuminación")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setBlur #\" -- Default value: \"0\" (focused)."),
              (Español,"\"setBlur #\" -- Valor por default: \"0\" (enfocado).")
            ]],
            Snippet 1 True "CineCer0" "video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/4d/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm.360p.vp9.webm\"",
            Paragraph [ Text $ Map.fromList[
              (English,"This function unfocus the video. The value can increase from \"0\"; the top value of a very unfocused video depends on the qualities of each video."),
              (Español,"Esta función desenfoca el video. El valor puede incrementar desde \"0\"; el valor máximo para que un video esté completamente desenfocado depende de las características de cada video.")
            ]],
            Snippet 1 True "CineCer0" "setBlur 20 $ video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/4d/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm.360p.vp9.webm\"",
            Snippet 1 True "CineCer0""setBlur 2 $ video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/4/4d/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm/Baha%27i_Temple_--_Wilmette_%2C_IL_--_Drone_Video_%28DJI_Spark%29.webm.360p.vp9.webm\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 11
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: contrast"),
        (Español,"Video/Imagen: contraste")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setContrast #\" -- Default value: \"1\"."),
              (Español,"\"setContrast #\" -- Valor por default: \"1\".")
            ]],
            Snippet 1 True "CineCer0" "image \"https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/Monarch-at-cerro-pelon.jpg/800px-Monarch-at-cerro-pelon.jpg\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Values between \"0\" and \"1\" creates less contrast in a video: the dark and light colours will reduce their intensity. Values over \"1\" increase contrast: dark colours will be darker, light colours will be brighter."),
              (Español,"Los valores entre \"0\" y \"1\" crean un contraste menor en el video: los colores oscuros y claros reducen su intensidad. Los valores sobre \"1\" incrementan el contraste: los colores oscuros serán más oscuros, los colores claros serán más brillosos.")
            ]],
            Snippet 1 True "CineCer0" "setContrast 0.5 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/Monarch-at-cerro-pelon.jpg/800px-Monarch-at-cerro-pelon.jpg\"",
            Snippet 1 True "CineCer0" "setContrast 2.5 $ image \"https://upload.wikimedia.org/wikipedia/commons/thumb/b/b2/Monarch-at-cerro-pelon.jpg/800px-Monarch-at-cerro-pelon.jpg\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 12
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: saturation"),
        (Español,"Video/Imagen: saturación")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setSaturate #\" -- Default value: \"1\"."),
              (Español,"\"setSaturate #\" -- Valor por default: \"1\".")
            ]],
            Snippet 1 True "CineCer0" "video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\"",
            Paragraph [ Text $ Map.fromList[
              (English,"If the value is less than \"1\", the colours will look more opaque; if the value is \"0\", the video will be in greyscale. If the value is higher than \"1\", the colours will have more intensity. Very high values will convert the range of colours of a video to pure colour (this results in video with primary colours only)."),
              (Español,"Si el valor es menor a \"1\", los colores se verán más opacos; si el valor es \"0\", el video se verá en escala de grises. Si el valor es mayor a \"1\", los colores tendrán más intensidad. Valores muy altos convertirán el rango de colores de un video a colores puros (esto resulta es un video únicamente con colores primarios).")
            ]],
            Snippet 1 True "CineCer0" "setSaturate 0.6 $ video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\"",
            Snippet 1 True "CineCer0" "setSaturate 3 $ video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 13
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video/Image: grayscale"),
        (Español,"Video/Imagen: escala de grises")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"setGrayscale #\" -- Default value: \"0\"."),
              (Español,"\"setGrayscale #\" -- Valor por default: \"0\".")
            ]],
            Snippet 1 True "CineCer0" "video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/recuerdos.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,"The value goes from \"0\" to \"1\", affecting the colours to convert the video into full grayscale."),
              (Español,"Los valores van de \"0\" to \"1\", afectando los colores para convertir el video completamente escala de grises.")
            ]],
            Snippet 1 True "CineCer0" "setGrayscale 0.5 $ video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/recuerdos.mov?raw=true\"",
            Snippet 1 True "CineCer0" "setGrayscale 1 $ video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/recuerdos.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 15: multiple functions 2
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Combining functions 2"),
        (Español,"Combinar funciones 2")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Examples with multiple functions affecting a the visualization of a video:"),
              (Español,"Ejemplos con múltiples funciones afectando la visualización de un video:")
            ]],
            Snippet 1 True "CineCer0" "setContrast 1.1 $ setBrightness 1.2 $ setSize 0.6 $ video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/0/02/Bobbing_downy_woodpecker_at_Prospect_Park.webm/Bobbing_downy_woodpecker_at_Prospect_Park.webm.480p.vp9.webm\"",
            Snippet 1 True "CineCer0" "setSaturate 12 $ setSize 0.5 $ setCoord 0 (-0.5) $ video \"https://upload.wikimedia.org/wikipedia/commons/transcoded/5/54/04-European_Robin-nX-1.webm/04-European_Robin-nX-1.webm.480p.webm\"",
            Snippet 1 True "CineCer0" "setBrightness 0.5 $ setGrayscale 1 $ setSize 0.8 $ setPosX 0.3 $ vol 0.3 $ video \"https://github.com/jac307/videoTextures/blob/master/otros/street.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 15 to 19: Text functions
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: colour"),
        (Español,"Texto: color")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"colour name\" -- Default color: \"white\""),
              (Español,"\"colour name\" -- Color por defecto: \"white\"")
            ]],
            Snippet 1 True "CineCer0" "size 4 $ setPosY (-0.4) $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Paragraph [ Text $ Map.fromList[
              (English,"This function accepts some words (inside \"\") in English assigned to colours like \"red\" or \"magenta\". Also accepts hex-colour codes."),
              (Español,"Esta función acepta palabras (dentro de \"\") en Inglés que describen colores como \"red\" o \"magenta\". También acepta códigos de colores hexa.")
            ]],
            Snippet 1 True "CineCer0" "colour \"magenta\" $ setPosY (-0.4) $ size 4 $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\"",
            Snippet 1 True "CineCer0" "colour \"#ff8000\" $ setPosY (-0.4) $ size 4 $ text \"My text. Mi texto. Mon texte. Meu texto. Mein Text\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 16
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: color RGB"),
        (Español,"Text: color RGB")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"rgb # # #\" -- Default values: \"1 1 1\" = white"),
              (Español,"\"rgb # # #\" -- Valores por defecto: \"1 1 1\" = white")
            ]],
            Snippet 1 True "CineCer0" "size 4 $ setPosY (-0.4) $ setPosY (-0.4) $ text \"un ganglio de luz que se ha vuelto loco\"",
            Paragraph [ Text $ Map.fromList[
              (English,"This function works with the RGB colour system, setting the value of each channel: Red, Green, and Blue. It goes from \"0\" = no colour, to \"1\" = full colour."),
              (Español,"Esta función trabaja con el sistema de color RGB, indicando el valor de cada canal: Red=rojo, Green=verde y Blue=azul. Va de \"0\" = sin color, a \"1\" = todo el color.")
            ]],
            Snippet 1 True "CineCer0" "rgb 0.7 0 0.9 $ size 4 $ setPosY (-0.4) $ text \"un ganglio de luz que se ha vuelto loco\"",
            Snippet 1 True "CineCer0""rgb 0 0 1 $ size 4 $ setPosY (-0.4) $ text \"un ganglio de luz que se ha vuelto loco\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 17
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: color HSL"),
        (Español,"Text: color HSL")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"hsl # # #\" -- Default values: \"1 1 1\" = white"),
              (Español,"\"hsl # # #\" -- Valores por defecto: \"1 1 1\" = white")
            ]],
            Snippet 1 True "CineCer0" "size 4 $ setPosY (-0.4) $ text \"Pourquoi un chapeau ferait-il peur?\"",
            Paragraph [ Text $ Map.fromList[
              (English,"Based on the colour wheel, this function works with the HSL colour system, setting the value of each channel: Hue (indicates the colour variation between red, green, and blue), Saturation, and Lightness. Each value goes from \"0\" to \"1\"."),
              (Español,"Basada en el círculo cromático, esta función trabaja con el sistema de color HSL, indicando el valor de: Hue=matiz (indica el color respecto a la combinación de rojo, verde y azul), Saturation=saturación y Lightness=brillo. Cada valor va de \"0\" a \"1\".")
            ]],
            Snippet 1 True "CineCer0" "hsl 1 1 0.7 $ size 4 $ setPosY (-0.4) $ text \"Pourquoi un chapeau ferait-il peur?\"",
            Snippet 1 True "CineCer0" "hsl 0 0.3 0.7 $ size 4 $ setPosY (-0.4) $ text \"Pourquoi un chapeau ferait-il peur?\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 18
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: font type"),
        (Español,"Text: tipo de fuente")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"font name\" -- Default font: same as browser"),
              (Español,"\"font name\" -- Fuente por default: same as browser")
            ]],
            Snippet 1 True "CineCer0" "size 4 $ setPosY (-0.4) $ text \"He walked across stepping carefully among the wounded\"",
            Paragraph [ Text $ Map.fromList[
              (English,"This function accepts a \"string\" of the name of the font. The availability of fonts depends on different systems and browser configurations."),
              (Español,"Esta función acepta un \"string\" (línea-de-texto) con el nombre de la fuente. La disponibilidad de las fuentes depende de cada sistema y las configuraciones del browser")
            ]],
            Snippet 1 True "CineCer0" "font \"Arial\" $ size 4 $ setPosY (-0.4) $ text \"He walked across stepping carefully among the wounded\"",
            Snippet 1 True "CineCer0" "font \"Impact\" $ size 4 $ setPosY (-0.4) $ text \"He walked across stepping carefully among the wounded\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 19
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Text: bold, italic, strike, & border"),
        (Español,"Text: negritas, italica, tachado y encuadrado")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"By default, all of these options are dissabled:"),
              (Español,"Por defecto, todas estas opciones están deshabilitadas:")
            ]],
            Snippet 1 True "CineCer0" "size 4 $ setPosY (-0.5) $ text \"rodzaje, urodzaje widzi, rozpoznaje\"",
            Paragraph [ Text $ Map.fromList[
              (English,"."),
              (Español,"Las funciones \"bold\" (negritas), \"italic\" (italicas), \"strike\" (tachado), y/o \"border\" (encuadrado), solamente producen efectos en el texto.")
            ]],
            Snippet 1 True "CineCer0" "bold $ size 4 $ setPosY (-0.5) $ text \"rodzaje, urodzaje widzi, rozpoznaje\"",
            Snippet 1 True "CineCer0" "italic $ size 4 $ setPosY (-0.5) $ text \"rodzaje, urodzaje widzi, rozpoznaje\"",
            Snippet 1 True "CineCer0" "strike $ size 4 $ setPosY (-0.5) $ text \"rodzaje, urodzaje widzi, rozpoznaje\"",
            Snippet 1 True "CineCer0" "border $ size 4 $ setPosY (-0.5) $ text \"rodzaje, urodzaje widzi, rozpoznaje\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 20: Combining functions 3
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Combining functions 3"),
        (Español,"Combinar funciones 3")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Examples with multiple functions affecting a the visualization of a text:"),
              (Español,"Ejemplos con múltiples funciones afectando la visualización de un text:")
            ]],
            Snippet 1 True "CineCer0" "font \"Didot\" $ colour \"#ff60a8\" $ size 3 $ setPosY (-0.6) $ italic $ text \"Hay besos que calcinan y que hieren, hay besos que arrebatan los sentidos\"",
            Snippet 1 True "CineCer0" "bold $ hsl 0.1 1 0.5 $ size 2 $ text \"That lofty monarch, Monarch Mind\"",
            Snippet 1 True "CineCer0" "font \"Georgia\" $ border $ size 3 $ setCoord 0.6 (-0.5) $ text \"sobald ich die Melone aufschneide\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 21 to 24: Video masks
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: circle mask -option 1"),
        (Español,"Video: máscara circular -opción 1")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"circleMask #\" accepts one value: diameter. The anchor point is at the centre. The value goes from \"0\" (no mask) to \"0.99\" (a very small mask)."),
              (Español,"\"circleMask #\" acepta un valor: diámetro. El punto de anclaje se encuentra al centro.El valor va de \"0\" (sin máscara) a \"0.99\" (una máscara muy pequeña).")
            ]],
            Snippet 1 True "CineCer0" "circleMask 0.3 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/hogweed.mov?raw=true\"",
            Snippet 1 True "CineCer0" "circleMask 0.92 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/hogweed.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 22
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: circle mask -option 2"),
        (Español,"Video: máscara circular -opción 2")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"circleMask' # # #\" accepta a three parameters: diameter, and X,Y coordenates of the anchor point. These two new values go from \"0\" (left/top) to \"1\" (right/bottom)."),
              (Español,"\"circleMask' # # #\" acepta tres parámetros: diámetro, y las coordenadas en X,Y del punto de anclaje. Estos dos nuevos parámetros van de \"0\" (izquierda/arriba) to \"1\" (derecha/abajo).")
            ]],
            Snippet 1 True "CineCer0" "circleMask' 0.3 0.2 0.5 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/thorns.mov?raw=true\"",
            Snippet 1 True "CineCer0" "circleMask' 0.7 0.7 0.3 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/thorns.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 23
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: square mask"),
        (Español,"Video: máscara cuadrada")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"sqrMask #\" accepts one value: size (the results depend on the natural aspect ratio of each video). The anchor point is at the centre. The value goes from \"0\" (no mask) to \"0.99\" (a very small mask)."),
              (Español,"\"sqrMask #\" acepta un valores: tamaño (el resultado depende de la proporción/aspect-ratio natural del video). El punto de anclaje se encuentra al centro. El valor va de \"0\" (sin máscara) a \"0.99\" (una máscara muy pequeña).")
            ]],
            Snippet 1 True "CineCer0" "sqrMask 0.5 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/flags-shadows.MOV?raw=true\"",
            Snippet 1 True "CineCer0" "sqrMask 0.83 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/flags-shadows.MOV?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 24
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Video: rectangular mask"),
        (Español,"Video: máscara rectangular")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"rectMask # # # #\" accepts four values in relation the to sides of a rectangle: top right bottom left. Each value goes from \"0\" to \"0.99\" and reduces the size in the opposite direction."),
              (Español,"\"rectMask # # # #\" acepta cuatro valores en relación a los lados de un rectángulo: arriba derecha abajo izquierda. Cada valor va de \"0\" a \"0.99\" y reduce el tamaño en la dirección contraria.")
            ]],
            Snippet 1 True "CineCer0" "rectMask 0.5 0 0 0 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/spencer-creek.mov?raw=true\"",
            Snippet 1 True "CineCer0" "rectMask 0 0.3 0 0 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/spencer-creek.mov?raw=true\"",
            Snippet 1 True "CineCer0" "rectMask 0 0 0.1 0 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/spencer-creek.mov?raw=true\"",
            Snippet 1 True "CineCer0" "rectMask 0 0 0 0.9 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/spencer-creek.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 25: Combining functions 4
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Combining functions 4"),
        (Español,"Combinar funciones 4")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Examples with multiple functions affecting a the visualization of a video:"),
              (Español,"Ejemplos con múltiples funciones afectando la visualización de un video:")
            ]],
            Snippet 1 True "CineCer0" "setSaturate 3.2 $ setContrast 1.15 $ rectMask 0 0.25 0 0.45 $ setPosX 0.4 $ size 1.2 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/spencer-creek.mov?raw=true\"",
            Snippet 1 True "CineCer0" "setGrayscale 1 $ setBrightness 0.6 $ setBlur 5.5 $ circleMask 0.3 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/musgo.mov?raw=true\"",
            Snippet 1 True "CineCer0" "setCoord 0.65 0.4 $ setContrast 1.2 $ sqrMask 0.5 $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/stem.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 26 to 28: Dynamic functions
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Dynamic functions as values: ramp"),
        (Español,"Funciones dinámicas como valores: ramp")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"ramp # # #\" accepts three values: Dur_In_Cycles Initial_Value End_Value. This function can be used in instead of a single value. It produces a one-time animation."),
              (Español,"\"ramp # # #\" acepta tres valores: Dur_En_Ciclos Valor_Inicial Valor_final. Esta función puede ser usada como reemplazo de un valor. Produce una animación única.")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"In this example, 10 cicles is the duration of the animation, vol=0 is the initial value, and vol=1 is the end value:"),
              (Español,"En este ejemplo, 10 ciclos es la duración de la animación, vol=0 es el valor inicial, vol=1 es el valor final:")
            ]],
            Snippet 1 True "CineCer0" "setSaturate (ramp 10 0 5) $ video \"https://github.com/jac307/videoTextures/blob/master/otros/colombia.mov?raw=true\"",
            Paragraph [ Text $ Map.fromList[
              (English,"In the second example, \"ramp\" is applied to two parametros: the value of red and the value of blue:"),
              (Español,"En el segundo ejemplo, se aplica \"ramp\" a dos parametros: el valor de rojo y el valor de azul:")
            ]],
            Snippet 1 True "CineCer0" "rgb (ramp 10 0 1) 0 (ramp 10 1 0) $ size 4 $ text \"Pourquoi un chapeau ferait-il peur?\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 27
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Dynamic functions as values: sin"),
        (Español,"Funciones dinámicas como valores: sin")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"sin #\" = signal. Accepts one value: frequency."),
              (Español,"\"sin #\" = señal. Acepta un valor: frequencia.")
            ]],
            Paragraph [ Text $ Map.fromList[
              (English,"Modify the value periodically depending on the frequency."),
              (Español,"Modifica el valor periódicamente dependiendo de la frequencia.")
            ]],
            Snippet 1 True "CineCer0" "setCoord (sin 1) (-0.5) $ size 2.5 $ text \"Mentira es la palabra. La palabra mentira\"",
            Snippet 1 True "CineCer0" "setPosY (sin 0.1) $ size 0.5 $ video \"https://github.com/jac307/videoTextures/blob/master/otros/street.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 28
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Dynamic values: range"),
        (Español,"Valores dinámicos: range")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"\"range # #\" accepts two parameters: Value_1 Value_2. It modifies the amplitude of the signal."),
              (Español,"\"range # #\" acepta dos parámetros: Valor_1 Valor_2. Modifica la amplitud de una señal.")
            ]],
            Snippet 1 True "CineCer0" "setCoord (range (-1.4) 1.4 $ sin 0.2) (-0.5) $ size 2.5 $ text \"Mentira es la palabra. La palabra mentira\"",
            Snippet 1 True "CineCer0" "circleMask (range 1 0.5 $ sin 0.1) $ video \"https://github.com/dktr0/cybernetic-samples/blob/main/videos/hogweed.mov?raw=true\""
        ],
        CodeView 1 0 []
      ]
    }),
    -- ///////////////////////////////////////////////////////////////
    --page 29: Multiple statements
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
        (English,"Multiple statements"),
        (Español,"Declaraciones múltiples")
      ],
      tutorialPageView = GridView 1 2 [
        Views [
            Paragraph [ Text $ Map.fromList[
              (English,"Multiple statements are divided by semicolon (\";\")."),
              (Español,"Las Declaraciones múltiples están divididas por punto y coma (\";\").")
            ]],
            Snippet 1 True "CineCer0" "rectMask 0.3 0 0.3 0 $ setSize 1.3 $ video \"https://github.com/jac307/MEMORIAS/blob/master/samples/videoSamples/instrumentos.mov?raw=true\"; rgb 0.2 0 0.2 $ italic $ size 4 $ text \"rodzaje, urodzaje widzi, rozpoznaje\"",
            Snippet 1 True "CineCer0" "brightness 0.8 $ size 2 $ video \"https://github.com/jac307/videoTextures/blob/master/mariposa/10.mov?raw=true\"; opacity 0.5 $ size 2 $ brightness 0.8 $ video \"https://github.com/jac307/videoTextures/blob/master/mariposa/20.mov?raw=true\"; font \"Didot\" $ colour \"#ff60a8\" $ size 5 $ setPosY (-0.6) $ italic $ text \"and then, there is water\""
        ],
        CodeView 1 0 []
      ]
    })
    -- end
  ]
}
