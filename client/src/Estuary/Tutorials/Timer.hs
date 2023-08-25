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
                (Español,"El widget de temporizador (timer widget) permitirá a lxs usuarixs contar intervalos de tiempo arbitrarios desde anclas temporales específicas. Esto es útil para algunas prácticas comunes de la programación al vuelo como: marcar un tiempo prescrito para un performance de programación al vuelo; comunicar con la audiencia, u otros actores, las diferentes secciones de un performance, donde se exploran diferentes intenciones; sincronizar gestos precisos en un ensamble; o cualquier otro uso que no hayamos imaginado de una (o varias) cuenta(s) regresiva(s).")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"You can call the timer with the command in the View system ‘timer Int’. For example, ‘timer 10’ will generate a timer that occupies the index 10 in the view system. The timer has an interface based on clickable areas. So, if you hoover over the timer you will get indications of what would happen if you click on that area. As you can see, the right area will change the visualisation mode, the centre up will reset/start the program, the centre down will pause or resume it and the left will flip the widget to editing mode."),
                (Español,"Es posible llamar al temporizador en el sistema View con el comando 'timer Int'. Por ejemplo, 'timer 10' generará un temporizador que ocupa el índice 10 en el sistema de visualización. El temporizador tiene una interfaz donde hay áreas en las que se puede hacer clic. Entonces, si pasas el cursor sobre el temporizador, obtendrás indicaciones de lo que sucedería si haces clic en esa área. Como puedes ver, el área de la derecha cambiará el modo de visualización, el centro hacia arriba reiniciará/iniciará el programa, el centro hacia abajo lo pausará o lo reanudará y el de la izquierda cambiará el widget al modo de edición.")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "To begin, activate the timer (by pressing the reset or resume/pause areas) and change the visualisation mode. Observe the different ways in which we are representing ellapsing time. Some have labels for each section, some are more visual, some display numbers and text. The visualisers are: progress bar with labels, progress bar, sand-clock with labels, sand-clock, numeric with label, numeric, only labels, circular, and stack."),
                (Español,"Para empezar, activa el temporizador (pulsando las zonas de reset o reanudar/pausar) y cambia el modo de visualización. Observa las diferentes formas en que estamos representando el tiempo transcurrido. Algunos tienen etiquetas para cada sección, algunos son más visuales, algunos muestran números y texto. Los visualizadores son: barra de progreso con etiquetas, barra de progreso, reloj de arena con etiquetas, reloj de arena, numérico con etiqueta, numérico, solo etiquetas, circular y apilado.")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "The stack visualiser has a special behaviour; it will show you all segments of your program at the same time and a line elapsing throughout the layers."),
                (Español,"El visualizador de 'apilado' tiene un comportamiento especial; este mostrará todos los segmentos del programa al mismo tiempo y, además, una línea que atraviesa todas las capas.")
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
                (English,"If you click on the left side of the timer display you will flip the widget into its ‘edit’ mode. Take a moment to see how different it is from the ‘display’ mode. On the top right you will see a ‘peek’ to the state of the timer display. This is useful because the flipping between ‘edit’ and ‘display’ mode is not networked; this means that some players might be still seeing the timer's display while someone else edits."),
                (Español,"Si haces clic en el lado izquierdo de la pantalla del temporizador, cambiará el widget a su modo de 'edición'. Toma un momento para ver qué tan diferente es del modo de 'display'. En la parte superior derecha, verás un vistazo ('peek') al estado de la pantalla del temporizador. Esto es útil porque el cambio entre el modo 'edit' y 'display' no está conectado en red; esto significa que es posible que algunxs usuarixs sigan viendo la pantalla del temporizador mientras otra persona edita.")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"Under the ‘peek’ area you will see an icon for loop, click on it to unloop (icon is grey in classic theme) and loop (icon is green in classic theme) the program. Under that loop icon you will see an icon of a metronome; click on it to change the time units from BPM/CPS (when icon is metronome) to seconds (when icon is clock). On the top left side you will see a botton called ‘display’ and a botton to run programs. The ‘display’ button will flip the widget back to display mode. The ▶ button will update the program as written in the text area below."),
                (Español,"Debajo del área de ‘peek’ verás un icono para el loop, haz clic en él para desloopear (el ícono aparecerá gris en el tema clásico) y loopear (el ícono se verá verde en el tema clásico) el programa. Debajo de ese ícono de loop, verás un ícono de un metrónomo; haz clic en él para cambiar las unidades de tiempo de BPM/CPS (cuando el ícono es un metrónomo) a segundos (cuando el ícono es un reloj). En la parte superior izquierda verás un botón llamado 'display' y un botón para ejecutar programas. El botón 'display' hará que el widget vuelva al modo de visualización. El botón ▶ actualizará el programa como está escrito en el área de texto a continuación descrita.")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "Compare the timer’s behaviour when modifying these different aspects: looped and unlooped as well as using BPMs as time units or seconds. Remember that, in order to start playing a program you need to flip into display mode and push the reset or pause/resume areas. There is no way to start a program in the ‘edit’ mode."),
                (Español,"Compara el comportamiento del temporizador al modificar estos diferentes aspectos: en loop y sin loop, así como al usar BPM como unidades de tiempo o segundos. Recuerda que, para comenzar a reproducir un programa, debes pasar al modo de visualización y presionar las áreas de reinicio o pausa/reanudar. No hay forma de iniciar un programa en el modo 'editar'.")
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
                (English,"In order to change a program of the timer we made a very simple text interface. The default program is: a=5, b=7, c=3. This can be broken down into: Section ‘a’ will have a duration of 5 time units; section ‘b’ will have a duration of 7 time units and section ‘c’ will have a duration of 3 time units. So, as you can see, the way to write a program is: nameOfSection1 = duration1, nameOfSection2 = duration2, etc..."),
                (Español,"Para cambiar un programa del temporizador, hemos creado una interfaz de texto muy simple. El programa predeterminado es: a=5, b=7, c=3. Este se puede desglosar en: La sección 'a' tendrá una duración de 5 unidades de tiempo; la sección ‘b’ tendrá una duración de 7 unidades de tiempo y la sección ‘c’ tendrá una duración de 3 unidades de tiempo. Entonces, la forma de escribir un programa es: nombreDeSección1 = duración1, nombreDeSección2 = duración2, etc...")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"In order to update the program, the ▶ button needs to be pressed, only then the changes will be reflected in the widget’s state and behaviour. This widget was inspired by a practice of the Cybernetic Orchestra where a prescribed set of durations (or rhythmic figures or metre) are changed in a synchronised manner by all performers. The Cybernetic Orchestra, if in need to play a similar piece, could now use this timer program: triplets = 120, quintuplets = 150, eight notes = 100, free rhythms = 60"),
                (Español,"Para actualizar el programa, se debe presionar el botón ▶, solo entonces los cambios se verán reflejados en el estado y el comportamiento del widget. Este widget se inspiró en una práctica de la Cybernetic Orchestra en la que todos los participantes cambian un conjunto prescrito de duraciones (o figuras rítmicas o métricas) de manera sincronizada. La orquesta, si necesita tocar una pieza similar, ahora podría usar este programa del temporizador: tresillos = 120, quintillos = 150, corcheas = 100, ritmos libres = 60")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "Paste or re-write the program above into the timer text interface and run it as an example of how to make your own programs. Then, make your own program that responds to your own performance ideas. :) Happy coding! "),
                (Español,"Pega o vuelve a escribir el programa anterior en la interfaz de texto del temporizador y ejecútalo como un ejemplo de cómo crear tus propios programas. Luego, crea tus propios programas que respondan a tus propias ideas de interpretación. :) ¡Feliz codigueo!")
              ]]
              ],
          TimerView 0
      ]
    })
  ]
}
