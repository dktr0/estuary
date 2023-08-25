{-# LANGUAGE OverloadedStrings #-}

module Estuary.Tutorials.Metre (metreTutorial) where

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

metreTutorial :: Tutorial
metreTutorial = Tutorial {
  tutorialTitle = Map.fromList [
    (English,"Metre tutorial"),
    (Español,"Tutorial del Compás")
  ],
  tutorialPages = Seq.fromList [
    -- ///////////////////////////////////////////////////////////////
    --page 1: tutorial 
    (TutorialPage {
      tutorialPageTitle = Map.fromList [
      (English,"Metre Tutorial"),
      (Español,"Tutorial del Compás")
      ],
      tutorialPageView = GridView 1 2 [
           Views [
          
              Paragraph [ Text $ Map.fromList[
                (English,"The Metre (or Meter) widget will allow players to visualise Estuary’s tempo in contrast with metric/rhythmic subdivisions and Bjorklund patterns. This is useful for pedagogical purposes; for example, to show how a pattern relates with the elapsing cycles or to explain how Bjorklund patterns work, but it can also be used to facilitate synchronisation between instrumentalists and an Estuary ensemble."),
                (Español,"El widget de compás (Metre widget) permitirá a lxs usuarixs visualizar el tempo de Estuary en contraste con subdivisiones métricas/rítmicas y patrones Bjorklund. Esto es útil para fines pedagógicos; por ejemplo, para mostrar cómo se relaciona un patrón con los ciclos que transcurren o para explicar cómo funcionan los patrones Bjorklund, pero también se puede utilizar para facilitar la sincronización entre instrumentistas y programadores tocando en Estuary.")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English,"You can call the Metre/Meter with the command in the View system ‘metre Int’ or ‘meter Int’. For example, ‘metre 10’ will generate a metre widget that occupies the index 10 in the view system. The metre visualiser has an interface based on clickable areas; if you hoover over it you will get indications of what would happen if you click on that area. As you can see, the right will take you to the next visualiser, the left to the previous one, up in the middle will add a new subdivision, and the one down in the middle will substract a subdivision."),
                (Español,"Puede llamar al Compás con el comando en el sistema View 'metre Int'. Por ejemplo, 'metre 10' generará un widget de compás que ocupa el índice 10 en el sistema de visualización. El visualizador tiene una interfaz basada en áreas en las que se puede hacer clic; si pasas el cursor sobre el widget, obtendrás indicaciones de lo que sucedería si haces clic en esa área. Como puedes ver, el área derecha te llevará al siguiente visualizador, la izquierda al anterior, el de arriba en medio agregará una nueva subdivisión, y el de abajo en medio restará una subdivisión.")
              ]],
              Paragraph [ Text $ Map.fromList[
                (English, "The visualisers are divided into 2 categories expensive (in terms of computational resources) and (relatively) cheap. The visualisers are: circular expensive, circular cheap, rectangular expensive, rectangular cheap, bead expensive and bead cheap. The bead visualisers present a special behaviour: the middle area is divided into three (up, centre, down) instead of two (up and down). The additional central area will add a ‘k’ value to form a Euclidean pattern on top of the subdivided metre, which will act as the ‘m’. To learn more about these musical patterns see Toussaint's paper: 'The Euclidean Algorithm Generates Traditional Musical Rhythms'"
                ),
                (Español,"Los visualizadores se dividen en 2 categorías caros (en términos de recursos computacionales) y (relativamente) baratos. Los visualizadores son: circular caro, circular barato, rectangular caro, rectangular barato, perlado caro y perlado barato. Los visualizadores de perlas presentan un comportamiento especial: la zona central se divide en tres (arriba, centro, abajo) en lugar de dos (arriba y abajo). El área central adicional agregará un valor 'k' para formar un patrón euclidiano en la parte superior del metro subdividido, que actuará como la 'm'. Para obtener más información sobre estos patrones musicales, consulte el artículo de Toussaint: 'The Euclidean Algorithm Generates Traditional Musical Rhythms'")
              ]]
              ],
          MetreView 0
      ]
    })
  ]
}
