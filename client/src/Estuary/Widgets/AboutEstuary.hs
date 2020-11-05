{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.AboutEstuary (aboutEstuary) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response,link)
import Data.Map

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor
import Estuary.Types.EnsembleRequest
import Estuary.Types.View
import Estuary.Widgets.View

aboutEstuary :: MonadWidget t m => Editor t m (Event t EnsembleRequest)
aboutEstuary = viewWidget never $ Views [

  Paragraph [
    Text $ fromList [
      (English,"Estuary is a platform for collaboration and learning through live coding. It enables you to create sound, music, and visuals in a web browser. Key features include:"),
      (Español,"Estuary es una plataforma de colaboración y aprendizaje a través del la codificación en vivo (live coding). Estuary le permite crear sonidos, música y visuales en el explorador de internet. Algunas características importantes de esta plataforma son:")
      ],

    BulletPoints [
      Text $ fromList [
        (English,"built-in tutorials and reference materials"),
        (Español,"tutoriales y materiales de referencia")
      ],
      Text $ fromList [
        (English,"a growing collection of different interfaces and live coding languages"),
        (Español,"una creciente colección de diferentes interfaces y lenguajes de codificación en vivo.")
      ],
      Text $ fromList [
        (English,"support for networked ensembles (whether in the same room or distributed around the world)"),
        (Español,"soporte para ensambles en red (ya sea que esten en la misma sala o distribuidos en todo el mundo)")
      ],
      Text $ fromList [
        (English,"text localization to an expanding set of natural languages"),
        (Español,"localización de texto a un conjunto creciente de lenguajes naturales.")
      ],
      Text $ fromList [
        (English,"visual customization via themes (described by CSS)"),
        (Español,"personalización visual a través de temas (descritos por CSS).")
      ]
    ]
  ],

  Paragraph [
    Text $ fromList [
      (English,"The development of Estuary is the result of ongoing collaborative work that has been \
      \supported by two grants from Canada's Social Sciences and Humanities Research Council (SSHRC) - \
      \initially for the project \"Projectional interfaces for musical live coding\", and more recently \
      \as part of the project \"Platforms  and  practices  for networked, language-neutral live coding\". \ \Estuary builds upon, and depends on, the work of many others, including but not limited to all \
      \those who contribute to Reflex and TidalCycles. Estuary is free and open source software, released \ \ under the terms of the GNU Public License (version 3)."),
      (Español,"El desarrollo de Estuary es el resultado del trabajo colaborativo que se ha realizado \
      \apoyado por dos becas del Consejo de Investigación de Ciencias Sociales y Humanidades de Canadá (SSHRC) -\
      \inicialmente para el proyecto \"Interfaces proyectivas para la codificación musical en vivo\", y más recientemente \
      \como parte del proyecto \"Plataformas y prácticas para la codificación en vivo en red y en idioma neutral\". Estuary se construye desde del trabajo de muchos otres, incluyendo pero no limitado a todes \
      \aquellos que contribuyen a Reflex y TidalCycles. Estuary es un software gratuito y de código abierto, publicado \ \ bajo los términos de la Licencia Pública GNU (versión 3).")
      ]
    ],

  Paragraph [
    Text $ fromList [
      (English,"Some places where further help/discussion/resources/etc about Estuary are available include:")
      ],
    BulletPoints [
      Views [ Text $ fromList [
        (English,"The Estuary Google group at ")
        ],
        link "https://groups.google.com/forum/#!forum/estuary"],
      Views [ Text $ fromList [
        (English,"The Estuary Facebook page at ")
        ],
        link "https://www.facebook.com/estuaryPlatform"],
      Views [ Text $ fromList [
        (English,"The #Estuary channel at ")
        ],
        link "https://chat.toplap.org"],
      Views [ Text $ fromList [
        (English,"Bug reports and similar issues can be filed at ")
        ],
        link "https://github.com/dktr0/Estuary/issues"],
      Text $ fromList [
        (English,"Please also feel free to contact lead developer David Ogborn via email <ogbornd@mcmaster.ca>")
        ]
      ]
    ]
  ]
