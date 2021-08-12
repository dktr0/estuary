{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.AboutEstuary (aboutEstuary) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response,link)
import Data.Map

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Types.EnsembleRequest
import Estuary.Types.View
import Estuary.Widgets.View

aboutEstuary :: MonadWidget t m => W t m (Event t EnsembleRequest)
aboutEstuary = viewWidget never $ Views [

  Paragraph [
    Text $ fromList [
      (English,"Estuary is a platform for collaboration and learning through live coding. It enables you to experiment with sound, music, and visuals in a web browser. Estuary brings together a curated collection of live coding languages in a single environment, without the requirement to install software (other than a web browser), and with support for networked ensembles (whether in the same room or distributed around the world). Estuary is free and open source software, released under the terms of the GNU Public License (version 3). Some of the live coding languages available within Estuary are:"),

      (Español,"Estuary es una plataforma para la colaboración y el aprendizaje a través de la codificación en vivo. Estuary permite experimentar con sonido, música e imágenes en un navegador web. Así mismo, Estuary reúne una colección curada de lenguajes de codificación en vivo en un solo entorno, sin el requisito de instalar software (que no sea un navegador web) y con soporte para agrupaciones artísticas en red (ya sea con gente participando en la misma sala o distribuida por todo el mundo). Estuary es un software gratuito y de código abierto, publicado bajo los términos de la Licencia Pública GNU (versión 3). Algunos de los lenguajes de codificación disponibles en Estuary son:")
      ],
    -- note: this list is in historical order by date of addition to Estuary
    -- and includes only languages that are being visibly used/developed/maintained
    BulletPoints [
      Text $ fromList [
        (English,"TidalCycles: for making patterns of musical events (created/maintained by Alex McLean)"),
        (Español,"TidalCycles: para hacer patrones de eventos musicales (creado/mantenido por Alex McLean)")
        ],
      Text $ fromList [
        (English,"Punctual: for synthesizing audio and/or video from the same notation (created/maintained by David Ogborn)"),
        (Español,"Puntual: para sintetizar audio y/o video de la misma notación (creado/mantenido por David Ogborn)")
        ],
      Text $ fromList [
        (English,"CineCer0: for videos and typography (created/maintained by the Estuary development team)")
        ],
      Text $ fromList [
        (English, "TimeNot: for creating temporal canons (created/maintained by Alejandro Franco Briones)"),
        (Español, "TimeNot: para crear cánones temporales (creado/mantenido por Alejandro Franco Briones)")
        ],
      Text $ fromList [
        (English,"Seis8s: for exploring Latin musical genres (created/maintained by Luis Navarro del Angel)"),
        (Español, "Seis8s: para explorar los géneros musicales latinos (creado/mantenido por Luis Navarro del Angel)")
        ],
      Text $ fromList [
        (English,"Hydra: for video synthesis (created/maintained by Olivia Jack)"),
        (Español, "Hydra: para síntesis de video (creado/mantenido por Olivia Jack)")
        ]
      ]
    ],

  Paragraph [
    Text $ fromList [
      (English,"Some additional features of Estuary are:"),
      (Español, "Algunas características adicionales de Estuary son:")
      ],
    BulletPoints [
      Text $ fromList [
        (English,"interfaces for collaboration and communication in networked ensembles"),
        (Español, "interfaces para la colaboración y la comunicación en agrupaciones artísticas en red")
        ],
      Text $ fromList [
        (English,"built-in tutorials and reference materials"),
        (Español,"tutoriales y materiales de referencia")
        ],
      Text $ fromList [
        (English,"text localization to an expanding set of natural languages"),
        (Español,"localización de texto hacia un conjunto creciente de lenguajes naturales")
        ],
      Text $ fromList [
        (English,"visual customization via themes (described by CSS)"),
        (Español,"personalización visual a través de temas (descritos por CSS)")
        ]
      ]
    ],

  Paragraph [
    Text $ fromList [
      (English,"Some places where further help/discussion/resources/etc about Estuary are available include:"),
      (Español, "Algunos lugares donde hay más ayuda/discusión/recursos/etc sobre Estuary son:")
      ],
    BulletPoints [
      Views [ Text $ fromList [
        (English,"The Estuary Discord server - all welcome! Sign-up at the following link (note that it's recommended to make an account on discord.com first):")
        ],
        link "https://discord.gg/snvFzkPtFr"],
      Views [ Text $ fromList [
        (English,"Bug reports and similar issues can be filed at "),
        (Español, "Los reportes de errores y problemas similares se pueden eviar a través de ")
        ],
        link "https://github.com/dktr0/Estuary/issues"]
      ]
    ],

  Paragraph [
    Text $ fromList [
      (English,"The development of Estuary is the result of ongoing work by many individuals, with the benefit of generous public and institutional support. Key contributions to work on the Estuary platform itself include:"),
      (Español, "El desarrollo de Estuary es el resultado del trabajo continuo de muchas personas, así como del beneficio de un generoso apoyo público e institucional. Algunas contribuciones clave para la propia plataforma de Estuary incluyen:")
      ],
    BulletPoints [
      Text $ fromList [
        (English,"Ongoing development and maintenance of Estuary by the Estuary development team, coordinated by David Ogborn, with members who have included Jamie Beverley, Alejandro Franco Briones, Alex MacLean, Luis Navarro del Angel, Matthew Paine, Spencer Park, and Jessica Rodriguez."),
        (Español, "Desarrollo y mantenimiento continuo de Estuary por el equipo de desarrollo de Estuary, coordinado por David Ogborn, con miembres que incluyen a Jamie Beverley, Alejandro Franco Briones, Alex MacLean, Luis Navarro del Angel, Matthew Paine, Spencer Park y Jessica Rodriguez . ")
        ],
      Text $ fromList [
        (English,"Estuary was born as part of research project supported by Canada's Social Sciences and Humanities Research Council (SSHRC): \"Projectional interfaces for musical live coding\" (2015-17, principal investigator: David Ogborn, co-applicant: Jacques Carette, collaborators: Alex McLean and Eldad Tsabary)"),
        (Español, "Estuary nació como parte de un proyecto de investigación apoyado por el Consejo de Investigación de Ciencias Sociales y Humanidades de Canadá (SSHRC): \" Interfaces de proyección para codificación musical en vivo \" (2015-17, investigador principal: David Ogborn, co-solicitante: Jacques Carette, colaboradores: Alex McLean y Eldad Tsabary) ")
        ],
      Text $ fromList [
        (English,"Estuary development continues as part of a second SSHRC-funded research project: \"Platforms  and  practices  for networked, language-neutral live coding\" (2018-23, principal investigator: David Ogborn, co-applicant: Eldad Tsabary, collaborator: Shelly Knotts)"),
        (Español, "El desarrollo de Estuary continúa actualmente como parte de un segundo proyecto de investigación financiado por SSHRC: \" Plataformas y prácticas para codificación en vivo en red y sin lenguaje \"(2018-23, investigador principal: David Ogborn, co-solicitante: Eldad Tsabary , colaboradora: Shelly Knotts) ")
        ],
      Text $ fromList [
        (English,"Most Estuary development happens in and around the Networked Imagination Laboratory, a research space at McMaster University created with support from the Canada Foundation for Innovation, Ontario's Ministry of Research and Innovation, and McMaster's Faculty of Humanities."),
        (Español, "La mayor parte del desarrollo de Estuary ocurre dentro y alrededor del Networked Imagination Laboratory, un espacio de investigación en la Universidad McMaster creado con el apoyo de la Fundación de Canadá para la Innovación, el Ministerio de Investigación e Innovación de Ontario y la Facultad de Humanidades de McMaster")
        ],
      Text $ fromList [
        (English,"Estuary's codebase builds upon the work of many others. Special thanks to the creators/maintainers of the tools used to build Estuary: GHCJS and the Reflex FRP platform!"),
        (Español, "El código base de Estuary se basa en el trabajo de muchos otres. ¡Un agradecimiento especial a les creadores / mantenedores de las herramientas utilizadas para construir Estuary: GHCJS y la plataforma Reflex FRP!")
        ],
      Text $ fromList [
        (English,"Last but not least... Estuary development would not be possible without the many individuals and groups that have made it a part of their explorations of live coding. Thank you!"),
        (Español, "Por último, pero no menos importante ... El desarrollo de Estuary no sería posible sin las muchas personas y grupos que lo han incluido en sus exploraciones de la codificación en vivo. ¡Gracias!")
        ]
      ]
    ]

  ]
