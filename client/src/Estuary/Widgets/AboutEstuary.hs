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
      (English,"Estuary is a platform for collaboration and learning through live coding. It enables you to experiment with sound, music, and visuals in a web browser. Estuary brings together a curated collection of live coding languages in a single environment, without the requirement to install software (other than a web browser), and with support for networked ensembles (whether in the same room or distributed around the world). Estuary is free and open source software, released under the terms of the GNU Public License (version 3). Some of the live coding languages available within Estuary are:")
      ],
    -- note: this list is in historical order by date of addition to Estuary
    -- and includes only languages that are being visibly used/developed/maintained
    BulletPoints [
      Text $ fromList [
        (English,"TidalCycles: for making patterns of musical events (created/maintained by Alex McLean)")
        ],
      Text $ fromList [
        (English,"Punctual: for synthesizing audio and/or video from the same notation (created/maintained by David Ogborn)")
        ],
      Text $ fromList [
        (English,"CineCer0: for videos and typography (created/maintained by the Estuary development team)")
        ],
      Text $ fromList [
        (English,"TimeNot: for creating temporal canons (created/maintained by Alejandro Franco Briones)")
        ],
      Text $ fromList [
        (English,"Seis8s: for exploring Latin musical genres (created/maintained by Luis Navarro del Angel)")
        ],
      Text $ fromList [
        (English,"Hydra: for video synthesis (created/maintained by Olivia Jack)")
        ]
      ]
    ],

  Paragraph [
    Text $ fromList [
      (English,"Some additional features of Estuary are:")
      ],
    BulletPoints [
      Text $ fromList [
        (English,"interfaces for collaboration and communication in networked ensembles")
        ],
      Text $ fromList [
        (English,"built-in tutorials and reference materials"),
        (Español,"tutoriales y materiales de referencia")
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
        link "https://github.com/dktr0/Estuary/issues"]
      ]
    ],

  Paragraph [
    Text $ fromList [
      (English,"The development of Estuary is the result of ongoing work by many individuals, with the benefit of generous public and institutional support. Key contributions to work on the Estuary platform itself include:")
      ],
    BulletPoints [
      Text $ fromList [
        (English,"Ongoing development and maintenance of Estuary by the Estuary development team, coordinated by David Ogborn, with members who have included Jamie Beverley, Alejandro Franco Briones, Alex MacLean, Luis Navarro del Angel, Matthew Paine, Spencer Park, and Jessica Rodriguez.")
        ],
      Text $ fromList [
        (English,"Estuary was born as part of research project supported by Canada's Social Sciences and Humanities Research Council (SSHRC): \"Projectional interfaces for musical live coding\" (2015-17, principal investigator: David Ogborn, co-applicant: Jacques Carette, collaborators: Alex McLean and Eldad Tsabary)")
        ],
      Text $ fromList [
        (English,"Estuary development continues as part of a second SSHRC-funded research project: \"Platforms  and  practices  for networked, language-neutral live coding\" (2018-23, principal investigator: David Ogborn, co-applicant: Eldad Tsabary, collaborator: Shelly Knotts)")
        ],
      Text $ fromList [
        (English,"Most Estuary development happens in and around the Networked Imagination Laboratory, a research space at McMaster University created with support from the Canada Foundation for Innovation, Ontario's Ministry of Research and Innovation, and McMaster's Faculty of Humanities.")
        ],
      Text $ fromList [
        (English,"Estuary's codebase builds upon the work of many others. Special thanks to the creators/maintainers of the tools used to build Estuary: GHCJS and the Reflex FRP platform!")
        ],
      Text $ fromList [
        (English,"Last but not least... Estuary development would not be possible without the many individuals and groups that have made it a part of their explorations of live coding. Thank you!")
        ]
      ]
    ]

  ]
