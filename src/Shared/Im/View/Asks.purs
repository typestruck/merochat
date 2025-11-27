module Shared.Im.View.Asks where

import Prelude

import Client.Privilege as CCP
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol as TDS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Ask (Ask)
import Shared.Change as SCN
import Shared.DateTime as SDT
import Shared.Im.Svg as SIS
import Shared.Im.Types (ImMessage(..), ImModel, PostMode(..), RetryableRequest(..))
import Shared.Im.View.ChatInput as SIVC
import Shared.Markdown as SM
import Shared.Modal (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Options.Post (maxPostCharacters)
import Shared.Post (Post)
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Resource (maxImageSizeKB)
import Type.Proxy (Proxy(..))

asked ∷ ∀ message. Ask → Html message
asked ask = HE.div [ HA.class' "ask-entry" ]
      [ HE.div [] [ HE.text ask.question ]
      , HE.div [  ] [ HE.text ask.answer ]
      ]

