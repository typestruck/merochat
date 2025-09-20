module Shared.Im.View.Posts where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.DateTime as SDT
import Shared.Im.Types (ImMessage)
import Shared.Markdown as SM
import Shared.Post (Post)

posted ∷ String → Post → Html ImMessage
posted userName post = HE.div [ HA.class' "post-entry" ]
      [ HE.div [ HA.class' "post-header" ] [ HE.div_ [ HE.text userName ], HE.div [HA.class' "post-header-separator duller"] [ HE.text " • " ], HE.div [ HA.class' "duller" ] [ HE.text <<< SDT.ago $ SC.coerce post.date ] ]
      , HE.div' [ HA.class' "post-content", HA.innerHtml $ SM.parse post.content ]
      ]