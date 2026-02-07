module Shared.Im.View.Praise where

import Prelude

import Client.Privilege as CCP
import Data.Array as DA
import Data.Enum as DE
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Change as SCN
import Shared.Im.Types (ImMessage(..), ImModel, RetryableRequest(..), User)
import Shared.Modal (Modal(..), ScreenModal(..))
import Shared.Options.Praise (maxPraiseCharacters)
import Shared.Praise (PraisedFor(..))
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))

praised ∷ ∀ message. _ → Html message
praised praise = HE.div [ HA.class' "praise-entry" ]
      [
      -- [ HE.div [ HA.class' "praise-question" ]
      --         [ HE.span [ HA.class' "duller" ] [ HE.text $ praise.name <> " praises: " ]
      --         , HE.div [HA.class' "praise-question-itself"] [ HE.b [] [ HE.i [] [ HE.text praise.question ] ] ]
      --         ]
      -- , HE.div [ HA.class' "praise-answer" ] [ HE.text $ SU.fromJust praise.answer ]
      ]

praiseForm ∷ ImModel → User → Html ImMessage
praiseForm model user =
      if not $ SP.hasPrivilege SendPraise model.user then
            CCP.notEnoughKarma "send praises" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
      -- else if DA.elem user.id model.praises.unallowed then
      --       HE.div_ [ HE.text $ "You have already praised " <> user.name ]
      -- else if DA.elem user.id model.praises.sent then
      --       HE.div_ [ HE.text $ "Praise sent" ]
      else
            form
      where
      praiseOption po = HE.div [HA.class' "tag tag-praise"] [HE.text po]

      form = HE.div [ HA.class' "praises-form" ]
            [ HE.label [HA.class' "label-praise"] [ HE.text $ "What best describes " <> user.name <> "?" ]
            , HE.div_ $ map praiseOption praises
             , HE.textarea'
                    [ HA.class' { "chat-input" : true, hidden: true }
                    , HA.placeholder "Please specify"
                    , HA.maxlength maxPraiseCharacters
                    , HA.onInput' ResizeChatInput
                    --, SCN.onChange (SetPraise <<< SCN.toMaybe)
                    , HA.autocomplete "off"
                    --, HA.value $ DM.fromMaybe "" model.praises.question
                    ]
            , if model.praise.freeToSend then
                    HE.input [ HA.disabled $ DA.null model.praise.selected, HA.type' "button", HA.class' "green-button post-button build praise-button", HA.value "Send praise"] --, HA.onClick $ SendPraise user.id ]
              else
                    HE.div' [ HA.class' "loading" ]
            ]


praises :: Array String
praises = map praisedFor $ DE.upFromIncluding Funny
      where
      praisedFor = case  _ of
            Funny → "Funny"
            Advice → "Good advice"
            Arguing → "Likes to argue"
            Serious → "Serious conversations"
            FastReply → "Replies fast"
            AlwaysReply → "Always reply"
            LongReply → "Long form conversations"
            Sarcastic → "Sarcastic "
            Other _ →  "Other"
