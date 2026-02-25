module Shared.Im.View.Praise where

import Prelude

import Client.Privilege as CCP
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple as DT
import Data.Tuple.Nested (type (/\), (/\))
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Change as SCN
import Shared.Im.Types (ImMessage(..), ImModel, RetryableRequest(..))
import Shared.Modal (Modal(..), ScreenModal(..))
import Shared.Options.Praise (maxPraiseCharacters)
import Shared.Praise (PraisedFor(..), Praise)
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.User (PraiseStatus(..), User)

praised ∷ ∀ message. Array PraisedFor → Array (Html message)
praised rows = map display counts
      where
      counts = map run $ DA.group  rows
      run p = DAN.head p /\ DAN.length p

      display (praise /\ n) = HE.div [ HA.class' "praise-entry" ]
            $ [ HE.div [ HA.class' "tag tag-praise" ]
                    [ HE.text <<< DT.snd $ displayPraise praise ]
            ] <> (if n > 1 then [HE.text $ "x " <> show n] else [])

praiseForm ∷ ImModel → User → Html ImMessage
praiseForm model user =
      if not $ SP.hasPrivilege SendPraise model.user then
            CCP.notEnoughKarma "send praises" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
      else if user.praiseStatus == HasPraised || not model.praise.freeToFetch then
            HE.div [] []
      else if user.praiseStatus == PraiseNotAccepted then
            HE.div_ [ HE.text $ "You have already praised " <> user.name ]
      else
            form
      where
      isSelected for = case model.praise.selected of
            Nothing → false
            Just (_ /\ selection) → DA.elem for selection

      praiseOption (for /\ name) = HE.div [ HA.class' { "tag tag-praise": true, selected: isSelected for }, HA.onClick $ TogglePraise user.id for ] [ HE.text name ]

      hasOtherPraise = isSelected $ Other ""
      form = HE.div [ HA.class' "praises-form" ]
            [ HE.label [ HA.class' "label-praise" ] [ HE.text $ "What best describes " <> user.name <> "?" ]
            , HE.div_ $ map praiseOption praises
            , HE.textarea'
                    [ HA.class' { "chat-input": true, hidden: not hasOtherPraise }
                    , HA.placeholder "Please specify"
                    , HA.maxlength maxPraiseCharacters
                    , HA.onInput' ResizeChatInput
                    , SCN.onChange (SetOtherPraise <<< SCN.toMaybe)
                    , HA.autocomplete "off"
                    , HA.value $ DM.fromMaybe "" model.praise.other
                    ]
            , if model.praise.freeToSave then
                    HE.input [ HA.disabled $ DM.isNothing model.praise.selected || hasOtherPraise && DM.isNothing model.praise.other, HA.type' "button", HA.class' "green-button post-button build ask-button", HA.value "Save", HA.onClick SavePraise ]
              else
                    HE.div' [ HA.class' "loading" ]
            ]

praises ∷ Array (PraisedFor /\ String)
praises = map displayPraise $ DE.upFromIncluding Funny

displayPraise = case _ of
      Funny → Funny /\ "Funny"
      Advice → Advice /\ "Good advice"
      Arguing → Arguing /\ "Likes to argue"
      Serious → Serious /\ "Serious conversations"
      FastReply → FastReply /\ "Replies fast"
      AlwaysReply → AlwaysReply /\ "Always reply"
      LongReply → LongReply /\ "Long form conversations"
      Sarcastic → Sarcastic /\ "Sarcastic "
      Other _ → Other "" /\ "Other"
