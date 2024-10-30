module Shared.Experiments.Impersonation where

import Prelude
import Shared.Availability
import Shared.User
import Shared.Experiments.Types

import Client.Common.Privilege as CCP
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Array as DA
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Show.Generic as DGRS
import Data.Tuple (Tuple(..))
import Droplet.Language (class FromValue)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Avatar as SA
import Shared.DateTime (DateTimeWrapper(..), epoch)
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SPV
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU

type ImpersonationProfile = Record IU

data ExperimentData = Impersonation (Maybe ImpersonationProfile)

data ExperimentPayload = ImpersonationPayload
      { id ∷ Int
      , sender ∷ Boolean
      }

derive instance Generic ExperimentPayload _

instance EncodeJson ExperimentPayload where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson ExperimentPayload where
      decodeJson = DADGR.genericDecodeJson

instance Show ExperimentPayload where
      show = DGRS.genericShow

joined ∷ ImpersonationProfile → Html ChatExperimentMessage
joined profile = HE.div (HA.class' "exit-impersonation")
      [ HE.strong_ "You have already joined the Impersonation Experiment"
      , HE.button [ HA.class' "green-button danger exit-experiment", HA.onClick QuitExperiment ] "Exit"
      ]

view ∷ ChatExperimentModel → Html ChatExperimentMessage
view model = HE.div (HA.class' "impersonation")
      [ header Characters "Fictional"
      , profiles Characters [ batman ]
      , header HistoricalFigures "Historical figures"
      , profiles HistoricalFigures [ socrates ]
      , header Celebrities "Celebrities"
      , profiles Celebrities [ nicolasCage ]
      , HE.div (HA.class' { "modal-placeholder-overlay": true, hidden: true }) --DM.isNothing impersonation })
              [ HE.div (HA.class' "confirmation")
                          if SPV.hasPrivilege ImpersonationChatExperiment model.user then
                                [ HE.span (HA.class' "bold") $ "Start Impersonation Experiment as " <> {- DM.maybe "" _.name model.impersonation -}  "?"
                                , HE.div (HA.class' "buttons")
                                        [ HE.button [ HA.class' "cancel" {- , HA.onClick $ ConfirmImpersonation Nothing -}] "Cancel"
                                        , HE.button [ HA.class' "green-button" {-, HA.onClick <<< JoinExperiment $ Impersonation impersonation -}] "Start"
                                        ]
                                ]
                          else
                                [ CCP.notEnoughKarma "start this chat experiment" RedirectKarma
                                , HE.div (HA.class' "buttons")
                                        $ HE.button [ HA.class' "green-button" {-, HA.onClick $ ConfirmImpersonation Nothing -}] "Dismiss"
                                ]
              ]
      ]
      where
      header s name = HE.div [ HA.class' "impersonation-header", HA.onClick $ ToggleSection s ]
            [ HE.text name
            , HE.span (HA.class' "header-plus") if model.section == s then "-" else "+"
            ]

      profiles s = HE.div (HA.class' { hidden: model.section /= s }) <<< DA.mapWithIndex toProfile
      toProfile index p = HE.div [ HA.class' "contact" {-, HA.onClick $ ConfirmExperiment Impersonation -}]
            [ HE.div (HA.class' "avatar-contact-list-div")
                    [ HE.img [ HA.title $ SU.fromJust p.avatar, HA.class' $ "avatar-contact-list" <> SA.avatarColorClass (Just index), HA.src $ SU.fromJust p.avatar ]
                    ]
            , HE.div [ HA.class' "contact-profile", HA.title $ "Start Impersonation as " <> p.name ]
                    [ HE.span (HA.class' "contact-name") p.name
                    , HE.span (HA.class' "duller") p.headline
                    ]
            ]

batman ∷ ImpersonationProfile
batman =
      { id: 1
      , name: "Batman"
      , availability: None
      , joined: DateTimeWrapper epoch
      , readReceipts: true
      , messageTimestamps: true
      , typingStatus: true
      , onlineStatus: true
      , completedTutorial: true
      , avatar: Just $ SP.resourcePath (Left BatmanNounProjectAnushaNarvekar) Png
      , headline: "*raspy voice* I am Batman"
      , description: "I am not afraid of bats. Don't tell Robin I am here."
      , tags: [ "Martial arts", "Detective work", "Costumes", "Bats" ]
      , karma: 877
      , karmaPosition: 342
      , gender: Just $ show Male
      , privileges: []
      , bin: 1
      , badges: []
      , temporary: false
      , country: Just "Gotham"
      , languages: []
      , profileVisibility: Everyone
      , age: Just 34
      }

socrates ∷ ImpersonationProfile
socrates =
      { id: 2
      , name: "Socrates"
      , avatar: Just $ SP.resourcePath (Left SocratesStingWikimedia) Png
      , headline: "I know that I know nothing"
      , profileVisibility: Everyone
      , bin: 1
      , availability: None
      , readReceipts: true
      , privileges: []
      , badges: []
      , temporary: false
      , completedTutorial: true
      , messageTimestamps: true
      , typingStatus: true
      , onlineStatus: true
      , joined: DateTimeWrapper epoch
      , description:
              """Crito, we owe a rooster to Asclepius. Please, don't forget to pay the debt.

Crito, we owe a rooster to Asclepius, Pay it and do not neglect it.

Crito, we owe a rooster to Asclepius, make this offering to him and do not forget."""
      , tags: [ "Philosophy", "Poison", "Debating", "Youth" ]
      , karma: 879
      , karmaPosition: 345
      , gender: Just $ show Male
      , country: Just "Greece"
      , languages: []
      , age: Just 71
      }

nicolasCage ∷ ImpersonationProfile
nicolasCage =
      { id: 3
      , name: "Nicolas Cage"
      , avatar: Just $ SP.resourcePath (Left NicolasCageHiclipart) Png
      , headline: "I think I jump around more when I'm alone"
      , profileVisibility: Everyone
      , readReceipts: true
      , messageTimestamps: true
      , completedTutorial: true
      , privileges: []
      , badges: []
      , bin: 1
      , typingStatus: true
      , temporary: false
      , joined: DateTimeWrapper epoch
      , onlineStatus: true
      , availability: None
      , description:
              """Oh, boy! Oh, boy! Three and a half million dollar budget, some 16mm film stock thrown in, and I'm holding one of these. I have got to thank the members of the Academy for this, for including me in this group of super talents and for helping me blur the line between art and commerce with this award. I know it's not hip to say it, but I just love acting, and I hope that there'll be more encouragement for alternative movies where we can experiment and fast forward into the future of acting.

Let me thank the awesome, multi-talented Mike Figgis. My incredible, amazing co-star Elisabeth Shue. I am going to share this award with both of you and the late John O'Brien, whose spirit moved me so much. Tony Dingman, Annie Stewart -- the producers, Annie Stewart and Stuart Regen, everyone at MGM/UA and Lumiere. I'd like to thank Ed Limato, my colleagues Gerry Harrington, Jeff Levine, Richard Lovett. Everyone in my family, my gorgeous wife Patricia. And I just finally want to say: Hi, Weston. It's Daddy. I love you. Thank you."""
      , tags: [ "Movies", "Funny faces", "Acting", "Money" ]
      , karma: 878
      , karmaPosition: 343
      , gender: Just $ show Male
      , country: Just "United States"
      , languages: []
      , age: Just 57
      }

impersonations ∷ HashMap Int ImpersonationProfile
impersonations = DH.fromFoldable [ Tuple batman.id batman, Tuple socrates.id socrates, Tuple nicolasCage.id nicolasCage ]