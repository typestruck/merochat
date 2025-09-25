module Shared.Im.View.UserMenu where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Data.Array as DA
import Droplet.Language (count)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Avatar as SA
import Shared.Im.Svg as SIS
import Shared.Intl as SI
import Shared.Modal.Types (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP

userMenu ∷ ImModel → Html ImMessage
userMenu model =
      HE.div [ HA.class' "settings" ]
            [ header model
            , HE.div [ HA.class' "outer-user-menu" ]
                    [ envelope [ HA.viewBox "0 0 122.88 78.607", HA.class' "svg-inbox", HA.onClick ToggleChangelog, HA.title "MeroChat updates" ]
                    , changelogCount model
                    , changelogInbox model
                    , HE.svg [ HA.onClick <<< SpecialRequest $ ToggleModal (Screen $ if model.smallScreen then ShowMenu else ShowProfile), HA.class' "svg-32 svg-user-menu-context", HA.viewBox "0 0 50 50" ]
                            [ HE.path' [ HA.d "M47.16,21.221l-5.91-0.966c-0.346-1.186-0.819-2.326-1.411-3.405l3.45-4.917c0.279-0.397,0.231-0.938-0.112-1.282 l-3.889-3.887c-0.347-0.346-0.893-0.391-1.291-0.104l-4.843,3.481c-1.089-0.602-2.239-1.08-3.432-1.427l-1.031-5.886 C28.607,2.35,28.192,2,27.706,2h-5.5c-0.49,0-0.908,0.355-0.987,0.839l-0.956,5.854c-1.2,0.345-2.352,0.818-3.437,1.412l-4.83-3.45 c-0.399-0.285-0.942-0.239-1.289,0.106L6.82,10.648c-0.343,0.343-0.391,0.883-0.112,1.28l3.399,4.863 c-0.605,1.095-1.087,2.254-1.438,3.46l-5.831,0.971c-0.482,0.08-0.836,0.498-0.836,0.986v5.5c0,0.485,0.348,0.9,0.825,0.985 l5.831,1.034c0.349,1.203,0.831,2.362,1.438,3.46l-3.441,4.813c-0.284,0.397-0.239,0.942,0.106,1.289l3.888,3.891 c0.343,0.343,0.884,0.391,1.281,0.112l4.87-3.411c1.093,0.601,2.248,1.078,3.445,1.424l0.976,5.861C21.3,47.647,21.717,48,22.206,48 h5.5c0.485,0,0.9-0.348,0.984-0.825l1.045-5.89c1.199-0.353,2.348-0.833,3.43-1.435l4.905,3.441 c0.398,0.281,0.938,0.232,1.282-0.111l3.888-3.891c0.346-0.347,0.391-0.894,0.104-1.292l-3.498-4.857 c0.593-1.08,1.064-2.222,1.407-3.408l5.918-1.039c0.479-0.084,0.827-0.5,0.827-0.985v-5.5C47.999,21.718,47.644,21.3,47.16,21.221z M25,32c-3.866,0-7-3.134-7-7c0-3.866,3.134-7,7-7s7,3.134,7,7C32,28.866,28.866,32,25,32z" ] ]
                    ]
            ]

changelogCount ∷ ImModel → Html ImMessage
changelogCount model = HE.span [ HA.class' { "changelog-count": true, hidden: count == 0 } ] [ HE.text <<< show $ count ]
      where
      count = DA.length $ DA.filter (not <<< _.read) model.changelogs

changelogInbox ∷ ImModel → Html ImMessage
changelogInbox model = HE.div [ HA.class' { "changelog-inbox": true, hidden: not model.showChangelogs } ]
      [ HE.div [ HA.class' "inbox-header" ]
              [ SIS.invertedMerochat [ HA.class' "inbox-inverted-merochat-svg", HA.viewBox "0 0 122 22", HA.fill "none" ]
              , HE.span [ HA.class' "inbox-updates" ] [ HE.text "Updates" ]
              , SIS.closeX [ HA.class' "svg-32", HA.onClick ToggleChangelog ]
              ]
      , HE.div
              [ HA.class' "inbox" ] $ map log model.changelogs
      ]
      where
      log c
            | c.read = HE.span [ HA.class' "inbox-entry" ] [ HE.text c.description ]
            | otherwise = HE.div [ HA.class' "inbox-entry unread-inbox-entry" ]
                    [ envelope [ HA.viewBox "0 0 122.88 78.607", HA.class' "svg-inbox-unread" ]
                    , HE.text c.description
                    ]

envelope ∷ Array (NodeData ImMessage) → Html ImMessage
envelope attrs = HE.svg attrs
      [ HE.g_
              [ HE.path' [ HA.fillRule "evenodd", HA.clipRule "evenodd", HA.d "M61.058,65.992l24.224-24.221l36.837,36.836H73.673h-25.23H0l36.836-36.836 L61.058,65.992L61.058,65.992z M1.401,0l59.656,59.654L120.714,0H1.401L1.401,0z M0,69.673l31.625-31.628L0,6.42V69.673L0,69.673z M122.88,72.698L88.227,38.045L122.88,3.393V72.698L122.88,72.698z" ]
              ]
      ]

header ∷ ImModel → Html ImMessage
header model = HE.fragment
      [ HE.img [ HA.onClick avatarAction, HA.class' "avatar-settings", HA.src $ SA.fromAvatar model.user, HA.title "Post to MeroChat" ]
      , HE.div [ HA.class' "settings-name" ]
              [ HE.strong [ HA.class' "contact-name" ] [ HE.text model.user.name ]
              , HE.div [ HA.class' "settings-karma", HA.onClick <<< SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges, HA.title "See your privileges and karma stats" ]
                      [ HE.span [ HA.class' "karma-number" ] [ HE.text $ SI.thousands model.user.karma ]
                      , HE.span [ HA.class' "duller" ] [ HE.text $ " karma • (#" <> show model.user.karmaPosition <> ")" ]
                      ]
              ]
      ]
      where
      avatarAction
            | SP.hasPrivilege PublishPosts model.user = SpecialRequest <<< ToggleModal $ Special ShowPostForm
            | otherwise = SpecialRequest <<< ToggleModal $ Screen ShowProfile