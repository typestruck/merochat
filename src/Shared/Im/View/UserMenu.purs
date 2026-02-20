module Shared.Im.View.UserMenu where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Data.Array as DA
import Data.Maybe as DM
import Droplet.Language (count)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Avatar as SA
import Shared.Im.Svg as SIS
import Shared.Intl as SI
import Shared.Modal (Modal(..), ScreenModal(..), SpecialModal(..))
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
                    , SIS.gear [ HA.onClick <<< SpecialRequest $ ToggleModal (Screen $ if model.smallScreen then ShowMenu else ShowProfile) ]
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
            | c.read = HE.span [ HA.onClick $ PerformChangelogAction c.action c.value, HA.class' { "inbox-entry": true, "inbox-entry-action": DM.isJust c.action } ] [ HE.text c.description ]
            | otherwise = HE.div [ HA.class' { "inbox-entry unread-inbox-entry": true, "inbox-entry-action": DM.isJust c.action }, HA.onClick $ PerformChangelogAction c.action c.value ]
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