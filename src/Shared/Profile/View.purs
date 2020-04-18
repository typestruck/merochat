module Shared.Profile.View where

import Prelude
import Shared.IM.Types
import Shared.Profile.Types
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String.Common as DSC
import Shared.Unsafe((!@))
import Shared.Unsafe as SU
import Flame (Html)
import Data.Tuple(Tuple(..))
import Flame.HTML.Attribute as HA
import Data.Int53 as DI
import Data.Newtype as DN
import Debug.Trace (spy)
import Data.Enum as DE
import Data.Foldable as DF
import Debug.Trace(spy)
import Flame.HTML.Element as HE
import Data.Array as DA

view :: ProfileUser -> Html ProfileMessage
view profileUser = HE.div (HA.class' "im") [
        HE.div (HA.class' "chat-box") [
                profile profileUser
        ]
]

profile :: ProfileUser -> Html ProfileMessage
profile (ProfileUser user) =
        HE.div (HA.class' "suggestion") [
                HE.div (HA.class' "profile-info") [
                        HE.div_ $ HE.img' [HA.class' "avatar-profile", HA.src user.avatar],
                        HE.div_ [
                                HE.h1_ user.name,
                                HE.h3 (HA.class' "headline") user.headline
                        ],
                        HE.div_ $
                                toInfoSpan false (map ((_ <> ",") <<< show) user.age) <>
                                toInfoSpan true user.gender <>
                                toInfoSpan true user.country <>
                                --maybe include local time?
                                (toInfoSpan false <<< maybeLanguages $ DSC.joinWith ", " user.languages),
                        HE.div_ $ map toTagSpan user.tags
                ]
        ]
         where  toInfoSpan includeSepator =
                        case _ of
                                Just s ->
                                        [HE.span_ $ s <> " "] <>
                                        (if includeSepator then
                                                [HE.span (HA.class' "smaller") "• "]
                                         else [])
                                _ -> [HE.createEmptyElement "span"]

                maybeLanguages =
                        case _ of
                                "" -> Nothing
                                l -> Just ("speaks " <> l)

                toTagSpan tag = HE.span (HA.class' "tag") tag