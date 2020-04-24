module Shared.Profile.View where

import Prelude
import Shared.IM.Types
import Shared.Profile.Types

import Data.Array as DA
import Data.Array as DA
import Data.Enum as DE
import Data.Foldable as DF
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String.Common as DSC
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Debug.Trace (spy)
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

view :: ProfileUser -> Html ProfileMessage
view profileUser = HE.div (HA.class' "profile-edition") [
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
                ] ,
                HE.br,
                HE.span' user.description
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