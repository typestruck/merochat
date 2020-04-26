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
import Debug.Trace (spy)
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE

view :: ProfileUser -> Html ProfileMessage
view (ProfileUser user) =
        HE.div (HA.class' "profile-info-edition") [
                HE.div (HA.class' "profile-info-alert") "Profile edition changes are saved automatically.",
                HE.div_ $ HE.img' [HA.class' "avatar-profile", HA.src user.avatar, title "avatar"],
                HE.div_ [
                        HE.h1 (title "name") user.name,
                        HE.h3 [HA.class' "headline", title "headline"] user.headline
                ],
                HE.div_ [
                        toInfoSpan "age"  (map ((_ <> ",") <<< show) user.age),
                        HE.span (HA.class' "smaller") " • ",
                        toInfoSpan "gender"  user.gender,
                        HE.span (HA.class' "smaller") " • ",
                        toInfoSpan "country"  user.country,
                        HE.span (HA.class' "smaller") " • ",
                        (toInfoSpan "languages"  <<< maybeLanguages $ DSC.joinWith ", " user.languages)
                ],
                HE.div_ $ map toTagSpan user.tags,
                HE.br,
                HE.span [HA.class' "profile-info-description", title "description"] user.description
        ]

        where   title name = HA.title $ "Click to edit your " <> name

                toInfoSpan itemName =
                        case _ of
                                Just s -> HE.span (title itemName) $ s <> " "
                                _ -> HE.span (HA.class' "profile-info-add") $ "Click here to add your " <> itemName <> " "

                maybeLanguages =
                        case _ of
                                "" -> Nothing
                                l -> Just ("speaks " <> l)

                toTagSpan tag = HE.span (HA.class' "tag") tag