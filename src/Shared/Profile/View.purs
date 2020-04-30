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
import Data.Symbol (SProxy(..))
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Record as R

view :: ProfileModel -> Html ProfileMessage
view (ProfileModel {user: ProfileUser user}) =
        HE.div (HA.class' "profile-info-edition") [
                HE.div_ $ HE.img' [HA.class' "avatar-profile", HA.src user.avatar, title "avatar", HA.onClick SelectAvatar],
                HE.input [HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],
                HE.div_ [
                        HE.h1 [HA.id "profile-edition-name", title "name", HA.contentEditable true, HA.onInput SetName, HA.onKeydown NameEnter ] user.name,
                        HE.h3 [HA.id "profile-edition-headline", HA.class' "headline", title "headline", HA.contentEditable true, HA.onInput SetHeadline, HA.onKeydown HeadlineEnter] user.headline
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
                HE.span [HA.class' "profile-info-description", title "description"] user.description,
                HE.br,
                HE.input [HA.type' "button", HA.onClick SaveProfile, HA.value "Save profile", HA.class' "action-button end"]
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