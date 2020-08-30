module Shared.Profile.View where

import Prelude
import Shared.Types

import Data.Array ((:), (..))
import Data.Date (Month(..))
import Data.Date as DD
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Int53 as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String.Common as DSC
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Types (NodeData)
import Prim.Row (class Cons)
import Record as R
import Shared.Avatar as SA
import Shared.DateTime as SDT
import Shared.Setter as SS
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)

view :: Int -> ProfileModel -> Html ProfileMessage
view lastYearEligible ({
      user,
      countries,
      languages,
      birthday,
      isAgeVisible,
      isCountryVisible,
      isGenderVisible,
      isLanguagesVisible,
      isTagsVisible
}) =
      HE.div (HA.class' "profile-info-edition") [
            HE.div_ $ HE.img [HA.class' "avatar-profile", HA.src $ SA.avatarForSender user.avatar, title "avatar", HA.onClick SelectAvatar],
            HE.input [HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],
            --contentEditable doesn't work with snabbdom in several cases
            -- one being that on emptying the element snabbom tries to patch over the no longer existing text node
            -- (which the browser replaced with <br/>)
            HE.div [HA.class' "profile-edition-name", titleWithGenerated "name"] [
                  HE.textarea [HA.id "profile-edition-name", HA.onInput (SetField <<< SS.setUserField (SProxy :: SProxy "name"))] user.name
            ],
            HE.div [HA.class' "profile-edition-headline", titleWithGenerated "headline"] [
                  HE.textarea [HA.id "profile-edition-headline"] user.headline
            ],
            HE.div (HA.class' "profile-stats") [
                  if isAgeVisible then displayAge else editAge,
                  separator,
                  if isGenderVisible then displayGender else editGender,
                  separator,
                  if isCountryVisible then displayCountry else editCountry,
                  separator,
                  if isLanguagesVisible then displayLanguages else editLanguages
            ],
            HE.div (HA.class' "karma-stats") <<< HE.span_ $ "Karma: " <> show user.karma,
            if isTagsVisible then displayTags else editTags,
            HE.br,
            HE.div [HA.class' "profile-edition-description", titleWithGenerated "description"] [
                  HE.textarea [HA.id "profile-edition-description"] user.description
            ],
            HE.br,
            HE.input [HA.type' "button", HA.onClick SaveProfile, HA.value "Save profile", HA.class' "action-button end"]
      ]

      where separator = HE.span (HA.class' "smaller") " • "

            displayAge = display "age" (SProxy :: SProxy "isAgeVisible") <<< map show <<< SDT.ageFrom $ map DN.unwrap user.birthday
            displayGender = display "gender" (SProxy :: SProxy "isGenderVisible") $ map show user.gender
            displayCountry = display "country" (SProxy :: SProxy "isCountryVisible") do
                  country <- user.country
                  map DT.snd $ DF.find ((country == _) <<< DT.fst) countries
            displayLanguages =
                  display "languages" (SProxy :: SProxy "isLanguagesVisible") $ case DSC.joinWith ", " $ map getLanguage user.languages of
                        "" -> Nothing
                        l -> Just $ "speaks " <> l
            displayTags =
                  case user.tags of
                        [] -> display "tags" (SProxy :: SProxy "isTagsVisible") Nothing
                        tags -> HE.div_ $ map (HE.span [HA.class' "tag", HA.onClick (unsetField (SProxy :: SProxy "isTagsVisible")), HA.title "Click to edit tags"]) tags

            editAge = HE.span_ [
                  HE.span_ "Year ",
                  HE.select [HA.onInput SetYear] <<< displayOptions (SDT.getYear <$> user.birthday) $ map optionEntry (lastYearEligible .. 1900),
                  HE.span_ " Month ",
                  HE.select [HA.onInput SetMonth] <<< displayOptionsWith "Select" (SDT.getMonth <$> user.birthday) $ map optionEntry (1 .. 12),
                  HE.span_ " Day ",
                  HE.select [HA.onInput SetDay] <<< displayOptionsWith "Select" (SDT.getDay <$> user.birthday) <<< map optionEntry $ if canSelectDay birthday then (1 .. lastDayMonth birthday) else []
            ]
            editGender = HE.select [HA.onInput SetGender] $ displayOptions user.gender [Tuple Female $ show Female, Tuple Male $ show Male, Tuple NonBinary $ show NonBinary, Tuple Other $ show Other]
            editCountry = HE.select [HA.onInput SetCountry] <<< displayOptions (map (DI.toInt <<< DN.unwrap) user.country) $ map toInt countries
            editLanguages = HE.span_ ([
                  HE.select [HA.onInput AddLanguage] <<< displayOptionsWith "Select" Nothing $ map toInt languages
                ] <> map (\id -> tagEdition "language" RemoveLanguage <<< Tuple id $ getLanguage id) user.languages)
            editTags = HE.span_ ([
                  HE.span_ "Add tags to show your interests, hobbies, etc ",
                  HE.input [HA.type' "text", HA.onKeydown SetTagEnter, HA.autofocus true, HA.placeholder "Press enter to add", HA.maxlength 30]
               ] <> map (\tag -> tagEdition "tag" RemoveTag $ Tuple tag tag) user.tags)

            optionEntry n = Tuple n $ show n
            canSelectDay =
                  case _ of
                        Tuple (Just _) (Tuple (Just _) _ ) -> true
                        _ -> false
            lastDayMonth =
                  case _ of
                        Tuple (Just year) (Tuple (Just month) _) -> DE.fromEnum $ DD.lastDayOfMonth (SU.toEnum year) (SU.toEnum month)
                        _ -> 0

            --safe for language and countries since these have a small fixed amonut of entries
            toInt (Tuple (PrimaryKey pk) value) = Tuple (DI.toInt pk) value

            languageHM = DH.fromArray languages
            getLanguage = SU.fromJust <<< flip DH.lookup languageHM

title :: String -> NodeData ProfileMessage
title name = HA.title $ "Click to edit your " <> name

titleWithGenerated :: String -> NodeData ProfileMessage
titleWithGenerated name = HA.title $ "Click to edit your " <> name <> ". Leave blank for autogenerated."

display :: forall field r. IsSymbol field => Cons field Boolean r PM => String -> SProxy field -> Maybe String -> Html ProfileMessage
display itemName field =
      case _ of
            Just s -> HE.span [title itemName, HA.onClick (unsetField field)] $ s <> " "
            _ -> HE.span [HA.class' "profile-info-add", HA.onClick (unsetField field)] $ "Click here to add your " <> itemName <> " "

displayOptions :: forall id. Show id => Eq id => Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
displayOptions = displayOptionsWith "Don't show"

displayOptionsWith :: forall id. Show id => Eq id => String -> Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
displayOptionsWith unselectedText current = (HE.option [HA.selected $ DM.isNothing current] unselectedText : _) <<< map makeOptions
      where makeOptions (Tuple id value) = HE.option [HA.value $ show id, HA.selected $ Just id == current] value

tagEdition :: forall a. String -> (a -> Event -> ProfileMessage) -> Tuple a String -> Html ProfileMessage
tagEdition title message (Tuple id text) = HE.span [HA.onClick' (message id), HA.title $ "Click to remove " <> title, HA.class' "tag"] [
      HE.text text,
      HE.a (HA.class' "remove-tag") "x"
]

unsetField :: forall field r. IsSymbol field => Cons field Boolean r PM => SProxy field -> ProfileMessage
unsetField field = SetField (R.set field false)