module Shared.Profile.View where

import Prelude
import Shared.Types

import Data.Array ((:), (..))
import Data.Date as DD
import Data.Enum as DE
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String.Common as DSC
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Uncurried (EffectFn2)
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Flame.Renderer.Hook as FRH
import Flame.Types (NodeData, VNode)
import Prim.Row (class Cons)
import Record as R
import Shared.Avatar as SA
import Shared.DateTime as SDT
import Shared.Markdown as SM
import Shared.Setter as SS
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)

foreign import focus :: EffectFn2 VNode VNode Unit

view :: Int -> ProfileModel -> Html ProfileMessage
view lastYearEligible {
      user,
      countries,
      languages,
      birthday,
      nameInputed,
      headlineInputed,
      descriptionInputed,
      isEditingAge,
      isEditingCountry,
      isEditingGender,
      isEditingLanguages,
      isEditingTags
} =
      HE.div (HA.class' "profile-edition suggestion contact") [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/profile.css"],
            HE.div_ $ HE.img [HA.class' "avatar-profile-edition", HA.src $ SA.avatarForSender user.avatar, HA.onClick SelectAvatar],
            HE.input [HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],

            HE.div [title "name", HA.class' {"hidden": DM.isJust nameInputed}, HA.onClick (toogleFieldEdition (SProxy :: SProxy "name") (SProxy :: SProxy "nameInputed"))] [
                  HE.h1 (HA.class' "profile-edition-name") user.name,
                  pen
            ],
            HE.div (HA.class' {edition: true, hidden: DM.isNothing nameInputed}) [
                  HE.div (HA.class' "bold") "Your name",
                  HE.div (HA.class' "duller") [
                        HE.text "This is the name other users will see when looking at your profile",
                        HE.br,
                        HE.text "Leave it blank to generate a new random name"
                  ],
                  HE.input [
                        FRH.atPostpatch focus,
                        HA.class' "single-line-input",
                        HA.maxlength 40,
                        HA.onKeydown (exitEditionGenerated SetName (SProxy :: SProxy "name") (SProxy :: SProxy "nameInputed")),
                        HA.onInput (setInputedField (SProxy :: SProxy "nameInputed")),
                        HA.value $ DM.fromMaybe "" nameInputed
                  ],
                  checkGenerated SetName,
                  cancel (SProxy :: SProxy "nameInputed")
            ],

            HE.div [title "headline", HA.class' {"hidden": DM.isJust headlineInputed}, HA.onClick (toogleFieldEdition (SProxy :: SProxy "headline") (SProxy :: SProxy "headlineInputed"))] [
                  HE.span (HA.class' "profile-edition-headline") user.headline,
                  pen
            ],
            HE.div (HA.class' {edition: true, hidden: DM.isNothing headlineInputed}) [
                  HE.div (HA.class' "bold") "Your headline",
                  HE.div (HA.class' "duller") [
                        HE.text "A tagline to draw attention to your profile",
                        HE.br,
                        HE.text "Leave it blank to generate a new random headline"
                  ],
                  HE.input [
                        FRH.atPostpatch focus,
                        HA.class' "single-line-input",
                        HA.maxlength 100,
                        HA.onKeydown (exitEditionGenerated SetHeadline (SProxy :: SProxy "headline") (SProxy :: SProxy "headlineInputed")),
                        HA.onInput (setInputedField (SProxy :: SProxy "headlineInputed")),
                        HA.value $ DM.fromMaybe "" headlineInputed
                  ],
                  checkGenerated SetHeadline,
                  cancel (SProxy :: SProxy "headlineInputed")
            ],

            -- HE.div (HA.class' "profile-karma no-cursor") [
            --       HE.div_ [
            --             HE.span [HA.class' "span-info"] $ show user.karma,
            --             HE.span [HA.class' "duller"] " karma"
            --       ]
            -- ],
            -- HE.div (HA.class' "profile-asl") [
            --       if isEditingAge then displayAge else editAge,
            --       if isEditingGender then displayGender else editGender,
            --       if isEditingCountry then displayCountry else editCountry,
            --       if isEditingLanguages then displayLanguages else editLanguages
            -- ],

            -- HE.div (HA.class' "profile-tags") $ if isEditingTags then displayTags else editTags,

            HE.div [title "description", HA.class' {about: true, hidden: DM.isJust descriptionInputed}, HA.onClick (toogleFieldEdition (SProxy :: SProxy "description") (SProxy :: SProxy "descriptionInputed"))] [
                  HE.span (HA.class' "duller") "About",
                  pen
            ],
            HE.div [title "description", HA.class' { hidden: DM.isJust descriptionInputed}, HA.onClick (toogleFieldEdition (SProxy :: SProxy "description") (SProxy :: SProxy "descriptionInputed"))] [
                  HE.div' [HA.class' "profile-description", HA.innerHTML $ SM.toHTML user.description]
            ],
            HE.div [title "description", HA.class' {"description-edition": true, hidden: DM.isNothing descriptionInputed}] [
                  HE.div (HA.class' "bold") "Your description",
                  HE.div (HA.class' "duller") [
                        HE.text "Talk a little (or a lot) about yourself",
                        HE.br,
                        HE.text "Leave it blank to generate a new random description"
                  ],
                  HE.textarea [
                        HA.class' "profile-edition-description",
                        HA.maxlength 1000,
                        FRH.atPostpatch focus,
                        HA.onInput (setInputedField (SProxy :: SProxy "descriptionInputed"))
                  ] $ DM.fromMaybe "" descriptionInputed,

                  HE.div (HA.class' "save-cancel") [
                        checkGenerated SetHeadline,
                        cancel (SProxy :: SProxy "headlineInputed")
                  ]
            ]

            -- HE.input [HA.type' "button", HA.onClick SaveProfile, HA.value "Save profile", HA.class' "green-button end"]
      ]

      -- where displayAge = display "age" (SProxy :: SProxy "isEditingAge") <<< map show <<< SDT.ageFrom $ map DN.unwrap user.birthday
      --       displayGender = display "gender" (SProxy :: SProxy "isEditingGender") $ map show user.gender
      --       displayCountry = display "country" (SProxy :: SProxy "isEditingCountry") do
      --             country <- user.country
      --             map DT.snd $ DF.find ((country == _) <<< DT.fst) countries
      --       displayLanguages =
      --             display "languages" (SProxy :: SProxy "isEditingLanguages") $ case DSC.joinWith ", " $ map getLanguage user.languages of
      --                   "" -> Nothing
      --                   l -> Just $ "speaks " <> l
      --       displayTags =
      --             case user.tags of
      --                   [] -> display "tags" (SProxy :: SProxy "isEditingTags") Nothing
      --                   tags -> HE.div_ $ map (HE.span [HA.class' "tag", HA.onClick (toogleFieldEdition (SProxy :: SProxy "isEditingTags")), HA.title "Click to edit tags"]) tags

      --       editAge = HE.span_ [
      --             HE.span_ "Year ",
      --             HE.select [HA.onInput SetYear] <<< displayOptions (SDT.getYear <$> user.birthday) $ map optionEntry (lastYearEligible .. 1900),
      --             HE.span_ " Month ",
      --             HE.select [HA.onInput SetMonth] <<< displayOptionsWith "Select" (SDT.getMonth <$> user.birthday) $ map optionEntry (1 .. 12),
      --             HE.span_ " Day ",
      --             HE.select [HA.onInput SetDay] <<< displayOptionsWith "Select" (SDT.getDay <$> user.birthday) <<< map optionEntry $ if canSelectDay birthday then (1 .. lastDayMonth birthday) else []
      --       ]
      --       editGender = HE.select [HA.onInput SetGender] $ displayOptions user.gender [Tuple Female $ show Female, Tuple Male $ show Male, Tuple NonBinary $ show NonBinary, Tuple Other $ show Other]
      --       editCountry = HE.select [HA.onInput SetCountry] $ displayOptions user.country countries
      --       editLanguages = HE.span_ ([
      --             HE.select [HA.onInput AddLanguage] $ displayOptionsWith "Select" Nothing languages
      --           ] <> map (\id -> tagEdition "language" RemoveLanguage <<< Tuple id $ getLanguage id) user.languages)
      --       editTags = HE.span_ ([
      --             HE.span_ "Add tags to show your interests, hobbies, etc ",
      --             HE.input [HA.type' "text", HA.onKeydown SetTagEnter, HA.autofocus true, HA.placeholder "Press enter to add", HA.maxlength 30]
      --          ] <> map (\tag -> tagEdition "tag" RemoveTag $ Tuple tag tag) user.tags)

      --       optionEntry n = Tuple n $ show n
      --       canSelectDay =
      --             case _ of
      --                   Tuple (Just _) (Tuple (Just _) _ ) -> true
      --                   _ -> false
      --       lastDayMonth =
      --             case _ of
      --                   Tuple (Just year) (Tuple (Just month) _) -> DE.fromEnum $ DD.lastDayOfMonth (SU.toEnum year) (SU.toEnum month)
      --                   _ -> 0

      --       languageHM = DH.fromArray languages
      --       getLanguage = SU.fromJust <<< flip DH.lookup languageHM

toogleFieldEdition field inputField = SetPField (\model -> R.set inputField (Just $ R.get field model.user) model)

exitEditionGenerated message field inputField (Tuple key _) =
      if key == "Enter" then
            message
       else if key == "Escape" then
            SetPField (resetEditionField inputField)
       else
            SetPField identity

exitEdition field inputField (Tuple key _) = SetPField $ \model ->
      if key == "Enter" then
            copyEditionToField field inputField model
       else if key == "Escape" then
            resetEditionField inputField model
       else
            model

copyEditionToField field inputField model = R.set inputField Nothing $ SS.setUserField field (DM.fromMaybe "" $ R.get inputField model) model

resetEditionField inputField = R.set inputField Nothing

setInputedField field = SetPField <<< R.set field <<< Just

setField field = SetPField <<< SS.setUserField field

pen :: Html ProfileMessage
pen = HE.svg [HA.class' "svg-16 edit", HA.viewBox "0 0 512 512"] [
      HE.path' $ HA.d "M295.772,113.228,85.791,323.209,38.818,461.681a9,9,0,0,0,2.159,9.255l.087.087a9,9,0,0,0,9.255,2.159l138.472-46.973L398.772,216.228Z",
      HE.path' $ HA.d "M458.648,53.353h0a72.833,72.833,0,0,0-103,0l-34.42,34.42,103,103,34.42-34.42A72.832,72.832,0,0,0,458.648,53.353Z"
]

checkGenerated message = HE.svg [HA.class' "svg-20 save", HA.viewBox "0 0 512 512", HA.onClick message] [
      HE.title "Save edition",
      HE.polygon' [HA.points "204 445.539 35.23 276.77 108.77 203.23 204 298.461 419.23 83.23 492.77 156.77 204 445.539"]
]


--check :: Html ProfileMessage
check field inputField = HE.svg [HA.class' "svg-20 save", HA.viewBox "0 0 512 512", HA.onClick (SetPField (copyEditionToField field inputField))] [
      HE.title "Save edition",
      HE.polygon' [HA.points "204 445.539 35.23 276.77 108.77 203.23 204 298.461 419.23 83.23 492.77 156.77 204 445.539"]
]

cancel inputField = HE.svg [HA.class' "svg-16 cancel", HA.viewBox "0 0 512 512", HA.onClick (SetPField (resetEditionField inputField)) ] [
      HE.title "Cancel edition",
      HE.polygon' $ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"
]

-- display :: forall field r. IsSymbol field => Cons field Boolean r PM => String -> SProxy field -> Maybe String -> Html ProfileMessage
-- display itemName field =
--       case _ of
--             Just s -> HE.span [title itemName, HA.onClick (toogleFieldEdition field)] $ s <> " "
--             _ -> HE.span [HA.class' "profile-info-add", title itemName, HA.onClick (toogleFieldEdition field)] [
--                   HE.text $ "Your " <> itemName <> " ",
--                   pen
--             ]

-- displayOptions :: forall id. Show id => Eq id => Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
-- displayOptions = displayOptionsWith "Don't show"

-- displayOptionsWith :: forall id. Show id => Eq id => String -> Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
-- displayOptionsWith unselectedText current = (HE.option [HA.selected $ DM.isNothing current] unselectedText : _) <<< map makeOptions
--       where makeOptions (Tuple id value) = HE.option [HA.value $ show id, HA.selected $ Just id == current] value

-- tagEdition :: forall a. String -> (a -> Event -> ProfileMessage) -> Tuple a String -> Html ProfileMessage
-- tagEdition title message (Tuple id text) = HE.span [HA.onClick' (message id), HA.title $ "Click to remove " <> title, HA.class' "tag"] [
--       HE.text text,
--       HE.a (HA.class' "remove-tag") "x"
-- ]

title :: String -> NodeData ProfileMessage
title name = HA.title $ "Click to edit your " <> name