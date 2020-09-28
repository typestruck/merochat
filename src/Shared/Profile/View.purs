module Shared.Profile.View where

import Prelude
import Shared.Types

import Control.Alt ((<|>))
import Data.Array ((:), (..))
import Data.Array as DA
import Data.Date as DD
import Data.Enum as DE
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String.Common as DSC
import Data.String.Read (class Read)
import Data.String.Read as DSR
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Debug.Trace (spy)
import Effect.Uncurried (EffectFn2)
import Flame (Html)
import Flame.HTML.Attribute as HA
import Flame.HTML.Element (class ToNode)
import Flame.HTML.Element as HE
import Flame.Renderer.Hook as FRH
import Flame.Types (NodeData, VNode)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record as R
import Shared.Avatar as SA
import Shared.DateTime as SDT
import Shared.Markdown as SM
import Shared.Setter as SS
import Shared.Unsafe as SU
import Type.Data.Symbol as TDS
import Web.Event.Internal.Types (Event)

foreign import focus :: EffectFn2 VNode VNode Unit

view :: ProfileModel -> Html ProfileMessage
view model@{
      user,
      countries,
      languages,
      descriptionInputed
} =
      HE.div (HA.class' "profile-edition suggestion contact") [
            HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/profile.css"],
            HE.div_ $ HE.img [HA.class' "avatar-profile-edition", HA.src $ SA.avatarForSender user.avatar, HA.onClick SelectAvatar],
            HE.input [HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"],

            displayEditName,
            displayEditHeadline,

            HE.div (HA.class' "profile-karma") [
                  HE.div_ [
                        HE.span [HA.class' "span-info"] $ show user.karma,
                        HE.span [HA.class' "duller"] " karma"
                  ]
            ],

            HE.div (HA.class' "profile-asl") [
                  displayEditAge,
                  displayEditGender,
                  displayEditCountry
            --       if isEditingLanguages then displayLanguages else editLanguages
            ],

            -- HE.div (HA.class' "profile-tags") $ if isEditingTags then displayTags else editTags,

            HE.div [title "description", HA.class' {hidden: DM.isJust descriptionInputed}, HA.onClick (copyFromField (SProxy :: SProxy "description") (SProxy :: SProxy "descriptionInputed"))] [
                  HE.div (HA.class' "about") [
                        HE.span (HA.class' "duller") "About",
                        pen
                  ],
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
                        HA.onInput (setFieldInputed (SProxy :: SProxy "descriptionInputed"))
                  ] $ DM.fromMaybe "" descriptionInputed,

                  HE.div (HA.class' "save-cancel") [
                        check SetDescription,
                        cancel (SProxy :: SProxy "descriptionInputed")
                  ]
            ]

            -- HE.input [HA.type' "button", HA.onClick SaveProfile, HA.value "Save profile", HA.class' "green-button end"]
      ]
      where displayEditName = displayEditGenerated SetName (SProxy :: SProxy "name") "This is the name other users will see when looking at your profile" 40
            displayEditHeadline = displayEditGenerated SetHeadline (SProxy :: SProxy "headline") "A tagline to draw attention to your profile" 100

            displayEditAge =
                  let   field = SProxy :: SProxy "age"
                        fieldInputed = TDS.append field (SProxy :: SProxy "Inputed")
                        currentFieldValue = map show <<< SDT.ageFrom $ map DN.unwrap user.age
                        parser = map DateWrapper <<< SDT.unformatISODate
                  in displayEditOptionalField field currentFieldValue $ HE.input [
                        HA.type' "date",
                        HA.class' "single-line-input",
                        HA.placeholder "yyyy-mm-dd",
                        HA.onInput (setFieldInputed fieldInputed <<< parser),
                        HA.value $ DM.fromMaybe "" currentFieldValue
                  ]
            displayEditGender = displayEditOptional DSR.read genders (SProxy :: SProxy "gender") (show <$> user.gender)
            displayEditCountry = displayEditOptional DI.fromString countries (SProxy :: SProxy "country") do
                  country <- user.country
                  map DT.snd $ DF.find ((country == _) <<< DT.fst) countries

            displayEditOptional :: forall r s t field fieldInputed. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Cons fieldInputed (Choice (Maybe t)) r PM => Cons field (Maybe t) s PU => Show t => Eq t => (String -> Maybe t) -> Array (Tuple t String) -> SProxy field -> Maybe String -> Html ProfileMessage
            displayEditOptional parser options field currentFieldValue =
                  let   fieldInputed = TDS.append field (SProxy :: SProxy "Inputed")
                  in displayEditOptionalField field currentFieldValue $ HE.select [HA.onInput (setFieldInputed fieldInputed <<< parser)] $ displayOptions (R.get field model.user) options

            displayEditOptionalField :: forall r s t field fieldInputed. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Cons fieldInputed (Choice (Maybe t)) r PM => Cons field (Maybe t) s PU => SProxy field -> Maybe String -> Html ProfileMessage -> Html ProfileMessage
            displayEditOptionalField field currentFieldValue control =
                  let   stringField = TDS.reflectSymbol field
                        fieldInputed = TDS.append field (SProxy :: SProxy "Inputed")
                        isEditing = DM.isJust $ R.get fieldInputed model
                  in HE.div (HA.class' { centered: isEditing }) [
                        HE.div [HA.class' { "profile-info-add": true, hidden: isEditing }, title stringField, HA.onClick (copyFromField field fieldInputed)] $
                              case currentFieldValue of
                                    Just value -> HE.div_ [
                                          HE.text value, pen
                                    ]
                                    _ -> HE.div (HA.class' "italics") [
                                          HE.text $ "Your " <> stringField,
                                          pen
                                    ],
                        HE.div (HA.class' { edit: true, hidden: not isEditing }) [
                              HE.div (HA.class' "bold") $ "Your " <> stringField,
                              control,
                              check (copyFromChoice field fieldInputed),
                              cancel fieldInputed
                        ]
                  ]

            displayEditGenerated :: forall r s field fieldInputed. IsSymbol field => Append field "Inputed" fieldInputed => IsSymbol fieldInputed => Cons fieldInputed (Maybe String) r PM => Cons field String s PU => ProfileMessage -> SProxy field -> String -> Int -> Html ProfileMessage
            displayEditGenerated message field explanation maxLength =
                  let   stringField = TDS.reflectSymbol field
                        fieldInputed = TDS.append field (SProxy :: SProxy "Inputed")
                        currentInputed = R.get fieldInputed model
                        isEditing = DM.isJust currentInputed
                  in HE.div_ [
                        HE.div [title stringField, HA.class' {"hidden": isEditing}, HA.onClick (copyFromField field fieldInputed)] [
                              HE.span [HA.class' $ "profile-edition-" <> stringField] [HE.text $ R.get field model.user],
                              pen
                        ],
                        HE.div (HA.class' { edition: true, hidden: not isEditing}) [
                              HE.div (HA.class' "bold") $ "Your " <> stringField,
                              HE.div (HA.class' "duller") [
                                    HE.text explanation,
                                    HE.br,
                                    HE.text "Leave it blank to generate a new random name"
                              ],
                              HE.input [
                                    FRH.atPostpatch focus,
                                    HA.class' "single-line-input",
                                    HA.maxlength maxLength,
                                    HA.onKeydown (exitEditGenerated message fieldInputed),
                                    HA.onInput (setFieldInputed fieldInputed),
                                    HA.value $ DM.fromMaybe "" currentInputed
                              ],
                              check message,
                              cancel fieldInputed
                        ]
                  ]
      -- where
      --       displayLanguages =
      --             display "languages" (SProxy :: SProxy "isEditingLanguages") $ case DSC.joinWith ", " $ map getLanguage user.languages of
      --                   "" -> Nothing
      --                   l -> Just $ "speaks " <> l
      --       displayTags =
      --             case user.tags of
      --                   [] -> display "tags" (SProxy :: SProxy "isEditingTags") Nothing
      --                   tags -> HE.div_ $ map (HE.span [HA.class' "tag", HA.onClick (setfieldInputed (SProxy :: SProxy "isEditingTags")), HA.title "Click to edit tags"]) tags
      --       editLanguages = HE.span_ ([
      --             HE.select [HA.onInput AddLanguage] $ displayOptionsWith "Select" Nothing languages
      --           ] <> map (\id -> tagEdition "language" RemoveLanguage <<< Tuple id $ getLanguage id) user.languages)
      --       editTags = HE.span_ ([
      --             HE.span_ "Add tags to show your interests, hobbies, etc ",
      --             HE.input [HA.type' "text", HA.onKeydown SetTagEnter, HA.autofocus true, HA.placeholder "Press enter to add", HA.maxlength 30]
      --          ] <> map (\tag -> tagEdition "tag" RemoveTag $ Tuple tag tag) user.tags)

      --       optionEntry n = Tuple n $ show n

      --       languageHM = DH.fromArray languages
      --       getLanguage = SU.fromJust <<< flip DH.lookup languageHM

copyFromField :: forall s t r field fieldInputed. IsSymbol field => Cons field t s PU => IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => SProxy field -> SProxy fieldInputed -> ProfileMessage
copyFromField field fieldInputed = SetPField (\model -> R.set fieldInputed (Just $ R.get field model.user) model)

copyFromChoice :: forall s t r field fieldInputed. IsSymbol field => Cons field (Maybe t) s PU => IsSymbol fieldInputed => Cons fieldInputed (Choice (Maybe t)) r PM => SProxy field -> SProxy fieldInputed -> ProfileMessage
copyFromChoice field fieldInputed = SetPField (\model -> R.set fieldInputed Nothing $ SS.setUserField field (DM.fromMaybe Nothing $ R.get fieldInputed model) model)

resetFieldInputed :: forall fieldInputed r t.  IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => SProxy fieldInputed  -> ProfileMessage
resetFieldInputed fieldInputed = SetPField (R.set fieldInputed Nothing)

setFieldInputed :: forall fieldInputed r t. IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => SProxy fieldInputed -> t -> ProfileMessage
setFieldInputed fieldInputed v = SetPField (R.set fieldInputed (Just v))

exitEditGenerated :: forall r field. IsSymbol field => Cons field (Maybe String) r PM => ProfileMessage -> SProxy field -> Tuple String String -> ProfileMessage
exitEditGenerated message fieldInputed (Tuple key _) =
      if key == "Enter" then
            message
       else if key == "Escape" then
            resetFieldInputed fieldInputed
       else
            SetPField identity

pen :: Html ProfileMessage
pen = HE.svg [HA.class' "svg-16 edit", HA.viewBox "0 0 512 512"] [
      HE.path' $ HA.d "M295.772,113.228,85.791,323.209,38.818,461.681a9,9,0,0,0,2.159,9.255l.087.087a9,9,0,0,0,9.255,2.159l138.472-46.973L398.772,216.228Z",
      HE.path' $ HA.d "M458.648,53.353h0a72.833,72.833,0,0,0-103,0l-34.42,34.42,103,103,34.42-34.42A72.832,72.832,0,0,0,458.648,53.353Z"
]

check :: ProfileMessage -> Html ProfileMessage
check message = HE.svg [HA.class' "svg-20 save", HA.viewBox "0 0 512 512", HA.onClick message] [
      HE.title "Save edition",
      HE.polygon' [HA.points "204 445.539 35.23 276.77 108.77 203.23 204 298.461 419.23 83.23 492.77 156.77 204 445.539"]
]

cancel :: forall r t fieldInputed. IsSymbol fieldInputed => Cons fieldInputed (Maybe t) r PM => SProxy fieldInputed -> Html ProfileMessage
cancel fieldInputed = HE.svg [HA.class' "svg-16 cancel", HA.viewBox "0 0 512 512", HA.onClick (resetFieldInputed fieldInputed) ] [
      HE.title "Cancel edition",
      HE.polygon' $ HA.points "438.627 118.627 393.373 73.373 256 210.746 118.627 73.373 73.373 118.627 210.746 256 73.373 393.373 118.627 438.627 256 301.254 393.373 438.627 438.627 393.373 301.254 256 438.627 118.627"
]

displayOptions :: forall id. Show id => Eq id => Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
displayOptions = displayOptionsWith "Don't show"

displayOptionsWith :: forall id. Show id => Eq id => String -> Maybe id -> Array (Tuple id String) -> Array (Html ProfileMessage)
displayOptionsWith unselectedText current = (HE.option [HA.selected $ DM.isNothing current] unselectedText : _) <<< map makeOptions
      where makeOptions (Tuple id value) = HE.option [HA.value $ show id, HA.selected $ Just id == current] value

-- tagEdition :: forall a. String -> (a -> Event -> ProfileMessage) -> Tuple a String -> Html ProfileMessage
-- tagEdition title message (Tuple id text) = HE.span [HA.onClick' (message id), HA.title $ "Click to remove " <> title, HA.class' "tag"] [
--       HE.text text,
--       HE.a (HA.class' "remove-tag") "x"
-- ]

title :: String -> NodeData ProfileMessage
title name = HA.title $ "Click to edit your " <> name

genders :: Array (Tuple Gender String)
genders = [Tuple Female $ show Female, Tuple Male $ show Male, Tuple NonBinary $ show NonBinary, Tuple Other $ show Other]
