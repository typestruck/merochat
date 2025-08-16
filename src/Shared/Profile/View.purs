module Shared.Profile.View where

import Prelude

import Client.Common.Dom as CCD
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Availability (Availability(..))
import Shared.Avatar (defaultAvatar)
import Shared.Avatar as SA
import Shared.DateTime as SDT
import Shared.Element (ElementId(..))
import Shared.Im.View.Profile (separator)
import Shared.Im.View.Profile as SIVP
import Shared.Intl as SI
import Shared.Keydown as SK
import Shared.Markdown as SM
import Shared.Network (RequestStatus(..))
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters, tagMaxCharacters)
import Shared.Profile.Types (ProfileMessage(..), ProfileMode(..), ProfileModel, What(..))
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource (resourcePath) as SP
import Shared.Unsafe as SU
import Shared.User (Gender(..))
import Web.DOM.Element as WDE
import Web.Event.Event as WEE

view ∷ ProfileModel → Html ProfileMessage
view model = HE.div (show ProfileEditionForm)
      [ HE.div [ HA.class' { "profile-edition": true, hidden: not model.visible } ]
              [ HE.div (HA.class' "profile-section tabbed")
                      [ HE.div [ HA.class' { c: true, "profile-mode-selected": model.mode == Edit }, HA.onClick <<< SetPField $ _ { mode = Edit } ] "Edit"
                      , HE.div (HA.class' "separator duller") "•"
                      , HE.div [ HA.class' { c: true, "profile-mode-selected": model.mode == Preview }, HA.onClick <<< SetPField $ _ { mode = Preview } ] "Preview"
                      ]
              , HE.fragment $ if model.mode == Edit then edit model else preview model
              ]
      ]

edit ∷ ProfileModel → Array (Html ProfileMessage)
edit model =
      [ HE.div (HA.class' "profile-section")
              [ HE.div (HA.class' "profile-section-label") "Avatar"
              , HE.div (HA.class' "profile-section-label-smaller") "Your display picture"
              , HE.div [ HA.class' "fit", HA.onClick SelectAvatar ]
                      [ HE.img [ HA.class' "avatar-profile-edition", HA.src <<< DM.fromMaybe defaultAvatar $ fromAvatar model.avatarInputed ]
                      , HE.input [ HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp" ]
                      , HE.svg [ HA.class' "svg-16", HA.viewBox "0 0 16 16", HA.onClick <<< SetPField $ _ { avatarInputed = Nothing } ]
                              [ HE.title "Reset profile picture"
                              , HE.path' [ HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.5A7.5,7.5,0,1,1,15.5,8,7.5,7.5,0,0,1,8,15.5Z" ]
                              , HE.path' [ HA.class' "a", HA.d "M11.65,7.38H4.4a.62.62,0,1,0,0,1.24h7.25a.62.62,0,0,0,0-1.24Z" ]
                              ]
                      ]
              ]
      , HE.div (HA.class' "profile-section")
              [ HE.div (HA.class' "profile-section-label") "Basic info"
              , HE.div (HA.class' "profile-section-label-smaller") "All fields are optional"
              , HE.div (HA.class' "profile-section-wedge")
                      [ HE.input [ onChange SetAge, HA.class' "modal-input margined", HA.type' "date", HA.placeholder "Your age", HA.value <<< DM.fromMaybe "" $ map SDT.formatIsoDate model.ageInputed ]
                      , HE.select [ HA.class' "modal-select", HA.onInput SetGender ]
                              [ HE.option [ HA.value "", HA.selected $ model.genderInputed == Nothing ] "Do not show my gender"
                              , HE.option [ HA.value $ show Female, HA.selected $ model.genderInputed == Just Female ] $ show Female
                              , HE.option [ HA.value $ show Male, HA.selected $ model.genderInputed == Just Male ] $ show Male
                              , HE.option [ HA.value $ show NonBinary, HA.selected $ model.genderInputed == Just NonBinary ] $ show NonBinary
                              , HE.option [ HA.value $ show Other, HA.selected $ model.genderInputed == Just Other ] $ show Other
                              ]
                      ]
              , HE.div (HA.class' "profile-section-wedge")
                      [ HE.select [ HA.class' "modal-select margined", HA.onInput SetCountry ]
                              ( HE.option [ HA.value "", HA.selected $ model.countryInputed == Nothing ] "Do not show my country"
                                      : map (\c → HE.option [ HA.value $ show c.id, HA.selected $ model.countryInputed == Just c.id ] c.name) model.countries
                              )
                      , HE.div (HA.class' "profile-languages")
                              [ HE.select [ HA.class' "modal-select", HA.onInput SetLanguage ]
                                      ( HE.option [ HA.value "", HA.selected $ DA.null model.languagesInputed ] "Do not show my languages"
                                              : map (\c → HE.option [ HA.value $ show c.id, HA.selected (Just c.id == firstLanguage) ] c.name) model.languages
                                      )
                              , HE.div (HA.class' { hidden: DA.null model.languagesInputed }) $ map languageEntry model.languagesInputed
                              ]
                      ]
              ]
      , HE.div (HA.class' "profile-section")
              [ HE.div (HA.class' "profile-section-label") "Display name"
              , HE.div (HA.class' "profile-section-label-smaller") "Leave it blank for a new auto generated name"
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength nameMaxCharacters, HA.value $ DM.fromMaybe "" model.nameInputed, onChange (SetAutoGenerated Name) ]
              ]
      , HE.div (HA.class' "profile-section")
              [ HE.div (HA.class' "profile-section-label") "Headline"
              , HE.div (HA.class' "profile-section-label-smaller") "An one liner to grab attention. Leave blank for autogenerated"
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength headlineMaxCharacters, HA.value $ DM.fromMaybe "" model.headlineInputed, onChange (SetAutoGenerated Headline) ]
              ]
      , HE.div (HA.class' "profile-section")
              [ HE.div (HA.class' "profile-section-label") "Tags"
              , HE.div (HA.class' "profile-section-label-smaller") "Your interests, labels, things to talk about, etc"
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength tagMaxCharacters, HA.placeholder "Enter to add", SK.onEnter SetTag ]
              , HE.div (HA.class' { hidden: DA.null model.tagsInputed }) $ map tagEntry model.tagsInputed
              ]
      , HE.div (HA.class' "profile-section")
              [ HE.div (HA.class' "profile-section-label") "About"
              , HE.div (HA.class' "profile-section-label-smaller") "Whatever you feel like saying. Leave blank for autogenerated"
              , HE.textarea [ HA.class' "modal-input", HA.maxlength descriptionMaxCharacters, HA.rows 7, HA.value $ DM.fromMaybe "" model.descriptionInputed, onChange (SetAutoGenerated Description) ] $ DM.fromMaybe "" model.descriptionInputed
              ]
      , HE.div (HA.class' "profile-section profile-buttons")
              [ HE.input [ HA.type' "button", HA.class' "cancel", HA.value "Discard" ]
              , if model.loading then HE.div (HA.class' "loading-over")
                      [ HE.div' (HA.class' "loading")
                      ]
                else
                      HE.input [ HA.type' "button", HA.onClick Save, HA.class' "green-button bigger", HA.value "Save" ]
              , HE.span (HA.class' { "request-error-message": true, hidden: model.updateRequestStatus == Nothing }) $ DM.maybe "" unwrapFailure model.updateRequestStatus
              ]
      ]
      where
      firstLanguage = DA.head model.languagesInputed
      languages = DH.fromArrayBy _.id _.name model.languages
      languageEntry id = HE.div [ HA.class' "profile-selected-item", HA.title "Click to remove", HA.onClick <<< SetLanguage $ show id ] (SU.fromJust (DH.lookup id languages) <> " x ")

      tagEntry tag = HE.div [ HA.class' "profile-selected-item", HA.title "Click to remove", HA.onClick $ SetTag tag ] (tag <> " x ")

      unwrapFailure = case _ of
            Failure s → s
            _ → ""

preview ∷ ProfileModel → Array (Html ProfileMessage)
preview model =
      [ HE.div (HA.class' "avatar-info")
              [ HE.div [ HA.class' "big-avatar-info" ]
                      [ HE.img [ HA.src <<< DM.fromMaybe defaultAvatar $ fromAvatar model.avatarInputed, HA.class' "big-suggestion-avatar" ]
                      , HE.div (HA.class' "big-suggestion-info")
                              ( HE.strong (HA.class' "big-card-name") (DM.fromMaybe "" model.nameInputed)
                                      : SIVP.badges model.user.badges <> [ HE.div (HA.class' "duller") onlineStatus  ]
                              )
                      , HE.div (HA.class' "big-suggestion-info auto-left")
                              ( [ HE.div_
                                        [ HE.strong (HA.class' "mini-suggestion-karma") $ SI.thousands model.user.karma
                                        , HE.span (HA.class' "duller") $ " karma • #" <> show model.user.karmaPosition
                                        ]
                                ]
                                      <> genderAge
                                      <> from
                                      <> speaks
                              )
                      ]
              ]
      , HE.div (HA.class' "full-card-headline-tags")
              ( [ HE.div (HA.class' "card-headline") $ DM.fromMaybe "" model.headlineInputed
                , HE.hr' (HA.class' "tag-ruler")
                ] <> map (HE.span (HA.class' "tag")) model.tagsInputed <> [ HE.hr' (HA.class' "tag-ruler") ]
              )
      , HE.div [ HA.class' "card-description", HA.title "See full profile" ]
              [ HE.span (HA.class' "card-about-description") "About"
              , HE.div' [ HA.innerHtml <<< SM.parse $ DM.fromMaybe "" model.descriptionInputed ]
              ]
      ]
      where
      onlineStatus
            | not model.user.onlineStatus = []
            | otherwise = [ HE.span_ $ show Online ]
      genderAge =
            case DM.maybe [] (DA.singleton <<< HE.span_ <<< show) model.genderInputed <> DM.maybe [] (DA.singleton <<< HE.span_ <<< show) model.ageInputed of
                  [ g, a ] → [ HE.div_ [ g, separator, a ] ]
                  ga → ga
      from = DM.maybe [] (\c → [ HE.div_ [ HE.span (HA.class' "duller") "from ", HE.span_ <<< _.name <<< SU.fromJust $ DA.find ( (c == _) <<< _.id ) model.countries ] ]) model.countryInputed
      speaks
            | DA.null model.languagesInputed = []
            | otherwise = [ HE.div_ $ HE.span (HA.class' "duller") "speaks " : (DA.intersperse separator $ map languageEntry model.languagesInputed) ]
      languages = DH.fromArrayBy _.id _.name model.languages
      languageEntry id = HE.span_ <<< SU.fromJust $ DH.lookup id languages

fromAvatar = case _ of
      Just a | DS.contains (Pattern "data:image") a → Just a
      Just aa → Just $ SP.resourcePath (Left $ Upload aa) Ignore
      aaa → aaa

onChange ∷ (String → ProfileMessage) → NodeData ProfileMessage
onChange message = HA.createRawEvent "change" handler
      where
      handler event = do
            value ← CCD.value $ SU.fromJust do
                  target ← WEE.target event
                  WDE.fromEventTarget target
            pure <<< Just $ message value

