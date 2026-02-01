module Shared.Profile.View where

import Prelude

import Client.Dom as CCD
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Debug (spy)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.Availability (Availability(..))
import Shared.Avatar (defaultAvatar)
import Shared.Avatar as SA
import Shared.Change as SC
import Shared.Change as SCN
import Shared.DateTime as DST
import Shared.DateTime as SDT
import Shared.Element (ElementId(..))
import Shared.Im.Svg (contextMenuElements)
import Shared.Im.Svg as SIA
import Shared.Im.View.Asks as SIVA
import Shared.Im.View.ChatInput as SICI
import Shared.Im.View.Posts as SIVPS
import Shared.Im.View.Profile (separator)
import Shared.Im.View.Profile as SIVP
import Shared.Intl as SI
import Shared.Keydown as SK
import Shared.Markdown as SM
import Shared.Network (RequestStatus(..))
import Shared.Options.Ask (maxAskCharacters)
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters, tagMaxCharacters)
import Shared.Profile.Types (ProfileMessage(..), ProfileMode(..), ProfileModel, What(..))
import Shared.Unsafe as SU
import Shared.User (Gender(..))

view ∷ ProfileModel → Html ProfileMessage
view model = HE.div [ HA.id $ show ProfileEditionForm]
      [ HE.div [ HA.onClick $ ToggleAskMenu Nothing, HA.class' { "profile-edition": true, hidden: not model.visible } ]
              ( [ HE.div [ HA.class' "green-tab" ]
                        [ HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Edit }, HA.onClick <<< SetPField $ _ { mode = Edit } ] [ HE.text "Edit" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Preview }, HA.onClick <<< SetPField $ _ { mode = Preview } ] [ HE.text "Preview" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == OwnPosts }, HA.onClick RefreshPosts ] [ HE.text "Posts" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Asked }, HA.onClick RefreshAsks ] [ HE.text $ unasweredAsks <> "Asks" ]
                        ]
                ] <>
                      if model.mode == Edit then
                            edit model
                      else if model.mode == Preview then
                            preview model
                      else if model.mode == OwnPosts then
                            posts model
                      else
                            asks model
              )
      ]
      where
      unasweredAsks =
            let count = DA.length $ DA.filter (DM.isNothing <<< _.answer) model.asks in if count == 0 then "" else "(" <> show count <> ") "

asks ∷ ProfileModel → Array (Html ProfileMessage)
asks model =
      [ HE.div [] <<< map unaswered $ DA.filter (DM.isNothing <<< _.answer) model.asks
      , HE.div [ HA.class' "ask-list" ] <<< map SIVA.asked $ DA.filter (DM.isJust <<< _.answer) model.asks

      ]
      where
      unaswered ask = HE.div_
            [ HE.text "MeroChat user asks: "
            , HE.b [] [ HE.i [] [ HE.text ask.question ] ]
            , HE.div [ HA.class' "ask-answer-form" ]
                    $
                          if model.loading then
                                [ HE.div' [ HA.class' "loading" ] ]
                          else
                                [ HE.div [ HA.class' "outer-user-menu" ]
                                        [ HE.svg [ HA.class' "svg-32", HA.viewBox "0 0 16 16", HA.id $ show AskContextMenu <> show ask.id, HA.onClick <<< ToggleAskMenu $ Just ask.id ] contextMenuElements
                                        , HE.div [ HA.class' { "ask-context-menu": true, hidden: model.contextMenuFor /= Just ask.id } ]
                                                [ HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick $ IgnoreAsk ask.id ] [ HE.text "Ignore" ]
                                                , HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick $ ReportAsk ask.id ask.asker ] [ HE.text "Report" ]
                                                ]
                                        ]
                                , HE.textarea'
                                        [ HA.class' "modal-input"
                                        , HA.placeholder "Type your answer"
                                        , HA.onInput' ResizeChatInput
                                        , SCN.onChange (SetAnswer ask.id <<< SCN.toMaybe)
                                        , HA.autocomplete "off"
                                        , HA.maxlength (maxAskCharacters * 2)
                                        ]
                                , HE.svg [ HA.class' "send-button svg-50", HA.viewBox "0 0 16 16", HA.onClick $ SendAnswer ask.id ] $ SICI.sendButtonElements "Answer"
                                ]
            ]

posts ∷ ProfileModel → Array (Html ProfileMessage)
posts model =
      [ HE.div [ HA.class' "posts" ] [ HE.div [ HA.class' "post-list" ] $ map (SIVPS.posted model.user.name) model.posts ]
      ]

edit ∷ ProfileModel → Array (Html ProfileMessage)
edit model =
      [ --likely a rendering bug on flame as it wont work with regular div
        HE.createElement "vid" [ HA.class' { "profile-section created-account": true, hidden: not model.fromTemporary } ] [ HE.text "Your account has been created" ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Avatar" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "Your profile picture" ]
              , HE.div [ HA.class' "fit", HA.onClick SelectAvatar ]
                      [ HE.img [ HA.class' "avatar-profile-edition", HA.src <<< DM.fromMaybe defaultAvatar $ SA.fromAvatarPath model.avatarInputed ]
                      , HE.input [ HA.id "avatar-file-input", HA.type' "file", HA.onChange' PrepareAvatar, HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp" ]
                      , HE.svg [ HA.class' "svg-16", HA.viewBox "0 0 16 16", HA.onClick <<< SetPField $ _ { avatarInputed = Nothing } ]
                              [ HE.title [ HE.text "Reset profile picture" ]
                              , HE.path' [ HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.5A7.5,7.5,0,1,1,15.5,8,7.5,7.5,0,0,1,8,15.5Z" ]
                              , HE.path' [ HA.class' "a", HA.d "M11.65,7.38H4.4a.62.62,0,1,0,0,1.24h7.25a.62.62,0,0,0,0-1.24Z" ]
                              ]
                      ]
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Basic info" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "All fields are optional" ]
              , HE.div [ HA.class' "profile-section-wedge" ]
                      [ HE.input [ SC.onChange SetAge, HA.class' "modal-input margined", HA.type' "date", HA.placeholder "Your age", HA.value <<< DM.fromMaybe "" $ map SDT.formatIsoDate model.ageInputed ]
                      , HE.select [ HA.class' "modal-select", HA.onInput SetGender ]
                              [ HE.option [ HA.value "", HA.selected $ model.genderInputed == Nothing ] [ HE.text "Do not show my gender" ]
                              , HE.option [ HA.value $ show Female, HA.selected $ model.genderInputed == Just Female ] [ HE.text $ show Female ]
                              , HE.option [ HA.value $ show Male, HA.selected $ model.genderInputed == Just Male ] [ HE.text $ show Male ]
                              , HE.option [ HA.value $ show NonBinary, HA.selected $ model.genderInputed == Just NonBinary ] [ HE.text $ show NonBinary ]
                              , HE.option [ HA.value $ show Other, HA.selected $ model.genderInputed == Just Other ] [ HE.text $ show Other ]
                              ]
                      ]
              , HE.div [ HA.class' "profile-section-wedge" ]
                      [ HE.select [ HA.class' "modal-select margined", HA.onInput SetCountry ]
                              ( HE.option [ HA.value "", HA.selected $ model.countryInputed == Nothing ] [ HE.text "Do not show my country" ]
                                      : map (\c → HE.option [ HA.value $ show c.id, HA.selected $ model.countryInputed == Just c.id ] [ HE.text c.name ]) model.countries
                              )
                      , HE.div [ HA.class' "profile-languages" ]
                              [ HE.select [ HA.class' "modal-select", HA.onInput SetLanguage ]
                                      ( HE.option [ HA.value "", HA.selected $ DA.null model.languagesInputed ] [ HE.text "Do not show my languages" ]
                                              : map (\c → HE.option [ HA.value $ show c.id, HA.selected (Just c.id == firstLanguage) ] [ HE.text c.name ]) model.languages
                                      )
                              , HE.div [ HA.class' { hidden: DA.null model.languagesInputed } ] $ map languageEntry model.languagesInputed
                              ]
                      ]
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Display name" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "Leave it blank for a new auto generated name" ]
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength nameMaxCharacters, HA.value $ DM.fromMaybe "" model.nameInputed, SC.onChange (SetAutoGenerated Name) ]
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Headline" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "An one liner to grab attention. Leave blank for autogenerated" ]
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength headlineMaxCharacters, HA.value $ DM.fromMaybe "" model.headlineInputed, SC.onChange (SetAutoGenerated Headline) ]
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Tags" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "Your interests, labels, things to talk about, etc" ]
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength tagMaxCharacters, HA.placeholder "Enter to add", SK.onEnter SetTag, enterKeyHint "enter" ]
              , HE.div [ HA.class' { hidden: DA.null model.tagsInputed } ] $ map tagEntry model.tagsInputed
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "About" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "Whatever you feel like saying. Leave blank for autogenerated" ]
              , HE.textarea [ HA.class' "modal-input", HA.maxlength descriptionMaxCharacters, HA.rows 7, HA.value $ DM.fromMaybe "" model.descriptionInputed, SC.onChange (SetAutoGenerated Description) ] [ HE.text $ DM.fromMaybe "" model.descriptionInputed ]
              ]
      , HE.div [ HA.class' "profile-section profile-buttons" ]
              [ HE.input [ HA.type' "button", HA.class' "cancel", HA.value "Discard", HA.onClick Clear ]
              , if model.loading then HE.div [ HA.class' "loading-over" ]
                      [ HE.div' [ HA.class' "loading" ]
                      ]
                else
                      HE.input [ HA.type' "button", HA.onClick Save, HA.class' "green-button bigger", HA.value "Save" ]
              , HE.span [ HA.class' { "request-error-message": true, hidden: model.updateRequestStatus == Nothing } ] [ HE.text $ DM.maybe "" unwrapFailure model.updateRequestStatus ]
              ]
      ]
      where
      firstLanguage = DA.head model.languagesInputed
      languages = DH.fromArrayBy _.id _.name model.languages
      languageEntry id = HE.div [ HA.class' "profile-selected-item", HA.title "Click to remove", HA.onClick <<< SetLanguage $ show id ] [ HE.text (SU.fromJust (DH.lookup id languages) <> " x ") ]

      tagEntry tag = HE.div [ HA.class' "profile-selected-item", HA.title "Click to remove", HA.onClick $ SetTag tag ] [ HE.text $ tag <> " x " ]

      unwrapFailure = case _ of
            Failure s → s
            _ → ""

      enterKeyHint = HA.createAttribute "enterkeyhint"

preview ∷ ProfileModel → Array (Html ProfileMessage)
preview model =
      [ HE.div [ HA.class' "avatar-info" ]
              [ HE.div [ HA.class' "big-avatar-info" ]
                      [ HE.img [ HA.src <<< DM.fromMaybe defaultAvatar $ SA.fromAvatarPath model.avatarInputed, HA.class' "big-suggestion-avatar" ]
                      , HE.div [ HA.class' "big-suggestion-info" ]
                              ( HE.strong [ HA.class' "big-card-name" ] [ HE.text $ DM.fromMaybe "" model.nameInputed ]
                                      : SIVP.badges model.user.badges <> [ HE.div [ HA.class' "duller" ] onlineStatus ]
                              )
                      , HE.div [ HA.class' "big-suggestion-info auto-left" ]
                              ( [ HE.div_
                                        [ HE.strong [ HA.class' "mini-suggestion-karma" ] [ HE.text $ SI.thousands model.user.karma ]
                                        , HE.span [ HA.class' "duller" ] [ HE.text $ " karma • #" <> show model.user.karmaPosition ]
                                        ]
                                ]
                                      <> genderAge
                                      <> from
                                      <> speaks
                              )
                      ]
              ]
      , HE.div [ HA.class' "full-card-headline-tags" ]
              ( [ HE.div [ HA.class' "card-headline" ] [ HE.text $ DM.fromMaybe "" model.headlineInputed ]
                , HE.hr' [ HA.class' "tag-ruler" ]
                ] <> map (HE.span [ HA.class' "tag" ] <<< DA.singleton <<< HE.text) model.tagsInputed <> [ HE.hr' [ HA.class' "tag-ruler" ] ]
              )
      , HE.div [ HA.class' "card-description", HA.title "See full profile" ]
              [ HE.span [ HA.class' "card-about-description" ] [ HE.text "About" ]
              , HE.div' [ HA.innerHtml <<< SM.parse $ DM.fromMaybe "" model.descriptionInputed ]
              ]
      ]
      where
      onlineStatus
            | not model.user.onlineStatus = []
            | otherwise = [ HE.span_ [ HE.text $ show Online ] ]
      genderAge =
            case DM.maybe [] (\c → [ HE.span_ [ HE.text $ show c ] ]) model.genderInputed <> DM.maybe [] (\c → [ HE.span_ [ HE.text <<< show $ DST.ageFrom c ] ]) model.ageInputed of
                  [ g, a ] → [ HE.div_ [ g, separator, a ] ]
                  ga → ga
      from = DM.maybe [] (\c → [ HE.div_ [ HE.span [ HA.class' "duller" ] [ HE.text "from " ], HE.span_ [ HE.text <<< _.name <<< SU.fromJust $ DA.find ((c == _) <<< _.id) model.countries ] ] ]) model.countryInputed
      speaks
            | DA.null model.languagesInputed = []
            | otherwise = [ HE.div_ $ HE.span [ HA.class' "duller" ] [ HE.text "speaks " ] : (DA.intersperse separator $ map languageEntry model.languagesInputed) ]
      languages = DH.fromArrayBy _.id _.name model.languages
      languageEntry id = HE.span_ [ HE.text <<< SU.fromJust $ DH.lookup id languages ]

