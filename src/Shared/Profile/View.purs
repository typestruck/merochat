module Shared.Profile.View where

import Prelude

import Data.Array ((:))
import Data.Array as DA
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Shared.Modal (Modal(..))
import Shared.Modal as SM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Countries as SCO
import Shared.Languages as SL
import Shared.Availability (Availability(..))
import Shared.Avatar (defaultAvatar)
import Shared.Avatar as SA
import Shared.Change as SC
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Extra as SE
import Shared.Im.Svg (contextMenuElements)
import Shared.Im.View.Asks as SIVA
import Shared.Im.View.ChatInput as SIVC
import Shared.Im.View.Posts as SIVP
import Shared.Im.View.Praise as SIVP
import Shared.Im.View.Profile (separator)
import Shared.Im.View.Profile as SIVP
import Shared.Intl as SI
import Shared.Keydown as SK
import Shared.Markdown as SM
import Shared.Network (RequestStatus(..))
import Shared.Options.Ask (maxAskCharacters)
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters, tagMaxCharacters)
import Shared.Profile.Mode (ProfileMode(..))
import Shared.Profile.Types (ProfileMessage(..), ProfileModel, What(..))
import Shared.Unsafe as SU
import Shared.User (Gender(..))

view ∷ ProfileModel → Html ProfileMessage
view model = HE.div [ HA.id $ show ProfileEditionForm ]
      [ HE.div [ HA.onClick $ ToggleAskMenu Nothing, HA.class' { "profile-edition": true, hidden: not model.visible } ]
              ( [ SM.header "Profile" $ ToggleVisibility HideModal
                  , HE.div [ HA.class' "green-tab lynx-inline" ]
                        [ HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Edit }, HA.onClick <<< SetPField $ _ { mode = Edit } ] [ HE.text "Edit" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Preview }, HA.onClick <<< SetPField $ _ { mode = Preview } ] [ HE.text "Preview" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Praise }, HA.onClick RefreshPraise ] [ HE.text "Praise" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == OwnPosts }, HA.onClick RefreshPosts ] [ HE.text "Posts" ]
                        , HE.div [ HA.class' { "regular-green-tab": true, "selected-green-tab": model.mode == Asked }, HA.onClick RefreshAsks ] [ HE.text $ unasweredAsks <> "Asks" ]
                        ]
                ] <>
                      if model.mode == Edit then
                            edit model
                      else if model.mode == Preview then
                            preview model
                      else if model.mode == Praise then
                            praises model
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
asks model = case model.asks of
      [] → [ HE.div_ [ HE.text "You haven't received any questions yet" ] ]
      _ →
            [ HE.div [] <<< map unaswered $ DA.filter (DM.isNothing <<< _.answer) model.asks
            , HE.div [ HA.class' "ask-list" ] <<< map SIVA.asked $ DA.filter (DM.isJust <<< _.answer) model.asks
            ]
      where
      unaswered ask = HE.div_
            [ HE.text "MeroChat user asks: "
            , HE.b [ HA.class' "ask-q" ] [ HE.i [] [ HE.text ask.question ] ]
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
                                        , SC.onChange (SetAnswer ask.id <<< SC.toMaybe)
                                        , HA.autocomplete "off"
                                        , HA.maxlength (maxAskCharacters * 2)
                                        ]
                                , HE.svg [ HA.class' "send-button svg-50", HA.viewBox "0 0 16 16", HA.onClick $ SendAnswer ask.id ] $ SIVC.sendButtonElements "Answer"
                                ]
            ]

posts ∷ ProfileModel → Array (Html ProfileMessage)
posts model = case model.posts of
      [] → [ HE.div [ HA.class' "posts" ] [ HE.text "You haven't posted yet" ] ]
      pts →
            [ HE.div [ HA.class' "posts" ] [ HE.div [ HA.class' "post-list" ] $ map (SIVP.posted model.user.name) pts ]
            ]

praises ∷ ProfileModel → Array (Html ProfileMessage)
praises model = case model.praise of
      [] → [ HE.div [ HA.class' "praise" ] [ HE.text "You haven't received any praise yet" ] ]
      prs → [ HE.div [ HA.class' "praise" ] [ HE.div [ HA.class' "ask-list" ] <<< SIVP.praised $ map _.content prs ] ]

edit ∷ ProfileModel → Array (Html ProfileMessage)
edit model =
      [ HE.div [ HA.class' { "profile-section created-account": true, hidden: not model.fromTemporary } ] [ HE.text "Your account has been created" ]
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
                      [ HE.input [ SC.onChange SetAge, HA.class' "modal-input margined", HA.type' "text", HA.placeholder "Birthday (YYYY-MM-DD)", HA.value <<< DM.fromMaybe "" $ map SD.formatIsoDate model.ageInputed, HA.createAttribute "default-value" <<< DM.fromMaybe "" $ map SD.formatIsoDate model.ageInputed ]
                      , dropdown model "gender" (DM.maybe "Do not show my gender" show model.genderInputed) SetGender
                              ({ value: "", name: "Do not show my gender" } : map (\gender → { value: show gender, name: show gender }) [ Female, Male, NonBinary, Other ])
                      ]
              , dropdown model "country" (DM.fromMaybe "Do not show my country" $ model.countryInputed >>= flip DH.lookup SCO.countries) SetCountry
                      ({ value: "", name: "Do not show my country" } : map (\country → { value: show country.id, name: country.name }) SCO.options)
              , dropdown model "language" "Choose a language" SetLanguage
                      ({ value: "", name: "Do not show my languages" } : map (\language → { value: show language.id, name: language.name }) SL.options)
              , HE.div [ HA.class' { hidden: DA.null model.languagesInputed } ] $ map languageEntry model.languagesInputed
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Display name" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "Leave it blank for a new auto generated name" ]
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength nameMaxCharacters, HA.value $ DM.fromMaybe "" model.nameInputed, HA.createAttribute "default-value" $ DM.fromMaybe "" model.nameInputed, SC.onChange <<< SetAutoGenerated $ Name ]
              ]
      , HE.div [ HA.class' "profile-section" ]
              [ HE.div [ HA.class' "profile-section-label" ] [ HE.text "Headline" ]
              , HE.div [ HA.class' "profile-section-label-smaller" ] [ HE.text "An one liner to grab attention. Leave blank for autogenerated" ]
              , HE.input [ HA.class' "modal-input", HA.type' "text", HA.maxlength headlineMaxCharacters, HA.value $ DM.fromMaybe "" model.headlineInputed, HA.createAttribute "default-value" $ DM.fromMaybe "" model.headlineInputed, SC.onChange <<< SetAutoGenerated $ Headline ]
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
              , HE.textarea' [ HA.class' "modal-input", HA.maxlength descriptionMaxCharacters, HA.rows 7, HA.value $ DM.fromMaybe "" model.descriptionInputed, HA.createAttribute "default-value" $ DM.fromMaybe "" model.descriptionInputed, SC.onChange <<< SetAutoGenerated $ Description ]
              ]
      , HE.div [ HA.class' "profile-section profile-buttons" ]
              [ HE.button [ HA.class' "cancel", HA.onClick Clear ] [ HE.text "Discard" ]
              , if model.loading then HE.div [ HA.class' "loading-over" ]
                      [ HE.div' [ HA.class' "loading" ]
                      ]
                else
                      HE.button [ HA.onClick Save, HA.class' "green-button bigger" ] [ HE.text "Save" ]
              , HE.span [ HA.class' { "request-error-message": true, hidden: model.updateRequestStatus == Nothing } ] [ HE.text $ DM.maybe "" unwrapFailure model.updateRequestStatus ]
              ]
      ]
      where
      languages = SL.languages
      languageEntry id = HE.div [ HA.class' "profile-selected-item", HA.title "Click to remove", HA.onClick <<< SetLanguage $ show id ] [ HE.text $ DM.fromMaybe "" (DH.lookup id languages) <> " x " ]

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
                              ( [ HE.span [ HA.class' "lynx-inline" ] $ HE.strong [ HA.class' "big-card-name" ] [ HE.text $ DM.fromMaybe "" model.nameInputed ]
                                        : SIVP.badges model.user.badges
                                ] <> [ HE.div [ HA.class' "duller" ] onlineStatus ]
                              )
                      , HE.div [ HA.class' "big-suggestion-info auto-left" ]
                              ( [ HE.div [ HA.class' "lynx-inline" ]
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
                , SE.hr [ HA.class' "tag-ruler" ]
                ] <> [ HE.span [ HA.class' "lynx-inline" ] $ map (HE.span [ HA.class' "tag" ] <<< DA.singleton <<< HE.text) model.tagsInputed ] <> [ SE.hr [ HA.class' "tag-ruler" ] ]
              )
      , HE.div [ HA.class' "card-description", HA.title "See full profile" ]
              [ HE.span [ HA.class' "card-about-description" ] [ HE.text "About" ]
              , SM.markdown [] $ DM.fromMaybe "" model.descriptionInputed
              ]
      ]
      where
      onlineStatus
            | not model.user.onlineStatus = []
            | otherwise = [ HE.span_ [ HE.text $ show Online ] ]
      genderAge =
            case DM.maybe [] (\c → [ HE.span_ [ HE.text $ show c ] ]) model.genderInputed <> DM.maybe [] (\c → [ HE.span_ [ HE.text <<< show $ SD.ageFrom c ] ]) model.ageInputed of
                  [ g, a ] → [ HE.div [ HA.class' "lynx-inline" ] [ g, separator, a ] ]
                  ga → ga
      from = DM.maybe [] (\c → [ HE.div [ HA.class' "lynx-inline" ] [ HE.span [ HA.class' "duller" ] [ HE.text "from " ], HE.span_ [ HE.text <<< DM.fromMaybe "" $ DH.lookup c SCO.countries ] ] ]) model.countryInputed
      speaks
            | DA.null model.languagesInputed = []
            | otherwise = [ HE.div [ HA.class' "lynx-inline" ] $ HE.span [ HA.class' "duller" ] [ HE.text "speaks " ] : (DA.intersperse separator $ map languageEntry model.languagesInputed) ]
      languages = SL.languages
      languageEntry id = HE.span_ [ HE.text <<< DM.fromMaybe "" $ DH.lookup id languages ]


dropdown ∷ ProfileModel → String → String → (String → ProfileMessage) → Array { value ∷ String, name ∷ String } → Html ProfileMessage
dropdown model key label select options = HE.div [ HA.class' "profile-choice" ]
      [ HE.button [ HA.class' "modal-select", HA.onClick <<< SetPField $ _ { choice = if model.choice == Just key then Nothing else Just key } ] [ HE.text label ]
      , if model.choice == Just key then
              HE.scrollView [ HA.class' "profile-choice-options", HA.scrollOrientation "vertical" ] $
                    map (\option → HE.button [ HA.onClick $ select option.value ] [ HE.text option.name ]) options
        else HE.div_ []
      ]
