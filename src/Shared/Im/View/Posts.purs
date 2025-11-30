module Shared.Im.View.Posts where

import Prelude

import Client.Privilege as CCP
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol as TDS
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Change as SCN
import Shared.DateTime as SDT
import Shared.Im.Svg as SIS
import Shared.Im.Types (ImMessage(..), ImModel, PostMode(..), RetryableRequest(..))
import Shared.Im.View.ChatInput as SIVC
import Shared.Markdown as SM
import Shared.Modal (Modal(..), ScreenModal(..), SpecialModal(..))
import Shared.Options.Post (maxPostCharacters)
import Shared.Post (Post)
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP
import Shared.Resource (maxImageSizeKB)
import Type.Proxy (Proxy(..))

posted ∷ ∀ message. String → Post → Html message
posted userName post = HE.div [ HA.class' "post-entry" ]
      [ HE.div [ HA.class' "post-header" ] [ HE.div_ [ HE.text userName ], HE.div [ HA.class' "post-header-separator duller" ] [ HE.text " • " ], HE.div [ HA.class' "duller" ] [ HE.text <<< SDT.ago $ SC.coerce post.date ] ]
      , HE.div' [ HA.class' "post-content", HA.innerHtml $ SM.parse post.content ]
      ]

postForm ∷ ImModel → Array (Html ImMessage)
postForm model =
      [ SIS.closeX [ HA.onClick $ if model.modal == Special ShowPostForm then SpecialRequest (ToggleModal HideModal) else ToggleSuggestionPostForm ]
      , HE.strong [ HA.class' "bottom" ] [ HE.text "Post to MeroChat" ]
      , HE.div [ HA.class' "posts-input-tab" ]
              [ HE.div [ HA.onClick $ SetPostMode TextOnly, HA.class' { "regular-posts-input-tab": true, "selected-posts-input-tab": model.posts.mode == TextOnly } ] [ textIcont, HE.text "Text" ]
              , HE.div [ HA.onClick $ SetPostMode LinkOnly, HA.class' { "regular-posts-input-tab": true, "selected-posts-input-tab": model.posts.mode == LinkOnly } ] [ linkIcon, HE.text "Link" ]
              , HE.div [ HA.onClick $ SetPostMode ImageOnly, HA.class' { "regular-posts-input-tab": true, "selected-posts-input-tab": model.posts.mode == ImageOnly } ] [ HE.svg [ HA.class' "post-input-tab-svg", HA.viewBox "0 0 16 16" ] $ SIVC.imageButtonElements "", HE.text " Image" ]
              ]

      , case model.posts.mode of
              TextOnly → HE.textarea'
                    [ HA.class' "chat-input"
                    , HA.placeholder "What's in your mind?"
                    , HA.maxlength maxPostCharacters
                    , HA.onInput' ResizeChatInput
                    , SCN.onChange (SetPostText <<< SCN.toMaybe)
                    , HA.autocomplete "off"
                    , HA.value $ DM.fromMaybe "" model.posts.text
                    ]
              LinkOnly → HE.div []
                    [ if SP.hasPrivilege SendLinks model.user then
                            HE.div [ HA.class' "post-links" ]
                                  [ HE.input
                                          [ HA.class' "chat-input"
                                          , HA.maxlength maxPostCharacters
                                          , SCN.onChange (SetPostLink <<< SCN.toMaybe)
                                          , HA.value $ DM.fromMaybe "" model.posts.link
                                          , HA.placeholder "Link (required)"
                                          ]
                                  , HE.input
                                          [ HA.class' "chat-input"
                                          , HA.value $ DM.fromMaybe "" model.posts.caption
                                          , HA.placeholder "Caption"
                                          , HA.maxlength maxPostCharacters
                                          , SCN.onChange (SetPostCaption <<< SCN.toMaybe)
                                          ]
                                  ]
                      else
                            CCP.notEnoughKarma "post links" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
                    ]
              ImageOnly → HE.div []
                    [ if SP.hasPrivilege SendImages model.user then
                            HE.div [ HA.class' "post-links" ]
                                  [ if DA.elem (TDS.reflectSymbol (Proxy ∷ _ "posts")) model.erroredFields then
                                          HE.div [ HA.class' "error-message" ] [ HE.text $ "Image is larger than the " <> maxImageSizeKB <> " limit. Please select a different file" ]
                                    else if DM.isJust model.posts.image then
                                          HE.img [ HA.src $ DM.maybe "" _.base64 model.posts.image ]
                                    else
                                          HE.input
                                                [ HA.onChange' PreparePostImage
                                                , HA.type' "file"
                                                , HA.value ""
                                                , HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp"
                                                ]
                                  , HE.input
                                          [ HA.class' "chat-input"
                                          , HA.value $ DM.fromMaybe "" model.posts.caption
                                          , HA.placeholder "Caption"
                                          , HA.maxlength maxPostCharacters
                                          , SCN.onChange (SetPostCaption <<< SCN.toMaybe)
                                          ]
                                  ]
                      else
                            CCP.notEnoughKarma "post images" (SpecialRequest <<< ToggleModal $ Screen ShowKarmaPrivileges)
                    ]
      , HE.div [ HA.class' "see-profile-chat posted" ]
              [ if not model.posts.freeToSend then
                      HE.div' [ HA.class' "loading" ]
                else if not model.showSuggestionsPostForm || model.user.totalPosts == 0 then
                      HE.input [ HA.disabled $ DM.isNothing model.posts.text && DM.isNothing model.posts.link && DM.isNothing model.posts.image, HA.type' "button", HA.class' "green-button post-button build", HA.value "Post", HA.onClick SendPost ]
                else
                      HE.span_ [ HE.text "Posted!" ]
              ]

      ]

linkIcon ∷ Html ImMessage
linkIcon = HE.svg [ HA.class' "post-input-tab-svg", HA.viewBox "0 0 1024 1024" ]
      [ HE.path' [ HA.d "M451.2 598.4c-9.6 0-16-3.2-22.4-9.6-38.4-38.4-60.8-89.6-60.8-144s22.4-105.6 60.8-144l182.4-182.4c38.4-38.4 89.6-60.8 144-60.8 54.4 0 105.6 22.4 144 60.8 80 80 80 208 0 288l-83.2 83.2c-12.8 12.8-32 12.8-44.8 0s-12.8-32 0-44.8l83.2-83.2c54.4-54.4 54.4-144 0-198.4-25.6-25.6-60.8-41.6-99.2-41.6-38.4 0-73.6 16-99.2 41.6l-182.4 182.4c-25.6 25.6-41.6 60.8-41.6 99.2s16 73.6 41.6 99.2c12.8 12.8 12.8 32 0 44.8-6.4 6.4-12.8 9.6-22.4 9.6z" ]
      , HE.path' [ HA.d "M268.8 953.6c-51.2 0-105.6-19.2-144-60.8C86.4 854.4 64 803.2 64 748.8c0-54.4 22.4-105.6 60.8-144l83.2-83.2c12.8-12.8 32-12.8 44.8 0s12.8 32 0 44.8l-83.2 83.2c-25.6 25.6-41.6 60.8-41.6 99.2 0 38.4 16 73.6 41.6 99.2 54.4 54.4 144 54.4 198.4 0l182.4-182.4c25.6-25.6 41.6-60.8 41.6-99.2 0-38.4-16-73.6-41.6-99.2-12.8-12.8-12.8-32 0-44.8s32-12.8 44.8 0c38.4 38.4 60.8 89.6 60.8 144 0 54.4-22.4 105.6-60.8 144l-182.4 182.4c-38.4 38.4-92.8 60.8-144 60.8z" ]
      ]

textIcont ∷ Html ImMessage
textIcont = HE.svg [ HA.class' "post-input-tab-svg", HA.viewBox "0 0 1024 1024" ]
      [ HE.path' [ HA.d "M704 896H602.496l-72.512-210.24H233.856L165.376 896H64L334.848 128h98.24L704 896zM501.44 598.976L394.048 279.68c-3.392-10.176-7.04-28.096-11.008-53.504h-2.304c-3.392 23.168-7.232 40.96-11.456 53.504L262.72 598.976h238.72zM735.04 604.48C766.72 585.472 803.328 576 844.736 576c76.8 0 115.2 38.656 115.2 116.032v196.736h-55.872v-47.232h-1.408c-22.08 36.352-54.592 54.464-97.6 54.464-30.976 0-55.488-8-73.728-24.064-18.24-16-27.392-37.696-27.392-64.896 0-57.216 35.264-90.368 105.664-99.84l94.464-12.672c0-49.92-21.504-74.88-64.512-74.88-38.208 0-73.024 12.288-104.512 36.992v-52.16z m94.464 140.672c-26.112 3.328-44.096 9.536-54.016 18.816-9.92 9.216-14.784 22.08-14.784 38.656 0 14.528 5.376 26.368 16.128 35.648 10.752 9.216 24.96 13.824 42.56 13.824 24.64 0 44.864-8.32 60.8-24.896 15.872-16.576 23.872-37.376 23.872-62.464v-29.312l-74.56 9.728z" ]
      ]