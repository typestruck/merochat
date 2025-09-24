module Shared.Im.View.Posts where

import Prelude

import Client.Common.Privilege as CCP
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Safe.Coerce as SC
import Shared.Change as SCN
import Shared.DateTime as SDT
import Shared.Im.Svg as SIS
import Shared.Im.Types (ImMessage(..), RetryableRequest(..), ShowPostForm(..), User, ImModel)
import Shared.Im.View.ChatInput as SIVC
import Shared.Markdown as SM
import Shared.Modal.Types (Modal(..), ScreenModal(..))
import Shared.Options.Post (maxPostCharacters)
import Shared.Post (Post)
import Shared.Privilege (Privilege(..))
import Shared.Privilege as SP

posted ∷ String → Post → Html ImMessage
posted userName post = HE.div [ HA.class' "post-entry" ]
      [ HE.div [ HA.class' "post-header" ] [ HE.div_ [ HE.text userName ], HE.div [ HA.class' "post-header-separator duller" ] [ HE.text " • " ], HE.div [ HA.class' "duller" ] [ HE.text <<< SDT.ago $ SC.coerce post.date ] ]
      , HE.div' [ HA.class' "post-content", HA.innerHtml $ SM.parse post.content ]
      ]

postForm ∷ ImModel → Array (Html ImMessage)
postForm model =
      [ SIS.closeX [ HA.onClick $ TogglePostForm NoPostForm ]
      , HE.strong [ HA.class' "bottom" ] [ HE.text "Post to MeroChat" ]
      , HE.div [ HA.class' "posts-input-tab" ]
              [ HE.div [ HA.class' "regular-posts-input-tab selected-posts-input-tab" ] [ textIcont, HE.text "Text" ]
              , HE.div [ HA.class' "regular-posts-input-tab" ] [ linkIcon, HE.text "Link" ]
              , HE.div [ HA.class' "regular-posts-input-tab" ] [ HE.svg [ HA.class' "post-input-tab-svg", HA.viewBox "0 0 16 16" ] $ SIVC.imageButtonElements "", HE.text " Image" ]
              ]

      , HE.textarea'
              [ HA.class' "chat-input"
              , HA.placeholder ("What's in your mind?")
              , HA.maxlength maxPostCharacters
              , HA.onInput' ResizeChatInput
              , SCN.onChange (SetPostContent <<< Just)
              , HA.autocomplete "off"
              , HA.value $ DM.fromMaybe "" model.postContent
              ]
      , HE.div [ HA.class' "see-profile-chat posted" ]
              [ if not model.freeToPost then
                      HE.div' [ HA.class' "loading" ]
                else if model.user.totalPosts == 0 then
                      HE.input [ HA.disabled $ DM.isNothing model.postContent, HA.type' "button", HA.class' "green-button post-button build", HA.value "Post", HA.onClick SendPost ]
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