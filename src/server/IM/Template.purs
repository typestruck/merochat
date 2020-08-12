module Server.IM.Template where

import Prelude
import Shared.IM.Types

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (QuerySelector(..))
import Flame as F
import Flame.HTML.Attribute as HA
import Flame.HTML.Element as HE
import Server.Template (defaultParameters)
import Server.Template as ST
import Shared.IM.View as SIV
import Shared.PrimaryKey as SP

template :: {
      contacts :: Array Contact,
      suggestions :: Array Suggestion,
      user :: IMUser
} -> Effect String
template {contacts, suggestions, user} = do
      let parameters = defaultParameters {
            javascript = javascript,
            css = css
      }
      F.preMount (QuerySelector ".im") {
            view: \model' -> ST.templateWith $ parameters { content = [SIV.view model'] },
            init: IMModel {
                  chatting: Nothing,
                  temporaryID: SP.fromInt 0,
                  suggesting: if DA.null suggestions then Nothing else Just 0,
                  freeToFetchChatHistory:true,
                  messageEnter: true,
                  imageCaption: Nothing,
                  emojisVisible: false,
                  link: Nothing,
                  linkText: Nothing,
                  linkFormVisible: false,
                  freeToFetchContactList: true,
                  userContextMenuVisible: false,
                  profileSettingsToggle: Hidden,
                  message: Nothing,
                  isPreviewing: false,
                  selectedImage: Nothing,
                  contacts,
                  suggestions,
                  user
            }
      }
      where javascript = [ HE.script' [HA.type' "text/javascript", HA.src "/client/javascript/im.bundle.js"] ]
            css = [
                  HE.link [HA.rel "stylesheet", HA.type' "text/css", HA.href "/client/css/im.css"]
            ]
