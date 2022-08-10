module Test.Client.Model where

import Prelude
import Shared.IM.Types
import Shared.ContentType

import Data.HashMap (HashMap)
import Data.HashMap as HS
import Data.HashSet as DHS
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.DateTime
import Shared.IM.Contact as SIC
import Shared.User
import Unsafe.Coerce as UC
import Web.DOM (Element)
import Web.Socket.WebSocket (WebSocket)

run ∷ ∀ m. m → Aff (m → m) → Aff m
run model f = do
      f' ← f
      pure $ f' model

model ∷ IMModel
model =
      { modalsLoaded: []
      , freeToFetchSuggestions: true
      , typingIds: []
      , initialScreen: true
      , suggestionsPage: 0
      , lastTyping: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
      , hash: ""
      , freeToFetchContactList: true
      , errorMessage: ""
      , reportReason: Nothing
      , reportComment: Nothing
      , imUpdated: false
      , smallScreen: false
      , failedRequests: []
      , toggleContextMenu: HideContextMenu
      , imageCaption: Nothing
      , fullContactProfileVisible: false
      , experimenting: Nothing
      , fortune: Nothing
      , link: Nothing
      , erroredFields: []
      , hasTriedToConnectYet: false
      , selectedImage: Nothing
      , messageEnter: true
      , linkText: Nothing
      , blockedUsers: []
      , toggleModal: HideUserMenuModal
      , toggleChatModal: HideChatModal
      , enableNotificationsVisible: false
      , user: imUser
      , suggestions: [ suggestion ]
      , temporaryId: 0
      , suggesting: Just 0
      , freeToFetchChatHistory: true
      , contacts: [ contact ]
      , isWebSocketConnected: true
      , chatting: Just 0
      }

imUserId ∷ Int
imUserId = 23

imUser ∷ ImUser
imUser =
      { karmaPosition: 1
      , age: Nothing
      , name: "test"
      , id: imUserId
      , profileVisibility: Everyone
      , readReceipts: true
      , messageTimestamps: true
      , typingStatus: true
      , onlineStatus: true
      , avatar: Nothing
      , country: Nothing
      , availability: None
      , languages: []
      , tags: []
      , headline: ""
      , description: ""
      , gender: Nothing
      , karma: 5
      }

anotherImUserId ∷ Int
anotherImUserId = 90

contactId ∷ Int
contactId = anotherImUserId

anotherImUser ∷ ImUser
anotherImUser = imUser { id = anotherImUserId }

contact ∷ Contact
contact = SIC.defaultContact imUserId anotherImUser

suggestionID ∷ Int
suggestionID = 300

suggestion ∷ Suggestion
suggestion = imUser { id = suggestionID }

historyMessage ∷ HistoryMessage
historyMessage =
      { id: 1
      , sender: imUserId
      , recipient: contactId
      , date: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
      , content: "test"
      , status: Received
      }

webSocket ∷ WebSocket
webSocket = UC.unsafeCoerce 2

elements ∷ HashMap ElementId Element
elements = HS.empty