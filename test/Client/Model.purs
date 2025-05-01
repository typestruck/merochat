module Test.Client.Model where

import Prelude
import Shared.Availability
import Shared.DateTime
import Shared.Im.Types
import Shared.User

import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashMap as HS
import Data.HashSet as DHS
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Now as EN
import Effect.Unsafe as EU
import Safe.Coerce as SC
import Shared.Element (ElementId)
import Shared.Im.Contact as SIC
import Unsafe.Coerce as UC
import Web.DOM (Element)
import Web.Socket.WebSocket (WebSocket)

run ∷ ∀ m. m → Aff (m → m) → Aff m
run model f = do
      f' ← f
      pure $ f' model

model ∷ ImModel
model =
      { modalsLoaded: []
      , freeToFetchSuggestions: true
      , typingIds: []
      , initialScreen: true
      , suggestionsFrom: ThisWeek
      , showMiniChatInput: false
      , temporaryEmail: Nothing
      , editing: Nothing
      , bugging: Nothing
      , temporaryPassword: Nothing
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
      , suggesting: 0
      , freeToFetchChatHistory: true
      , contacts: [ contact ]
      , isWebSocketConnected: true
      , chatting: Nothing
      }

imUserId ∷ Int
imUserId = 23

imUser ∷ User
imUser =
      { karmaPosition: 1
      , age: Nothing
      , name: "test"
      , id: imUserId
      , profileVisibility: Everyone
      , readReceipts: true
      , messageTimestamps: true
      , joined: DateTimeWrapper epoch
      , temporary: false
      , typingStatus: true
      , onlineStatus: true
      , avatar: Nothing
      , bin: 1
      , country: Nothing
      , availability: None
      , languages: []
      , completedTutorial: true
      , tags: []
      , badges: []
      , privileges: []
      , headline: ""
      , description: ""
      , gender: Nothing
      , karma: 5
      }

anotherImUserId ∷ Int
anotherImUserId = 90

contactId ∷ Int
contactId = 90

anotherImUser ∷ User
anotherImUser = imUser { id = anotherImUserId }

contact ∷ Contact
contact = SIC.defaultContact (SC.coerce imUserId) anotherImUser

suggestionID ∷ Int
suggestionID = 300

suggestion ∷ Suggestion
suggestion = imUser { id = suggestionID }

historyMessage ∷ HistoryMessage
historyMessage =
      { id: 1
      , sender: imUserId
      , edited: false
      , recipient: contactId
      , date: DateTimeWrapper $ EU.unsafePerformEffect EN.nowDateTime
      , content: "test"
      , status: Received
      }

webSocket ∷ WebSocket
webSocket = UC.unsafeCoerce 2

elements ∷ HashMap ElementId Element
elements = HS.empty