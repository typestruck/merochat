module Test.Client.Model where

import Prelude
import Shared.Types

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Now as EN
import Effect.Unsafe as EU
import Shared.IM.Contact as SIC

import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)

run :: forall m. m -> Aff (m -> m) -> Aff m
run model f = do
      f' <- f
      pure $ f' model

model :: IMModel
model = {
      message: Just "test message",
      suggestionsPage: 0,
      shouldSendMessage: true,
      freeToFetchContactList: true,
      userContextMenuVisible: false,
      imageCaption: Nothing,
      fullContactProfileVisible: false,
      link: Nothing,
      erroredFields: [],
      selectedImage: Nothing,
      messageEnter: true,
      linkText: Nothing,
      blockedUsers: [],
      toggleModal: HideUserMenuModal,
      toggleChatModal: HideChatModal,
      user: imUser,
      suggestions: [suggestion],
      temporaryID: 0,
      suggesting: Just 0,
      freeToFetchChatHistory: true,
      contacts: [contact],
      isOnline: true,
      chatting: Just 0
}

imUserID :: PrimaryKey
imUserID = 23

imUser :: IMUser
imUser = {
      karmaPosition: 1,
      age: Nothing,
      name: "test",
      id: imUserID,
      avatar: Nothing,
      country: Nothing,
      languages: [],
      tags: [],
      headline: "",
      description: "",
      gender: Nothing,
      karma: 5
}

anotherIMUserID :: PrimaryKey
anotherIMUserID = 90

contactID :: PrimaryKey
contactID = anotherIMUserID

anotherIMUser :: IMUser
anotherIMUser = imUser { id = anotherIMUserID }

contact :: Contact
contact = SIC.defaultContact imUserID anotherIMUser

suggestionID :: PrimaryKey
suggestionID =  300

suggestion :: Suggestion
suggestion = imUser { id = suggestionID }

historyMessage :: HistoryMessage
historyMessage = {
      id: 1,
      sender: imUserID,
      recipient: contactID,
      date: DateTimeWrapper <<< EU.unsafePerformEffect $ EN.nowDateTime,
      content: "test",
      status: Received
}

webSocket :: WebSocket
webSocket = UC.unsafeCoerce 2