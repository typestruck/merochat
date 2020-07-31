module Test.Client.Model where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Now as EN
import Effect.Unsafe as EU
import Prelude
import Shared.IM.Contact as SIC
import Shared.IM.Types
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Types
import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)

run :: forall m. m -> Aff (m -> m) -> Aff m
run model f = do
      f' <- f
      pure $ f' model

model :: IMModel
model = IMModel {
      message: Nothing,
      isPreviewing: false,
      freeToFetchContactList: true,
      userContextMenuVisible: false,
      profileSettingsToggle: Hidden,
      user: imUser,
      suggestions: [suggestion],
      temporaryID : SP.fromInt 0,
      suggesting: Just 0,
      token: Just "",
      freeToFetchChatHistory: true,
      contacts: [contact],
      webSocket: Just $ WS (UC.unsafeCoerce 23 :: WebSocket),
      chatting: Just 0
}

imUserID :: PrimaryKey
imUserID = SP.fromInt 23

imUser :: IMUser
imUser = IMUser {
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
anotherIMUserID = SP.fromInt 90

contactID :: PrimaryKey
contactID = anotherIMUserID

anotherIMUser :: IMUser
anotherIMUser = SN.updateUser imUser $ _ { id = anotherIMUserID }

contact :: Contact
contact = SIC.defaultContact imUserID anotherIMUser

suggestionID :: PrimaryKey
suggestionID = SP.fromInt 300

suggestion :: Suggestion
suggestion = SN.updateUser imUser $ _ { id = suggestionID }

historyMessage :: HistoryMessage
historyMessage = HistoryMessage {
      id: SP.fromInt 1,
      sender:  imUserID,
      recipient:contactID,
      date: MDateTime <<< EU.unsafePerformEffect $ EN.nowDateTime,
      content: "test",
      status: Unread
}