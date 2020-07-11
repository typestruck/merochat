module Test.Client.Model where

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Prelude (bind, pure, ($))
import Shared.IM.Types (Contact(..), IMModel(..), IMUser(..), ProfileSettingsToggle(..), Suggestion, WS(..))
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Types (PrimaryKey(..))
import Unsafe.Coerce as UC
import Web.Socket.WebSocket (WebSocket)

run :: forall m. m -> Aff (m -> m) -> Aff m
run model f = do
        f' <- f
        pure $ f' model


model :: IMModel
model = IMModel {
        userContextMenuVisible: false,
        profileSettingsToggle: Hidden,
        user: imUser,
        suggestions: [suggestion],
        temporaryID : SP.fromInt 0,
        suggesting: Just 0,
        contactsPage: 0,
        token: Just "",
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

anotherIMUser :: IMUser
anotherIMUser = SN.updateUser imUser $ _ { id = anotherIMUserID }

contact :: Contact
contact = Contact {
        chatStarter: imUserID,
        user: anotherIMUser,
        history: [],
        chatAge: 0.0
}

suggestionID :: PrimaryKey
suggestionID = SP.fromInt 300

suggestion :: Suggestion
suggestion = SN.updateUser imUser $ _ { id = suggestionID }