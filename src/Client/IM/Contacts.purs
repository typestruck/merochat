module Client.IM.Contacts where

import Debug.Trace
import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIM
import Client.IM.WebSocket as CIW
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Flame ((:>))
import Flame as F
import Shared.Newtype as SN
import Shared.Router as SR
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Internal.Types (Event)
import Shared.IM.Contact as SIC
import Web.HTML.HTMLElement as WHH
import Shared.Page(initialMessagesPerPage)
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WUW

resumeChat :: PrimaryKey -> IMModel -> MoreMessages
resumeChat searchID model@(IMModel { contacts, chatting }) =
      let   index = DA.findIndex ((searchID == _) <<< _.id <<< DN.unwrap <<< _.user <<< DN.unwrap) contacts
            Contact { shouldFetchChatHistory, history, user: IMUser { id } } = SIC.chattingContact contacts index
      in
            if index == chatting then
                  F.noMessages model
             else
                  (SN.updateModel model $ _ {
                        suggesting = Nothing,
                        chatting = index
                  }) :> [
                        CIF.next UpdateReadCount ,
                        CIF.next $ FetchHistory shouldFetchChatHistory
                  ]

markRead :: IMModel -> MoreMessages
markRead =
      case _ of
            model@(IMModel {
                  token: Just tk,
                  user: IMUser { id: userID },
                  webSocket: Just (WS ws),
                  contacts,
                  chatting: Just index
            }) -> updateReadHistory model {
                  token: tk,
                  webSocket: ws,
                  chatting: index,
                  userID,
                  contacts
            }
            model -> F.noMessages model

updateReadHistory :: IMModel -> {
      token :: String,
      userID :: PrimaryKey,
      webSocket :: WebSocket,
      contacts :: Array Contact,
      chatting :: Int
} -> MoreMessages
updateReadHistory model { token, webSocket, chatting, userID, contacts } =
      let contactRead@(Contact { history }) = contacts !@ chatting
          messagesRead = DA.mapMaybe (unreadID userID) <<< _.history $ DN.unwrap contactRead
      in if DA.null messagesRead then
            CIF.nothingNext model scroll
          else
            CIF.nothingNext (updateContacts contactRead) do
                  confirmRead messagesRead
                  scroll

      where unreadID userID (HistoryMessage { recipient, id, status })
                  | status == Unread && recipient == userID = Just id
                  | otherwise = Nothing

            read userID historyEntry@(HistoryMessage { recipient, id, status })
                  | status == Unread && recipient == userID = SN.updateHistoryMessage historyEntry $ _ { status = Read }
                  | otherwise = historyEntry

            updateContacts contactRead@(Contact { history }) = SN.updateModel model $ _ {
                  contacts = SU.fromJust "updateReadHistory" $ DA.updateAt chatting (SN.updateContact contactRead $ _ { history = map (read userID) history }) contacts
            }

            confirmRead messages = liftEffect <<< CIW.sendPayload webSocket $ ReadMessages {
                  ids: messages,
                  token
            }

            scroll = liftEffect CIS.scrollLastMessage

--REFACTOR: dont do querySelector inside updates, pass the element as parameter
checkFetchContacts :: IMModel -> MoreMessages
checkFetchContacts model@(IMModel { contacts, freeToFetchContactList })
      | freeToFetchContactList = model :> [ Just <<< FetchContacts <$> getScrollBottom ]

      where getScrollBottom = liftEffect do
                  element <- CCD.querySelector "#message-history-wrapper"
                  top <- WDE.scrollTop element
                  height <- WDE.scrollHeight element
                  offset <- WHH.offsetHeight <<< SU.fromJust "fetchContacts" $ WHH.fromElement element
                  pure $ top == height - offset

      | otherwise = F.noMessages model

fetchContacts :: Boolean -> IMModel -> MoreMessages
fetchContacts shouldFetch model@(IMModel { contacts, freeToFetchContactList })
      | shouldFetch = (SN.updateModel model $ _ {
                  freeToFetchContactList = false
            }) :> [Just <<< DisplayContacts <$> CCN.get' (Contacts { skip: DA.length contacts })]
      | otherwise =F.noMessages model

displayContacts :: Array Contact -> IMModel -> NoMessages
displayContacts newContacts model@(IMModel { contacts }) =
      F.noMessages <<< SN.updateModel model $ _ {
            contacts = contacts <> newContacts,
            freeToFetchContactList = true
      }