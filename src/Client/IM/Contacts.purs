module Client.IM.Contacts where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.Unread as CIU
import Client.IM.Unread as CIUN
import Client.IM.WebSocket as CIW
import Data.Array ((!!), (..))
import Data.Array as DA
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Shared.IM.Contact as SIC
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHH
import Web.Socket.WebSocket (WebSocket)

resumeChat :: PrimaryKey -> IMModel -> MoreMessages
resumeChat searchID model@{ contacts, chatting } =
      let   index = DA.findIndex ((searchID == _) <<< _.id <<< _.user) contacts
            { shouldFetchChatHistory, history, user: { id } } = SIC.chattingContact contacts index
      in
            if index == chatting then
                  F.noMessages model
             else
                  model {
                        chatting = index,
                        fullContactProfileVisible = false,
                        toggleChatModal = HideChatModal,
                        selectedImage = Nothing,
                        failedRequests = []
                  } :> [
                        CIF.next UpdateReadCount ,
                        CIF.next <<< SpecialRequest $ FetchHistory shouldFetchChatHistory,
                        CIF.next $ FocusInput "#chat-input"
                  ]

markRead :: WebSocket -> IMModel -> MoreMessages
markRead webSocket =
      case _ of
            model@{
                  user: { id: userID },
                  contacts,
                  chatting: Just index
            } -> updateReadHistory model {
                  chatting: index,
                  webSocket,
                  userID,
                  contacts
            }
            model -> F.noMessages model

updateReadHistory :: IMModel -> {
      userID :: PrimaryKey,
      webSocket :: WebSocket,
      contacts :: Array Contact,
      chatting :: Int
} -> MoreMessages
updateReadHistory model { webSocket, chatting, userID, contacts } =
      let contactRead@{ history } = contacts !@ chatting
          messagesRead = DA.mapMaybe (unreadID userID) history
      in
            if DA.null messagesRead then
                  CIF.nothingNext model $ liftEffect scroll
             else
                  let updatedModel@{ user: { id }, contacts } = (updateContacts contactRead)
                  in CIF.nothingNext updatedModel $ liftEffect do
                        confirmRead messagesRead
                        scroll
                        alertUnread id contacts

      where unreadID userID ( { recipient, id, status })
                  | status == Received && recipient == userID = Just id
                  | otherwise = Nothing

            read userID historyEntry@( { recipient, id, status })
                  | status == Received && recipient == userID = historyEntry { status = Read }
                  | otherwise = historyEntry

            updateContacts contactRead@{ history } = model {
                  contacts = SU.fromJust $ DA.updateAt chatting (contactRead { history = map (read userID) history }) contacts
            }

            confirmRead messages = CIW.sendPayload webSocket $ ReadMessages { ids: messages }

            scroll = CIS.scrollLastMessage

            alertUnread id contacts = CIUN.setTabCount id contacts

checkFetchContacts :: IMModel -> MoreMessages
checkFetchContacts model@{ contacts, freeToFetchContactList }
      | freeToFetchContactList = model :> [ Just <<< SpecialRequest <<< FetchContacts <$> getScrollBottom ]

      where getScrollBottom = liftEffect do
                  element <- CCD.unsafeQuerySelector "#message-history-wrapper"
                  top <- WDE.scrollTop element
                  height <- WDE.scrollHeight element
                  offset <- WHH.offsetHeight <<< SU.fromJust $ WHH.fromElement element
                  pure $ top == height - offset

      | otherwise = F.noMessages model

fetchContacts :: Boolean -> IMModel -> MoreMessages
fetchContacts shouldFetch model@{ contacts, freeToFetchContactList }
      | shouldFetch = model {
                  freeToFetchContactList = false
            } :> [CCN.retryableResponse (FetchContacts true) DisplayContacts $ request.im.contacts { query: { skip: DA.length contacts }}]
      | otherwise = F.noMessages model

displayContacts :: Array Contact -> IMModel -> NoMessages
displayContacts newContacts model@{ contacts } =
      CIU.alertUnreadChats $ model {
            contacts = contacts <> newContacts,
            freeToFetchContactList = true
      }

resumeMissedEvents :: MissedEvents -> IMModel -> NoMessages
resumeMissedEvents {contacts: missedContacts, messageIDs } model@{ contacts, user: { id: senderID } } =
      let missedFromExistingContacts = map markSenderError $ DA.updateAtIndices (map getExisting existing) contacts
          missedFromNewContacts = map getNew new
      in F.noMessages $ model {
            --wew lass
            contacts = missedFromNewContacts <> missedFromExistingContacts
      }
      where messageMap = DH.fromArrayBy _.temporaryID _.id messageIDs
            markSenderError contact@{ history } = contact {
                  history = map updateSenderError history
            }
            updateSenderError history@{ sender, status, id }
                  | status == Sent && sender == senderID && not (DH.member id messageMap) = history { status = Errored } -- message that was not delivered
                  | otherwise = history

            indexesToIndexes = DA.zip (0 .. DA.length missedContacts) $ findContact <$> missedContacts
            existing = DA.filter (DM.isJust <<< DT.snd) indexesToIndexes
            new = DA.filter (DM.isNothing <<< DT.snd) indexesToIndexes

            getNew (Tuple newIndex _) = missedContacts !@ newIndex

            getExisting (Tuple existingIndex contactsIndex) = SU.fromJust do
                  index <- contactsIndex
                  currentContact <- contacts !! index
                  contact <- missedContacts !! existingIndex
                  pure <<< Tuple index $ currentContact {
                        history = currentContact.history <> contact.history
                  }

            findContact ({user: { id }}) = DA.findIndex (sameContact id) contacts
            sameContact userID ({user: { id }}) = userID == id
