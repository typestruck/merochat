module Client.IM.Contacts where

import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.Notification as CIU
import Client.IM.Notification as CIUN
import Client.IM.Scroll as CIS
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
resumeChat searchID model@{ contacts, chatting, smallScreen } =
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
                        initialScreen = false,
                        selectedImage = Nothing,
                        failedRequests = []
                  } :> ([
                        CIF.next UpdateReadCount,
                        liftEffect do
                              CIS.scrollLastMessage
                              pure Nothing,
                        CIF.next <<< SpecialRequest $ FetchHistory shouldFetchChatHistory
                  ] <> if smallScreen then [] else [CIF.next $ FocusInput ChatInput])

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
          messagesRead = DA.mapMaybe unreadID  history
      in
            if DA.null messagesRead then
                  F.noMessages model
             else
                  let updatedModel@{ contacts } = updateContacts contactRead
                  in CIF.nothingNext updatedModel $ liftEffect do
                        confirmRead messagesRead
                        alertUnread contacts

      where unreadID { recipient, id, status }
                  | status == Received && recipient == userID = Just id
                  | otherwise = Nothing

            read historyEntry@( { recipient, id, status })
                  | status == Received && recipient == userID = historyEntry { status = Read }
                  | otherwise = historyEntry

            updateContacts contactRead@{ history } = model {
                  contacts = SU.fromJust $ DA.updateAt chatting (contactRead { history = map read  history }) contacts
            }

            confirmRead messages = CIW.sendPayload webSocket $ ReadMessages { ids: messages }

            alertUnread contacts = CIUN.updateTabCount userID contacts

checkFetchContacts :: IMModel -> MoreMessages
checkFetchContacts model@{ contacts, freeToFetchContactList }
      | freeToFetchContactList = model :> [ Just <<< SpecialRequest <<< FetchContacts <$> getScrollBottom ]

      where getScrollBottom = liftEffect do
                  element <- CCD.unsafeQuerySelector $ "#" <> show ContactList
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

--paginated contacts
displayContacts :: Array Contact -> IMModel -> MoreMessages
displayContacts newContacts model@{ contacts } = updateDisplayContacts newContacts [] model

--new chats
displayNewContacts :: Array Contact -> IMModel -> MoreMessages
displayNewContacts newContacts model@{ contacts } = updateDisplayContacts newContacts (map (_.id <<< _.user) newContacts) model

resumeMissedEvents :: MissedEvents -> IMModel -> MoreMessages
resumeMissedEvents { contacts: missedContacts, messageIDs } model@{ contacts, user: { id: senderID } } =
      let missedFromExistingContacts = map markSenderError $ DA.updateAtIndices (map getExisting existing) contacts
          missedFromNewContacts = map getNew new
      in CIU.notifyUnreadChats (model {
            --wew lass
            contacts = missedFromNewContacts <> missedFromExistingContacts
      }) $ map (_.id <<< _.user) missedContacts
      where messageMap = DH.fromArrayBy _.temporaryID _.id messageIDs
            markSenderError contact@{ history } = contact {
                  history = map updateSenderError history
            }
            updateSenderError history@{ sender, status, id }
                  | status == Sent && sender == senderID =
                        if DH.member id messageMap then  --received or not by the server
                              history {
                                    status = Received,
                                    id = SU.lookup id messageMap
                              }
                        else
                              history { status = Errored }
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

            findContact {user: { id }} = DA.findIndex (sameContact id) contacts
            sameContact userID {user: { id }} = userID == id

updateDisplayContacts :: Array Contact -> Array PrimaryKey -> IMModel -> MoreMessages
updateDisplayContacts newContacts userIDs model@{ contacts } =
      CIU.notifyUnreadChats (model {
            contacts = contacts <> newContacts,
            freeToFetchContactList = true
      }) userIDs