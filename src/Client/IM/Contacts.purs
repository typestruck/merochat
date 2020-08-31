module Client.IM.Contacts where

import Debug.Trace
import Debug.Trace
import Prelude
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages, NextMessage)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIM
import Client.IM.WebSocket as CIW
import Data.Array ((!!), (..))
import Data.Array as DA
import Data.Foldable as DF
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Flame ((:>))
import Flame as F
import Shared.IM.Contact as SIC
import Shared.Newtype as SN
import Shared.Options.Page (initialMessagesPerPage)

import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement as WHH
import Web.Socket.WebSocket (WebSocket)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WUW

resumeChat :: PrimaryKey -> IMModel -> MoreMessages
resumeChat searchID model@{ contacts, chatting } =
      let   index = DA.findIndex ((searchID == _) <<< _.id <<< _.user) contacts
            { shouldFetchChatHistory, history, user: { id } } = SIC.chattingContact contacts index
      in
            if index == chatting then
                  F.noMessages model
             else
                  (model {
                        suggesting = Nothing,
                        chatting = index
                  }) :> [
                        CIF.next UpdateReadCount ,
                        CIF.next $ FetchHistory shouldFetchChatHistory
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
      in if DA.null messagesRead then
            CIF.nothingNext model scroll
          else
            CIF.nothingNext (updateContacts contactRead) do
                  confirmRead messagesRead
                  scroll

      where unreadID userID ( { recipient, id, status })
                  | status == Unread && recipient == userID = Just id
                  | otherwise = Nothing

            read userID historyEntry@( { recipient, id, status })
                  | status == Unread && recipient == userID = historyEntry { status = Read }
                  | otherwise = historyEntry

            updateContacts contactRead@{ history } = model {
                  contacts = SU.fromJust $ DA.updateAt chatting (contactRead { history = map (read userID) history }) contacts
            }

            confirmRead messages = liftEffect <<< CIW.sendPayload webSocket $ ReadMessages { ids: messages }

            scroll = liftEffect CIS.scrollLastMessage

checkFetchContacts :: IMModel -> MoreMessages
checkFetchContacts model@{ contacts, freeToFetchContactList }
      | freeToFetchContactList = model :> [ Just <<< FetchContacts <$> getScrollBottom ]

      where getScrollBottom = liftEffect do
                  element <- CCD.querySelector "#message-history-wrapper"
                  top <- WDE.scrollTop element
                  height <- WDE.scrollHeight element
                  offset <- WHH.offsetHeight <<< SU.fromJust $ WHH.fromElement element
                  pure $ top == height - offset

      | otherwise = F.noMessages model

fetchContacts :: Boolean -> IMModel -> MoreMessages
fetchContacts shouldFetch model@{ contacts, freeToFetchContactList }
      | shouldFetch = model {
                  freeToFetchContactList = false
            } :> [Just <<< DisplayContacts <$> (CCN.response $ request.im.contacts { query: { skip: DA.length contacts }})]
      | otherwise =F.noMessages model

displayContacts :: Array Contact -> IMModel -> NoMessages
displayContacts newContacts model@{ contacts } =
      F.noMessages $ model {
            contacts = contacts <> newContacts,
            freeToFetchContactList = true
      }

--3. needs testing
displayMissedMessages :: Array Contact -> IMModel -> NoMessages
displayMissedMessages missedContacts model@{ contacts } =
      F.noMessages $ model {
            --wew lass
            contacts = map getNew new <> DA.updateAtIndices (map getExisting existing) contacts
      }
      where indexesToIndexes = DA.zip (0 .. DA.length missedContacts) $ findContact <$> missedContacts
            existing = DA.filter (DM.isJust <<< DT.snd) indexesToIndexes
            new = DA.filter (DM.isNothing <<< DT.snd) indexesToIndexes

            getNew (Tuple newIndex _) = missedContacts !@ newIndex

            getExisting (Tuple existingIndex contactsIndex) = SU.fromJust do
                  index <- contactsIndex
                  contact <- missedContacts !! existingIndex
                  pure $ Tuple index contact

            findContact ({user: { id }}) = DA.findIndex (sameContact id) contacts
            sameContact userID ({user: { id }}) = userID == id
