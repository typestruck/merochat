module Client.IM.Contacts where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.IM.Flame (MoreMessages, NoMessages)
import Client.IM.Flame as CIF
import Client.IM.Notification as CIU
import Client.IM.Notification as CIUN
import Client.IM.Scroll as CIS
import Client.IM.WebSocket (WebSocket)
import Client.IM.WebSocket as CIW
import Data.Array ((!!), (..), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DS
import Data.Traversable as DTV
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Debug (spy)
import Effect.Class (liftEffect)
import Flame ((:>))
import Flame as F
import Flame.Html.Attribute (d)
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHH
import Web.Socket.WebSocket (WebSocket)

resumeChat ∷ Int → Maybe Int → ImModel → MoreMessages
resumeChat searchId impersonating model@{ contacts, chatting, smallScreen } =
      let
            index = DA.findIndex (\cnt → cnt.user.id == searchId && cnt.impersonating == impersonating) contacts
            cnt@{ shouldFetchChatHistory, user: { id } } = SIC.chattingContact contacts index
      in
            if index == chatting then
                  F.noMessages model
            else
                  model
                        { chatting = index
                        , fullContactProfileVisible = false
                        , toggleChatModal = HideChatModal
                        , initialScreen = false
                        , selectedImage = Nothing
                        , failedRequests = []
                        } :>
                        ( [ CIF.next UpdateReadCount
                          , CIS.scrollLastMessage'
                          , CIF.next <<< SpecialRequest $ FetchHistory shouldFetchChatHistory
                          ] <> smallScreenEffect
                        )
      where
      smallScreenEffect = if smallScreen then [] else [ CIF.next $ FocusInput ChatInput ]

markRead ∷ WebSocket → ImModel → MoreMessages
markRead webSocket =
      case _ of
            model@
                  { user: { id: userId }
                  , contacts
                  , chatting: Just index
                  } → updateStatus model
                  { newStatus: Read
                  , index
                  , webSocket
                  , sessionUserId: userId
                  , contacts
                  }
            model → F.noMessages model

updateStatus ∷
      ImModel →
      { sessionUserId ∷ Int
      , webSocket ∷ WebSocket
      , contacts ∷ Array Contact
      , index ∷ Int
      , newStatus ∷ MessageStatus
      } →
      MoreMessages
updateStatus model@{ experimenting } { webSocket, index, sessionUserId, contacts, newStatus } =
      let
            contactRead@{ history, user: { id: contactUserId } } = contacts !@ index
            messagesToUpdate = DA.mapMaybe toChange history
      in
            if DA.null messagesToUpdate then
                  F.noMessages model
            else
                  let
                        updatedModel@{ contacts } = updateContacts contactRead
                  in
                        CIF.nothingNext updatedModel $ liftEffect do
                              sendStatusChange contactUserId messagesToUpdate
                              alertUnread contacts

      where
      toChange { recipient, id, status }
            | status >= Sent && status < newStatus && recipient == sessionUserId = Just id
            | otherwise = Nothing

      changeStatus historyEntry@{ recipient, status }
            | status >= Sent && status < newStatus && recipient == sessionUserId = historyEntry { status = newStatus }
            | otherwise = historyEntry

      updateContacts contactRead@{ history } = model
            { contacts = SU.fromJust $ DA.updateAt index (contactRead { history = map changeStatus history, typing = false }) contacts
            }

      sendStatusChange contactUserId messages = CIW.sendPayload webSocket $ ChangeStatus
            { status: newStatus
            , ids: [ Tuple contactUserId messages ]
            , persisting: case experimenting of
                    Just (Impersonation (Just _)) → false
                    _ → true
            }

      alertUnread = CIUN.updateTabCount sessionUserId

--we need to update message status from sent to unread in the cases of
-- opening ws connection (messages might have been sent while offline)
-- chats from new users
-- recovering from disconnect
markDelivered ∷ WebSocket → ImModel → NoMessages
markDelivered webSocket model@{ experimenting, contacts, user: { id: sessionUserId } } =
      if DA.null ids then
            F.noMessages model
      else CIF.nothingNext updatedModel <<< liftEffect <<< CIW.sendPayload webSocket $ ChangeStatus
            { status: Delivered
            , ids
            , persisting: case experimenting of
                    Just (Impersonation (Just _)) → false
                    _ → true
            }
      where
      ids = DA.foldl sent [] contacts

      updatedModel = model { contacts = map updateDelivered contacts }

      updateDelivered contact@{ history } =
            let
                  update h@{ status, recipient }
                        | recipient == sessionUserId && status == Received = h { status = Delivered }
                        | otherwise = h
            in
                  contact { history = map update history }

      sent running { history, user: { id: userId } } =
            case DA.filter (\h → h.recipient == sessionUserId && h.status == Received) history of
                  [] → running
                  hs → Tuple userId (map _.id hs) : running

checkFetchContacts ∷ ImModel → MoreMessages
checkFetchContacts model@{ freeToFetchContactList }
      | freeToFetchContactList = model :> [ Just <<< SpecialRequest <<< FetchContacts <$> getScrollBottom ]

              where
              getScrollBottom = liftEffect do
                    element ← CCD.unsafeGetElementById ContactList
                    top ← WDE.scrollTop element
                    height ← WDE.scrollHeight element
                    offset ← WHH.offsetHeight <<< SU.fromJust $ WHH.fromElement element
                    pure $ top == height - offset

      | otherwise = F.noMessages model

fetchContacts ∷ Boolean → ImModel → MoreMessages
fetchContacts shouldFetch model@{ contacts, experimenting }
      | shouldFetch =
              model
                    { freeToFetchContactList = false
                    } :> if DM.isJust experimenting then [] else [ CCN.retryableResponse (FetchContacts true) DisplayContacts $ request.im.contacts { query: { skip: DA.length contacts } } ]
      | otherwise = F.noMessages model

--paginated contacts
displayContacts ∷ Array Contact → ImModel → MoreMessages
displayContacts newContacts model = updateDisplayContacts newContacts [] model

--new chats
displayNewContacts ∷ Array Contact → ImModel → MoreMessages
displayNewContacts newContacts model = updateDisplayContacts newContacts (map (\cnt → Tuple cnt.user.id cnt.impersonating) newContacts) model

--new chats from impersonation experiment
displayImpersonatedContacts ∷ Int → HistoryMessage → Array Contact → ImModel → MoreMessages
displayImpersonatedContacts id history newContacts = displayNewContacts (map (_ { shouldFetchChatHistory = false, impersonating = Just id, history = [ history ] }) newContacts)

resumeMissedEvents ∷ MissedEvents → ImModel → MoreMessages
resumeMissedEvents { contacts: missedContacts, messageIds } model@{ contacts, user: { id: senderID } } =
      let
            missedFromExistingContacts = map markSenderError $ DA.updateAtIndices (map getExisting existing) contacts
            missedFromNewContacts = map getNew new
      in
            CIU.notifyUnreadChats
                  ( model
                          {
                            --wew lass
                            contacts = missedFromNewContacts <> missedFromExistingContacts
                          }
                  ) $ map (\cnt → Tuple cnt.user.id cnt.impersonating) missedContacts
      where
      messageMap = DH.fromArrayBy _.temporaryId _.id messageIds
      markSenderError contact@{ history } = contact
            { history = map updateSenderError history
            }
      updateSenderError history@{ sender, status, id }
            | status == Sent && sender == senderID =
                    if DH.member id messageMap then --received or not by the server

                          history
                                { status = Received
                                , id = SU.lookup id messageMap
                                }
                    else
                          history { status = Errored }
            | otherwise = history

      indexesToIndexes = DA.zip (0 .. DA.length missedContacts) $ findContact <$> missedContacts
      existing = DA.filter (DM.isJust <<< DT.snd) indexesToIndexes
      new = DA.filter (DM.isNothing <<< DT.snd) indexesToIndexes

      getNew (Tuple newIndex _) = missedContacts !@ newIndex

      getExisting (Tuple existingIndex contactsIndex) = SU.fromJust do
            index ← contactsIndex
            currentContact ← contacts !! index
            contact ← missedContacts !! existingIndex
            pure <<< Tuple index $ currentContact
                  { history = currentContact.history <> contact.history
                  }

      findContact { user: { id } } = DA.findIndex (sameContact id) contacts
      sameContact userId { user: { id } } = userId == id

updateDisplayContacts ∷ Array Contact → Array (Tuple Int (Maybe Int)) → ImModel → MoreMessages
updateDisplayContacts newContacts userIds model@{ contacts } =
      CIU.notifyUnreadChats
            ( model
                    { contacts = contacts <> onlyNew
                    , freeToFetchContactList = true
                    }
            )
            userIds
      where
      existingContactIds = DS.fromFoldable (_.id <<< _.user <$> contacts)
      onlyNew = DA.filter (\cnt → not $ DS.member cnt.user.id existingContactIds) newContacts -- if a contact from pagination is already in the list

deleteChat ∷ Tuple Int (Maybe Int) → ImModel → MoreMessages
deleteChat tii@(Tuple id impersonating) model@{ contacts } =
      updatedModel :>
            if DM.isNothing impersonating then
                  [ backToSuggestions
                  , do
                          result ← CCN.defaultResponse $ request.im.delete { body: { userId: id, messageId: SU.fromJust lastMessageId } }
                          case result of
                                Left _ → pure <<< Just $ RequestFailed { request: DeleteChat tii, errorMessage: Nothing }
                                _ → pure Nothing
                  ]
            else [ backToSuggestions ]
      where
      backToSuggestions = pure $ Just ResumeSuggesting

      updatedModel = model
            { toggleModal = HideUserMenuModal
            , contacts = DA.filter (\cnt → cnt.user.id /= id && (cnt.impersonating == Nothing || cnt.impersonating /= impersonating)) contacts
            }
      lastMessageId = do
            contact ← DA.find (\cnt → cnt.user.id == id && cnt.impersonating == impersonating) contacts
            { id } ← DA.last contact.history
            pure id