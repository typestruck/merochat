module Client.Im.Contacts where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CCNT
import Client.Im.Flame (MoreMessages, NoMessages)
import Client.Im.Flame as CIF
import Client.Im.Notification as CIU
import Client.Im.Notification as CIUN
import Client.Im.Scroll as CIS
import Client.Im.WebSocket as CIW
import Data.Array ((!!), (..), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DS
import Data.Tuple (Tuple(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class (liftEffect)
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHH
import Web.Socket.WebSocket (WebSocket)

resumeChat ∷ Int → ImModel → MoreMessages
resumeChat searchId model@{ contacts, chatting, smallScreen } =
      let
            index = DA.findIndex (\cnt → cnt.user.id == searchId) contacts
            { shouldFetchChatHistory } = SIC.chattingContact contacts index
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
                        } /\
                        ( [ pure $ Just UpdateReadCount
                          , CIS.scrollLastMessage'
                          , pure <<< Just <<< SpecialRequest $ FetchHistory shouldFetchChatHistory
                          ] <> smallScreenEffect
                        )
      where
      smallScreenEffect = if smallScreen then [] else [ pure <<< Just $ FocusInput ChatInput ]

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
                  , loggedUserId: userId
                  , contacts
                  }
            model → F.noMessages model

updateStatus ∷
      ImModel →
      { loggedUserId ∷ Int
      , webSocket ∷ WebSocket
      , contacts ∷ Array Contact
      , index ∷ Int
      , newStatus ∷ MessageStatus
      } →
      MoreMessages
updateStatus model { webSocket, index, loggedUserId, contacts, newStatus } =
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
                        updatedModel /\
                              [ liftEffect do
                                      sendStatusChange contactUserId messagesToUpdate
                                      alertUnread contacts
                                      pure Nothing
                              ]

      where
      toChange { recipient, id, status }
            | status >= Sent && status < newStatus && recipient == loggedUserId = Just id
            | otherwise = Nothing

      changeStatus historyEntry@{ recipient, status }
            | status >= Sent && status < newStatus && recipient == loggedUserId = historyEntry { status = newStatus }
            | otherwise = historyEntry

      updateContacts contactRead@{ history } = model
            { contacts = SU.fromJust $ DA.updateAt index (contactRead { history = map changeStatus history, typing = false }) contacts
            }

      sendStatusChange contactUserId messages = CIW.sendPayload webSocket $ ChangeStatus
            { status: newStatus
            , ids: [ Tuple contactUserId messages ]
            }

      alertUnread = CIUN.updateTabCount loggedUserId

--we need to update message status from sent to unread in the cases of
-- opening ws connection (messages might have been sent while offline)
-- chats from new users
-- recovering from disconnect
markDelivered ∷ WebSocket → ImModel → NoMessages
markDelivered webSocket model@{ contacts, user: { id: loggedUserId } } =
      if DA.null ids then
            F.noMessages model
      else updatedModel /\
            [ liftEffect do
                    CIW.sendPayload webSocket $ ChangeStatus
                          { status: Delivered
                          , ids
                          }
                    pure Nothing
            ]
      where
      ids = DA.foldl sent [] contacts

      updatedModel = model { contacts = map updateDelivered contacts }

      updateDelivered contact@{ history } =
            let
                  update h@{ status, recipient }
                        | recipient == loggedUserId && status == Received = h { status = Delivered }
                        | otherwise = h
            in
                  contact { history = map update history }

      sent running { history, user: { id: userId } } =
            case DA.filter (\h → h.recipient == loggedUserId && h.status == Received) history of
                  [] → running
                  hs → Tuple userId (map _.id hs) : running

checkFetchContacts ∷ ImModel → MoreMessages
checkFetchContacts model
      | model.freeToFetchContactList = model /\ [ Just <<< SpecialRequest <<< FetchContacts <$> getScrollBottom ]

              where
              lenience = if model.smallScreen then 2.0 else 0.0
              getScrollBottom = liftEffect do
                    element ← CCD.unsafeGetElementById ContactList
                    top ← WDE.scrollTop element
                    height ← WDE.scrollHeight element
                    offset ← WHH.offsetHeight <<< SU.fromJust $ WHH.fromElement element
                    pure $ top + lenience >= height - offset

      | otherwise = F.noMessages model

fetchContacts ∷ Boolean → ImModel → MoreMessages
fetchContacts shouldFetch model@{ contacts }
      | shouldFetch =
              model
                    { freeToFetchContactList = false
                    } /\ [ CCN.retryableResponse (FetchContacts true) DisplayContacts $ request.im.contacts { query: { skip: DA.length contacts } } ]
      | otherwise = F.noMessages model

--paginated contacts
displayContacts ∷ Array Contact → ImModel → MoreMessages
displayContacts newContacts model = updateDisplayContacts newContacts [] model

--new chats
displayNewContacts ∷ Array Contact → ImModel → MoreMessages
displayNewContacts newContacts model = updateDisplayContacts newContacts (map (\cnt → cnt.user.id) newContacts) model

resumeMissedEvents ∷ MissedEvents → ImModel → MoreMessages
resumeMissedEvents ev model = CIU.notifyUnreadChats updatedModel contactsWithNewMessages # thenPerform fetchNew
      where
      updatedModel = model
            { contacts = map updateHistory model.contacts
            }

      messagesByUser = DA.foldl (\hm v → DH.insertWith (<>) (if v.recipient == model.user.id then v.sender else v.recipient) [ v ] hm) DH.empty (spy "received" ev.missedMessages)
      updateHistory contact = case DH.lookup contact.user.id messagesByUser of
            Nothing → contact
            Just found → contact
                  { history = DA.sortWith _.date $ DA.nubBy (\g h → compare g.id h.id) (contact.history <> found)
                  }

      --if the new message comes from an user that is already in the contact list show notifications
      -- otherwise fetch the user
      existingContacts = DS.fromFoldable $ map (\c → c.user.id) model.contacts
      contactsWithNewMessages = map _.sender $ DA.filter (\h → h.status < Read && h.recipient == model.user.id && DS.member h.sender existingContacts) ev.missedMessages
      newContacts = map _.sender $ DA.filter (\h → h.recipient == model.user.id && not (DS.member h.sender existingContacts)) ev.missedMessages

      thenPerform e (m /\ ms) = m /\ (ms <> e)
      fetchNew =
            map (\id → CCNT.retryableResponse CheckMissedEvents DisplayNewContacts $ request.im.contact { query: { id } }) newContacts

updateDisplayContacts ∷ Array Contact → Array Int → ImModel → MoreMessages
updateDisplayContacts newContacts userIds model@{ contacts } =
      CIU.notifyUnreadChats
            ( model
                    { contacts = contacts <> onlyNew
                    , freeToFetchContactList = true
                    }
            )
            userIds
      where
      existingContactIds = DS.fromFoldable $ map (\cnt → cnt.user.id) contacts
      onlyNew = DA.filter (\cnt → not $ DS.member cnt.user.id existingContactIds) newContacts -- if a contact from pagination is already in the list

deleteChat ∷ Int → ImModel → MoreMessages
deleteChat id model =
      updatedModel /\
            [ do
                    result ← CCN.defaultResponse $ request.im.delete { body: { userId: id, messageId: lastMessageId } }
                    case result of
                          Left _ → pure <<< Just $ RequestFailed { request: DeleteChat id, errorMessage: Nothing }
                          _ → pure Nothing
            ]
      where

      deletedIndex = DA.findIndex (\cnt → cnt.user.id == id) model.contacts
      updatedModel = model
            { toggleModal = HideUserMenuModal
            , toggleChatModal = HideChatModal
            , contacts = DM.fromMaybe model.contacts do
                    i ← deletedIndex
                    DA.deleteAt i model.contacts
            , chatting =
                    if deletedIndex < model.chatting then (max 0 <<< (_ - 1)) <$> model.chatting
                    else if deletedIndex > model.chatting then model.chatting
                    else Nothing
            }

      lastMessageId = SU.fromJust do
            di ← deletedIndex
            contact ← model.contacts !! di
            entry ← DA.last contact.history
            pure entry.id