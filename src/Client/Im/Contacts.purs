module Client.Im.Contacts where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CCNT
import Client.Im.Flame (MoreMessages, NoMessages)
import Client.Im.Notification as CIU
import Client.Im.Notification as CIUN
import Client.Im.Scroll as CIS
import Client.Im.WebSocket as CIW
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DS
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class (liftEffect)
import Flame as F
import Server.Im.Database (changeStatus)
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Types (Contact, ImMessage(..), ImModel, MessageStatus(..), MissedEvents, RetryableRequest(..), ShowChatModal(..), ShowUserMenuModal(..), WebSocketPayloadServer(..))
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE
import Web.HTML.HTMLElement as WHH
import Web.Socket.WebSocket (WebSocket)

-- | When a contact is selected from the list, update `chatting` accordingly
resumeChat ∷ Int → ImModel → MoreMessages
resumeChat userId model =
      if updatedChatting == model.chatting then
            F.noMessages model
      else
            model
                  { chatting = updatedChatting
                  , fullContactProfileVisible = false
                  , toggleChatModal = HideChatModal
                  , initialScreen = false
                  , selectedImage = Nothing
                  , editing = Nothing
                  , contacts = updatedContacts
                  , failedRequests = []
                  } /\
                  ( smallScreenEffect <>
                          [ updateReadCountEffect
                          , CIS.scrollLastMessageAff
                          , fetchHistoryEffect
                          ]
                  )
      where
      updatedChatting = DA.find ((userId == _) <<< _.id <<< _.user) model.contacts
      updatedContacts =
            let
                  uc = DA.filter ((userId /= _) <<< _.id <<< _.user) model.contacts
            in
                  case model.chatting of
                        Nothing → uc
                        Just oldChatting → oldChatting : uc

      updateReadCountEffect = pure $ Just SetReadStatus
      fetchHistoryEffect = let contact = SU.fromJust updatedChatting in pure <<< Just <<< SpecialRequest $ FetchHistory contact.user.id contact.shouldFetchChatHistory
      smallScreenEffect
            | model.smallScreen = [ pure Nothing ]
            | otherwise = [ pure <<< Just $ FocusInput ChatInput ]

-- | When coming back to the site mark messages as read if a chat is open
setReadStatus ∷ WebSocket → ImModel → MoreMessages
setReadStatus webSocket model =
      case model.chatting of
            Just chatting → setMessageStatus webSocket (Right chatting) Read model
            Nothing → F.noMessages model

-- | Update status of messages with the given contact
setMessageStatus ∷ WebSocket → Either Int Contact → MessageStatus → ImModel → MoreMessages
setMessageStatus webSocket who newStatus model =
      case who of
            Right chatting →
                  let
                        updatedChatting = chatting { history = map updateStatus chatting.history }
                  in
                        model
                              { chatting = Just updatedChatting
                              } /\
                              [ setIt
                                      chatting.user.id
                                      (messageIdsUpdated chatting.history)
                                      (updatedChatting : model.contacts)
                              ]

            Left userId →
                  let
                        updatedContacts = map (updateContact userId) model.contacts
                  in
                        model
                              { contacts = updatedContacts
                              } /\
                              [ setIt
                                      userId
                                      (messageIdsUpdated <<< DM.maybe [] _.history $ DA.find ((userId == _) <<< _.id <<< _.user) model.contacts)
                                      (DM.maybe updatedContacts (_ : updatedContacts) model.chatting)
                              ]

      where
      needsUpdate entry = entry.status >= Sent && entry.status < newStatus && entry.recipient == model.user.id
      messageIdsUpdated history = map _.id $ DA.filter needsUpdate history

      updateStatus entry
            | needsUpdate entry = entry { status = newStatus }
            | otherwise = entry

      updateContact userId contact
            | contact.user.id == userId = contact { history = map updateStatus contact.history }
            | otherwise = contact

      setIt userId messages contacts = liftEffect do
            CIW.sendPayload webSocket $ ChangeStatus
                  { status: newStatus
                  , ids: [ Tuple userId messages ]
                  }
            CIUN.updateTabCount userId contacts
            pure Nothing

-- | Update message status to unread upon openning the site or receveing from new contacts
setDeliveredStatus ∷ WebSocket → ImModel → NoMessages
setDeliveredStatus webSocket model@{ contacts, user: { id: loggedUserId } } =
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

-- | Messages sent or received while the web socket connection was down
resumeMissedEvents ∷ MissedEvents → ImModel → MoreMessages
resumeMissedEvents ev model = CIU.notifyUnreadChats updatedModel contactsWithNewMessages # thenPerform fetchNew
      where
      updatedModel = model
            { contacts = map updateHistory model.contacts
            }

      messagesByUser = DA.foldl (\hm v → DH.insertWith (<>) (if v.recipient == model.user.id then v.sender else v.recipient) [ v ] hm) DH.empty ev.missedMessages
      updateHistory contact = case DH.lookup contact.user.id messagesByUser of
            Nothing → contact
            Just found →
                  let
                        history = DA.sortWith _.date $ DA.nubBy (\g h → compare g.id h.id) (found <> contact.history)
                  in
                        contact
                              { history = history
                              , lastMessageDate = DM.fromMaybe contact.lastMessageDate <<< map _.date $ DA.last history
                              }

      --if the new message comes from an user that is already in the contact list show notifications
      -- otherwise fetch the user
      existingContacts = DS.fromFoldable $ map (\c → c.user.id) model.contacts
      contactsWithNewMessages = map _.sender $ DA.filter (\h → h.status < Read && h.recipient == model.user.id && DS.member h.sender existingContacts) ev.missedMessages
      newContacts = map _.sender $ DA.filter (\h → h.recipient == model.user.id && not (DS.member h.sender existingContacts)) ev.missedMessages

      thenPerform e (m /\ ms) = m /\ (ms <> e)
      fetchNew =
            map (\id → CCNT.retryableResponse (CheckMissedEvents Nothing) DisplayNewContacts $ request.im.contact { query: { id } }) newContacts

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
deleteChat userId model =
      updatedModel /\
            [ do
                    result ← CCN.defaultResponse $ request.im.delete { body: { userId, messageId: SU.fromJust lastMessageId } }
                    case result of
                          Left _ → pure <<< Just $ RequestFailed { request: DeleteChat userId, errorMessage: Nothing }
                          _ → pure Nothing
            ]
      where
      updatedModel = model
            { toggleModal = HideUserMenuModal
            , toggleChatModal = HideChatModal
            , contacts = DA.filter ((userId /= _) <<< _.id <<< _.user) model.contacts
            , chatting =
                    case model.chatting of
                          Just chatting | chatting.user.id == userId → Nothing
                          c → c
            }

      lastMessageId = case model.chatting of
            Just chatting | chatting.user.id == userId → _.id <$> DA.last chatting.history
            _ →  DA.find ((userId == _) <<< _.id <<< _.user) model.contacts >>= (map _.id <<< DA.last <<< _.history)