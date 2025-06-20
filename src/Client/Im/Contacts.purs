module Client.Im.Contacts where

import Prelude

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (NoMessages, MoreMessages)
import Client.Im.History as CIH
import Client.Im.Notification as CIU
import Client.Im.Notification as CIUN
import Client.Im.Pwa (SwMessage(..))
import Client.Im.Pwa as CIP
import Client.Im.Scroll as CIS
import Client.Im.WebSocket as CIW
import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DS
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class (liftEffect)
import Effect.Class as EC
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Types (Contact, ImMessage(..), ImModel, MessageStatus(..), RetryableRequest(..), ShowChatModal(..), ShowUserMenuModal(..), WebSocketPayloadServer(..))
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)
import Web.Socket.WebSocket (WebSocket)

-- | When a contact is selected from the list, update `chatting` accordingly
resumeChat ∷ Int → ImModel → MoreMessages
resumeChat userId model =
      if model.chatting == Just userId then
            F.noMessages model
      else case SIC.findContact userId model.contacts of
            Nothing → F.noMessages model
            Just chatting →
                  model
                        { chatting = Just userId
                        , fullContactProfileVisible = false
                        , toggleChatModal = HideChatModal
                        , initialScreen = false
                        , selectedImage = Nothing
                        , editing = Nothing
                        , showLargeAvatar = false
                        , failedRequests = []
                        } /\
                        ( screenEffects chatting <>
                                [ updateReadCountEffect chatting.user.id
                                , CIS.scrollLastMessageAff
                                , loadDraft chatting.draft
                                , fetchHistoryEffect chatting
                                ]
                        )
      where
      loadDraft currentDraft = EC.liftEffect do
            input ← CCD.unsafeGetElementById ChatInput
            previousDraft ← CCD.value input
            CCD.setValue input currentDraft
            if DM.isNothing model.chatting then
                  pure Nothing
            else
                  pure <<< Just $ UpdateDraft (SU.fromJust model.chatting) previousDraft

      updateReadCountEffect ui = pure <<< Just <<< SetReadStatus $ Just ui
      fetchHistoryEffect chatting = pure <<< Just <<< SpecialRequest $ FetchHistory chatting.user.id chatting.shouldFetchChatHistory
      removeNotifications chatting = do
            EC.liftEffect <<< CIP.postMessage $ OpenChat chatting.user.id
            pure Nothing
      screenEffects chatting
            | model.smallScreen = [ removeNotifications chatting ]
            | otherwise = [ pure <<< Just $ FocusInput ChatInput ]

-- | When coming back to the site mark messages as read if a chat is open
setReadStatus ∷ Maybe Int → WebSocket → ImModel → MoreMessages
setReadStatus userId webSocket model =
      case userId <|> model.chatting of
            Just chatting → setMessageStatus webSocket chatting Read model
            Nothing → F.noMessages model

-- | Update status of messages with the given contact
setMessageStatus ∷ WebSocket → Int → MessageStatus → ImModel → MoreMessages
setMessageStatus webSocket userId newStatus model =
      model
            { contacts = updatedContacts
            } /\
            [ setIt userId (messageIdsUpdated <<< DM.maybe [] _.history $ SIC.findContact userId model.contacts)
            ]

      where
      updatedContacts = map (updateContact userId) model.contacts

      needsUpdate entry = entry.status >= Sent && entry.status < newStatus && entry.recipient == model.user.id
      messageIdsUpdated history = map _.id $ DA.filter needsUpdate history

      updateStatus entry
            | needsUpdate entry = entry { status = newStatus }
            | otherwise = entry
      updateContact ui contact
            | contact.user.id == ui = contact { history = map updateStatus contact.history }
            | otherwise = contact

      setIt ui messages = liftEffect do
            CIW.sendPayload webSocket $ ChangeStatus
                  { status: newStatus
                  , ids: [ ui /\ messages ]
                  }
            CIUN.updateTabCount model updatedContacts
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
                  hs → (userId /\ (map _.id hs)) : running

updateDraft ∷ Int → String → ImModel → NoMessages
updateDraft userId draft model = model { contacts = map update model.contacts } /\ []
      where
      update contact
            | contact.user.id == userId = contact { draft = draft }
            | otherwise = contact

checkFetchContacts ∷ Event → ImModel → MoreMessages
checkFetchContacts event model
      | model.freeToFetchContactList = model /\ [ Just <<< SpecialRequest <<< FetchContacts <$> EC.liftEffect (CIS.isScrolledDown event) ]
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

updateDisplayContacts ∷ Array Contact → Array Int → ImModel → MoreMessages
updateDisplayContacts newContacts userIds model = notifyTrack updatedModel userIds
      where
      updatedModel = model
            { contacts = model.contacts <> onlyNew
            , freeToFetchContactList = true
            }

      existingContactIds = DS.fromFoldable $ map (\cnt → cnt.user.id) model.contacts
      onlyNew = DA.filter (\cnt → not $ DS.member cnt.user.id existingContactIds) newContacts -- if a contact from pagination is already in the list

-- | Messages sent or received while the web socket connection was down
displayMissedContacts ∷ (Array Contact) → ImModel → MoreMessages
displayMissedContacts contacts model = notifyTrack updatedModel $ map userId contacts
      where
      updatedModel = model
            { contacts = map update model.contacts <> newContacts
            }

      userId contact = contact.user.id
      new = DH.fromArrayBy userId identity contacts
      update contact = case DH.lookup contact.user.id new of
            Nothing → contact
            Just found → found { shouldFetchChatHistory = contact.shouldFetchChatHistory, history = CIH.fixHistory $ contact.history <> found.history }

      existing = DH.fromArrayBy userId identity model.contacts
      newContacts = DH.values $ DH.difference new existing

notifyTrack ∷ ImModel → Array Int → MoreMessages
notifyTrack model userIds = track $ CIU.notifyUnreadChats model userIds
      where
      track (m /\ a) = m /\ ((pure $ Just TrackAvailability) : a)

deleteChat ∷ Int → ImModel → MoreMessages
deleteChat userId model =
      case lastMessageId of
            Just id →
                  model
                        { toggleModal = HideUserMenuModal
                        , toggleChatModal = HideChatModal
                        , contacts = DA.filter ((userId /= _) <<< _.id <<< _.user) model.contacts
                        , chatting = if model.chatting == Just userId then Nothing else model.chatting
                        } /\ [ delete id, track ]
            Nothing → F.noMessages model
      where
      lastMessageId = SIC.findContact userId model.contacts >>= (map _.id <<< DA.last <<< _.history)

      delete id = do
            result ← CCN.defaultResponse $ request.im.delete { body: { userId, messageId: id } }
            case result of
                  Left _ → pure <<< Just $ RequestFailed { request: DeleteChat userId, errorMessage: Nothing }
                  _ → pure Nothing
      track = pure $ Just TrackAvailability