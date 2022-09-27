module Shared.Im.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Array as DA
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Hashable as HS
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Show.Generic as DGRS
import Data.String.Regex as DSRG
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSRU
import Data.Tuple (Tuple)
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Effect.Timer (TimeoutId)
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.DateTime (DateTimeWrapper)
import Shared.Experiments.Types (ExperimentData, ExperimentPayload)
import Shared.Resource (Bundle)
import Shared.Element
import Shared.ResponseError (DatabaseError)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU
import Shared.User (Availability, IU)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Unsafe.Coerce as UC
import Web.Event.Internal.Types (Event)

type ImUser = Record IU

type Report =
      { reason ∷ ReportReason
      , comment ∷ Maybe String
      , userId ∷ Int
      }

type Suggestion = ImUser

type BasicMessage fields =
      { id ∷ Int
      , experimenting ∷ Maybe ExperimentPayload
      | fields
      }

type ClientMessagePayload = BasicMessage
      ( content ∷ String
      , userId ∷ Int
      , date ∷ DateTimeWrapper
      )

type BaseContact fields =
      { -- except for the last few messages, chat history is loaded when clicking on a contact for the first time
        shouldFetchChatHistory ∷ Boolean
      , chatAge ∷ Number
      , lastMessageDate ∷ DateTimeWrapper
      , chatStarter ∷ Int
      , impersonating ∷ Maybe Int
      | fields
      }

type Contact = BaseContact
      ( user ∷ ImUser
      , typing ∷ Boolean
      , history ∷ Array HistoryMessage
      )

type HM r =
      ( sender ∷ Int
      , recipient ∷ Int
      , date ∷ DateTimeWrapper
      , content ∷ String
      , status ∷ MessageStatus
      | r
      )

type HistoryMessage = Record (HM (id ∷ Int))

data MessageStatus
      = Errored
      | Sent
      | Received
      | Delivered
      | Read

type TemporaryMessageId =
      { id ∷ Int
      , temporaryId ∷ Int
      }

type MissedEvents =
      { contacts ∷ Array Contact
      , messageIds ∷ Array TemporaryMessageId
      }

--refactor: these fields can be grouped into inner objects (eg. report: { reason, comment })
type Im =
      ( suggestions ∷ Array Suggestion
      , contacts ∷ Array Contact
      --in case a message from someone blocked was already midway
      , blockedUsers ∷ Array Int
      , temporaryId ∷ Int
      , freeToFetchChatHistory ∷ Boolean
      , temporaryEmail ∷ Maybe String
      , temporaryPassword ∷ Maybe String
      , freeToFetchContactList ∷ Boolean
      , freeToFetchSuggestions ∷ Boolean
      , selectedImage ∷ Maybe String
      , imageCaption ∷ Maybe String
      , messageEnter ∷ Boolean
      , link ∷ Maybe String
      , suggestionsPage ∷ Int
      , linkText ∷ Maybe String
      , isWebSocketConnected ∷ Boolean
      , erroredFields ∷ Array String
      , fortune ∷ Maybe String
      , failedRequests ∷ Array RequestFailure
      , errorMessage ∷ String
      , experimenting ∷ Maybe ExperimentData
      , modalsLoaded ∷ Array ShowUserMenuModal
      , reportReason ∷ Maybe ReportReason
      , reportComment ∷ Maybe String
      , lastTyping ∷ DateTimeWrapper
      , typingIds ∷ Array TimeoutIdWrapper -- TimeoutId constructor is private
      --the current logged in user
      , user ∷ ImUser
      --indexes
      , suggesting ∷ Maybe Int
      , chatting ∷ Maybe Int
      , smallScreen ∷ Boolean
      --used to signal that the page should be reloaded
      , hash ∷ String
      --visibility switches
      , initialScreen ∷ Boolean --used on mobile to switch screens
      , hasTriedToConnectYet ∷ Boolean
      , fullContactProfileVisible ∷ Boolean
      , imUpdated ∷ Boolean
      , enableNotificationsVisible ∷ Boolean
      , toggleContextMenu ∷ ShowContextMenu
      , toggleModal ∷ ShowUserMenuModal --refactor: toggleModal and toggleChatModal should be merged
      , toggleChatModal ∷ ShowChatModal
      )

type ImModel = Record Im

newtype TimeoutIdWrapper = TimeoutIdWrapper TimeoutId

data ShowChatModal
      = HideChatModal
      | ShowSelectedImage
      | ShowPreview
      | ShowEmojis
      | ShowLinkForm

data ShowContextMenu
      = HideContextMenu
      | ShowUserContextMenu
      | ShowSuggestionContextMenu
      | ShowCompactProfileContextMenu
      | ShowFullProfileContextMenu
      | ShowContactContextMenu (Tuple Int (Maybe Int))

data ShowUserMenuModal
      = HideUserMenuModal
      | ConfirmLogout
      | ConfirmTerminationTemporaryUser
      | ConfirmDeleteChat (Tuple Int (Maybe Int))
      | ConfirmBlockUser (Tuple Int (Maybe Int))
      | ShowExperiments
      | ShowProfile
      | ShowSettings
      | ShowLeaderboard
      | ShowHelp
      | ShowBacker
      | ShowFeedback
      | ShowReport Int
      | Tutorial Step

data Step
      = Welcome
      | ChatSuggestions
      | Chatting
      | BackSuggestions
      | ChatList
      | OptionsMenu

type Stats =
      { characters ∷ Number
      , interest ∷ Number
      , replyDelay ∷ Maybe Number -- Minutes
      , accountAge :: Number
      }

type Turn =
      { senderStats ∷ Stats
      , recipientStats ∷ Stats
      , chatAge ∷ Number -- Days
      }

data ProfilePresentation
      = FullContactProfile
      | CenterCard
      | PreviousCard
      | NextCard

data MessageContent
      = Image String String
      | --caption & base64
        Text String

data Markup
      = Bold
      | Italic
      | Strike
      | Heading
      | OrderedList
      | UnorderedList

type RequestFailure =
      { request ∷ RetryableRequest
      , errorMessage ∷ Maybe String
      }

data RetryableRequest
      = FetchHistory Boolean
      | FetchContacts Boolean
      | CheckMissedEvents
      | ToggleModal ShowUserMenuModal
      | BlockUser (Tuple Int (Maybe Int))
      | PreviousSuggestion
      | NextSuggestion
      | ReportUser Int
      | DeleteChat (Tuple Int (Maybe Int))

data ReportReason = DatingContent | Harassment | HateSpeech | Spam | Minor | OtherReason

data ImMessage
      =
        --history
        CheckFetchHistory
      | DisplayHistory (Array HistoryMessage)

      --user menu
      | ToggleInitialScreen Boolean -- | Mobile screen navigation
      | Logout
      | SetContextMenuToggle ShowContextMenu
      | SetModalContents (Maybe Bundle) ElementId String

      --contact
      | ResumeChat (Tuple Int (Maybe Int))
      | UpdateReadCount
      | CheckFetchContacts
      | UpdateDelivered
      | DisplayContacts (Array Contact)
      | DisplayNewContacts (Array Contact)
      | DisplayImpersonatedContact Int HistoryMessage (Array Contact)
      | ResumeMissedEvents MissedEvents

      --suggestion
      | FetchMoreSuggestions
      | ResumeSuggesting
      | DisplayMoreSuggestions (Array Suggestion)

      --chat
      | SetSelectedImage (Maybe String)
      | ToggleContactProfile
      | DropFile Event
      | ToggleMessageEnter
      | FocusInput ElementId
      | QuoteMessage String Event
      | FocusCurrentSuggestion
      | EnterBeforeSendMessage Event
      | ForceBeforeSendMessage
      | ResizeChatInput Event
      | BeforeSendMessage MessageContent
      | SendMessage MessageContent DateTimeWrapper
      | SetMessageContent (Maybe Int) String
      | Apply Markup
      | SetSmallScreen
      | SetEmoji Event
      | InsertLink
      | CheckTyping String
      | NoTyping Int
      | TypingId TimeoutId

      --main
      | DisplayAvailability AvailabilityStatus
      | SendPing Boolean
      | AskChatExperiment
      | SetChatExperiment (Maybe ExperimentData)
      | ReloadPage
      | FinishTutorial
      | ToggleUserContextMenu Event
      | SpecialRequest RetryableRequest
      | ReceiveMessage WebSocketPayloadClient Boolean
      | PreventStop Event
      | AskNotification
      | ToggleAskNotification
      | SetNameFromProfile String
      | SetAvatarFromProfile (Maybe String)
      | CheckUserExpiration
      | ToggleConnected Boolean
      | SetField (ImModel → ImModel)
      | TerminateTemporaryUser
      | ToggleFortune Boolean
      | DisplayFortune String
      | RequestFailed RequestFailure
      | SetPrivacySettings PrivacySettings
      | CreateUserFromTemporary
      | SetRegistered
      | ToggleChatModal ShowChatModal

data WebSocketPayloadServer
      = UpdateHash
      | Ping
              { isActive ∷ Boolean
              , statusFor ∷ Array Int
              }
      | Typing { id ∷ Int }
      | OutgoingMessage OutgoingRecord
      | ChangeStatus
              { status ∷ MessageStatus
              , persisting ∷ Boolean
              , ids ∷ Array (Tuple Int (Array Int))
              }
      | UnavailableFor
              { id ∷ Int
              }

type OutgoingRecord =
      BasicMessage
            ( userId ∷ Int
            , content ∷ MessageContent
            , turn ∷ Maybe Turn
            )

type AvailabilityStatus = Array { id ∷ Int, status ∷ Availability }

data FullWebSocketPayloadClient
      = Pong { status ∷ AvailabilityStatus }
      | Content WebSocketPayloadClient

data WebSocketPayloadClient
      = CurrentHash String
      | NewIncomingMessage ClientMessagePayload
      | ContactTyping { id ∷ Int }
      | ServerReceivedMessage
              { previousId ∷ Int
              , id ∷ Int
              , userId ∷ Int
              }
      | ServerChangedStatus
              { ids ∷ Array Int
              , status ∷ MessageStatus
              , userId ∷ Int
              }
      | ContactUnavailable { userId ∷ Int, temporaryMessageId ∷ Maybe Int } --either block or change of privacy settings
      | PayloadError { origin ∷ WebSocketPayloadServer, context ∷ Maybe DatabaseError }

newtype ArrayPrimaryKey = ArrayPrimaryKey (Array Int)

derive instance Ord ReportReason
derive instance Ord MessageStatus

instance ReadForeign MessageStatus where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance ReadForeign ReportReason where
      readImpl value = SU.fromJust <<< DE.toEnum <$> F.readInt value

instance WriteForeign ReportReason where
      writeImpl reason = F.unsafeToForeign $ DE.fromEnum reason

instance WriteForeign MessageStatus where
      writeImpl messageStatus = F.unsafeToForeign $ DE.fromEnum messageStatus

instance Bounded MessageStatus where
      bottom = Received
      top = Read

instance Bounded ReportReason where
      bottom = DatingContent
      top = OtherReason

instance BoundedEnum MessageStatus where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Errored → -1
            Sent → 0
            Received → 1
            Delivered → 2
            Read → 3
      toEnum = case _ of
            -1 → Just Errored
            0 → Just Sent
            1 → Just Received
            2 → Just Delivered
            3 → Just Read
            _ → Nothing

instance BoundedEnum ReportReason where
      cardinality = Cardinality 1
      fromEnum = case _ of
            DatingContent → 0
            Harassment → 1
            HateSpeech → 2
            Spam → 3
            Minor → 4
            OtherReason → 255
      toEnum = case _ of
            0 → Just DatingContent
            1 → Just Harassment
            2 → Just HateSpeech
            3 → Just Spam
            4 → Just Minor
            255 → Just OtherReason
            _ → Nothing

instance Enum ReportReason where
      succ = case _ of
            DatingContent → Just Harassment
            Harassment → Just HateSpeech
            HateSpeech → Just Spam
            Spam → Just Minor
            Minor → Just OtherReason
            OtherReason → Nothing
      pred = case _ of
            DatingContent → Nothing
            Harassment → Just DatingContent
            HateSpeech → Just Harassment
            Spam → Just HateSpeech
            Minor → Just Spam
            OtherReason → Just Minor

instance Enum MessageStatus where
      succ = case _ of
            Errored → Just Received
            Sent → Just Sent
            Received → Just Delivered
            Delivered → Just Read
            Read → Nothing
      pred = case _ of
            Errored → Nothing
            Sent → Just Sent
            Received → Just Errored
            Delivered → Just Received
            Read → Just Delivered

instance DecodeJson TimeoutIdWrapper where
      decodeJson = Right <<< UC.unsafeCoerce

instance DecodeJson WebSocketPayloadServer where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson MessageContent where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ShowUserMenuModal where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson Step where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson WebSocketPayloadClient where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ShowContextMenu where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson RetryableRequest where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ShowChatModal where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ReportReason where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson MessageStatus where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson TimeoutIdWrapper where
      encodeJson = UC.unsafeCoerce

instance EncodeJson WebSocketPayloadServer where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson MessageContent where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson Step where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ShowUserMenuModal where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson WebSocketPayloadClient where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ShowContextMenu where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson RetryableRequest where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ShowChatModal where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ReportReason where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson MessageStatus where
      encodeJson = DAEGR.genericEncodeJson

instance Show MessageStatus where
      show = case _ of
            Errored → "Failed to send"
            Sent → "Sending"
            Received → "Sent"
            Delivered → "Unread"
            Read → "Read"

instance Show ReportReason where
      show = case _ of
            DatingContent → "Dating content"
            Harassment → "Harassment/Bullying"
            HateSpeech → "Hate Speech/Call to violence"
            Minor → "User is a minor"
            Spam → "Spam/Product placement"
            OtherReason → "Other"

instance Show ShowUserMenuModal where
      show = case _ of
            ShowProfile → "Your profile"
            ShowSettings → "Your settings"
            ShowLeaderboard → "Karma leaderboard"
            ShowHelp → "Help"
            ShowExperiments → "Chat experiments"
            ShowBacker → "Backing"
            ShowFeedback -> "Send feedback"
            _ → ""

instance Show MessageContent where
      show = DGRS.genericShow

instance Show WebSocketPayloadClient where
      show = DGRS.genericShow

instance Show WebSocketPayloadServer where
      show = DGRS.genericShow

instance EncodeQueryParam ArrayPrimaryKey where
      encodeQueryParam (ArrayPrimaryKey ap) = Just $ show ap

instance DecodeQueryParam ArrayPrimaryKey where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing → Left $ QueryParamNotFound { key, queryObj: query }
                  --this is terrible
                  Just [ value ] → Right <<< ArrayPrimaryKey <<< DA.catMaybes <<< map DI.fromString $ DSRG.split (DSRU.unsafeRegex "\\D" noFlags) value
                  _ → errorDecoding query key

derive instance Eq ShowContextMenu

derive instance Eq ProfilePresentation
derive instance Eq RetryableRequest
derive instance Eq ShowChatModal
derive instance Eq Step
derive instance Eq ShowUserMenuModal
derive instance Eq ReportReason
derive instance Eq MessageStatus

derive instance Generic MessageStatus _
derive instance Generic Step _
derive instance Generic ReportReason _
derive instance Generic MessageContent _
derive instance Generic WebSocketPayloadClient _
derive instance Generic FullWebSocketPayloadClient _
derive instance Generic WebSocketPayloadServer _
derive instance Generic ShowUserMenuModal _
derive instance Generic ShowContextMenu _
derive instance Generic RetryableRequest _
derive instance Generic ShowChatModal _

instance ToValue MessageStatus where
      toValue v = F.unsafeToForeign $ DE.fromEnum v

instance ToValue ReportReason where
      toValue v = F.unsafeToForeign $ DE.fromEnum v

instance FromValue ReportReason where
      fromValue v = SU.fromJust <<< DE.toEnum <$> (DL.fromValue v ∷ Either String Int)

instance FromValue MessageStatus where
      fromValue v = SU.fromJust <<< DE.toEnum <$> (DL.fromValue v ∷ Either String Int)

errorDecoding ∷ ∀ a. Object (Array String) → String → Either DecodeError a
errorDecoding queryObj key = Left $ QueryDecodeError
      { values: []
      , message: "Could not decode parameter " <> key
      , key
      , queryObj
      }
