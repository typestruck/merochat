module Shared.Im.Types where

import Prelude
import Shared.Availability
import Shared.Element
import Shared.Modal

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Show.Generic as DGRS
import Data.Tuple (Tuple)
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL
import Effect.Timer (TimeoutId)
import Foreign as F
import Foreign.Object (Object)
import Foreign.Object as FO
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError(..))
import Shared.Changelog (Changelog, ChangelogAction)
import Shared.Content (Content(..))
import Shared.DateTime (DateTimeWrapper)
import Shared.Experiments.Types (ExperimentsMessage(..))
import Shared.Post (Post)
import Shared.Privilege (Privilege)
import Shared.ProfileColumn (ProfileColumn)
import Shared.Resource (Bundle)
import Shared.ResponseError (DatabaseError)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU
import Shared.User (IU, ProfileTab(..))
import Unsafe.Coerce as UC
import Web.Event.Internal.Types (Event)

type User = Record IU

type Report =
      { reason ∷ ReportReason
      , comment ∷ Maybe String
      , userId ∷ Int
      }

type Suggestion = User

type BasicMessage fields =
      { id ∷ Int
      | fields
      }

type ClientMessagePayload = BasicMessage
      ( content ∷ String
      , recipientId ∷ Int
      , senderId ∷ Int
      , date ∷ DateTimeWrapper
      )

type EditedMessagePayload = BasicMessage
      ( content ∷ String
      , recipientId ∷ Int
      , senderId ∷ Int
      )

type DeletedMessagePayload = BasicMessage
      ( userId ∷ Int
      )

type BaseContact fields =
      { -- except for the last few messages, chat history is loaded when clicking on a contact for the first time
        shouldFetchChatHistory ∷ Boolean
      , chatAge ∷ Number
      , lastMessageDate ∷ DateTimeWrapper
      , chatStarter ∷ Int
      | fields
      }

type Contact = BaseContact
      ( user ∷ User
      , typing ∷ Boolean
      , draft ∷ String
      , scrollChatDown ∷ Boolean
      , history ∷ Array HistoryMessage
      )

type HM r =
      ( sender ∷ Int
      , recipient ∷ Int
      , date ∷ DateTimeWrapper
      , edited ∷ Boolean
      , reaction ∷ Maybe String
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

--refactor: these fields can be grouped into inner objects (eg. report: { reason, comment })
type Im =
      ( suggestions ∷ Array Suggestion
      , contacts ∷ Array Contact
      --in case a message from someone blocked was already midway
      , blockedUsers ∷ Array Int
      , temporaryId ∷ Int
      , showBuildProfile ∷ Boolean
      , freeToFetchChatHistory ∷ Boolean
      , freeToFetchContactList ∷ Boolean
      , freeToFetchSuggestions ∷ Boolean
      , temporaryEmail ∷ Maybe String
      , temporaryPassword ∷ Maybe String
      , suggestionsFrom ∷ SuggestionsFrom
      , webSocketMessages ∷ Array WebSocketPayloadServer
      , selectedImage ∷ SelectedImage
      , imageCaption ∷ Maybe String
      , messageEnter ∷ Boolean
      , suggestionsPage ∷ Int
      , webSocketStatus ∷ WebSocketConnectionStatus
      , erroredFields ∷ Array String
      , fortune ∷ Maybe String
      , failedRequests ∷ Array RequestFailure
      , changelogs ∷ Array Changelog
      , errorMessage ∷ String
      , reportReason ∷ Maybe ReportReason
      , reportComment ∷ Maybe String
      , lastTyping ∷ DateTimeWrapper
      , typingIds ∷ Array TimeoutIdWrapper -- TimeoutId constructor is private
      --the current logged in user
      , user ∷ User
      , suggesting ∷ Maybe Int
      , chatting ∷ Maybe Int
      , smallScreen ∷ Boolean
      , showMiniChatInput ∷ Boolean
      , showCollapsedMiniSuggestions ∷ Boolean
      , editing ∷ Maybe Int
      --used to signal that the page should be reloaded
      , hash ∷ String
      --visibility switches
      , initialScreen ∷ Boolean --used on mobile to switch screens
      , fullContactProfileVisible ∷ Boolean
      , showLargeAvatar ∷ Boolean
      , modalsLoaded ∷ Array ScreenModal
      , imUpdated ∷ Boolean
      , react ∷ ReactWith
      , loadingContact ∷ Maybe Int
      , enableNotificationsVisible ∷ Boolean
      , showSuggestionChatInput ∷ Maybe Int
      , showChangelogs ∷ Boolean
      , showSuggestionsPostForm ∷ Boolean
      , toggleContextMenu ∷ ShowContextMenu
      , modal ∷ Modal
      , posts ∷
              { freeToSend ∷ Boolean
              , freeToFetch ∷ Boolean
              , text ∷ Maybe String
              , mode ∷ PostMode
              , link ∷ Maybe String
              , caption ∷ Maybe String
              , image ∷ SelectedImage
              }
      , asks ∷
              { freeToSend ∷ Boolean
              , freeToFetch ∷ Boolean
              , question ∷ Maybe String
              , unallowed ∷ Array Int
              , sent :: Array Int
              }
      )

type ImModel = Record Im

newtype TimeoutIdWrapper = TimeoutIdWrapper TimeoutId

data WebSocketConnectionStatus = Connected | Reconnect | Closed

data AfterLogout
      = LoginPage
      | Banned

type SelectedImage = Maybe
      { width ∷ Int
      , height ∷ Int
      , base64 ∷ String
      }

data ReactWith = WithEmoji | WithText

data ShowContextMenu
      = HideContextMenu
      | ShowUserContextMenu
      | ShowSuggestionContextMenu
      | ShowCompactProfileContextMenu
      | ShowMiniSuggestionContextMenu
      | ShowFullProfileContextMenu
      | ShowMessageContextMenu Int

type Stats =
      { characters ∷ Number
      , interest ∷ Maybe Number
      , replyDelay ∷ Maybe Number -- Minutes
      , accountAge ∷ Number
      }

type Turn =
      { senderStats ∷ Stats
      , recipientStats ∷ Stats
      , chatAge ∷ Number -- Days
      }

type RequestFailure =
      { request ∷ RetryableRequest
      , errorMessage ∷ Maybe String
      }

data RetryableRequest
      = FetchHistory Int Boolean
      | FetchContacts Boolean
      | FetchMissedContacts
      | FetchPosts Int
      | FetchAsks Int
      | ToggleModal Modal
      | BlockUser Int
      | PreviousSuggestion
      | NextSuggestion
      | ReportUser Int
      | DeleteChat Int

data ReportReason
      = DatingContent
      | Harassment
      | HateSpeech
      | Spam
      | Minor
      | OtherReason

type Touch = { startX ∷ Int, endX ∷ Int, startY ∷ Int, endY ∷ Int }

data ImMessage
      =
        --history
        DisplayHistory Int (Array HistoryMessage)
      | SetReactWithText String Event
      | React Int Int (Either String String) Event
      | DisplayReaction Int Int String

      --user menu
      | ToggleInitialScreen Boolean -- | Mobile screen navigation
      | Logout AfterLogout
      | SetContextMenuToggle ShowContextMenu
      | SetModalContents Bundle ElementId String

      --contact
      | ResumeChat Int
      | SetReadStatus (Maybe Int)
      | CheckFetchContacts Event
      | SetDeliveredStatus
      | UpdateDraft Int String
      | DisplayContacts (Array Contact)
      | DisplaySuggestionContact Int (Array Contact)
      | DisplayNewContacts (Array Contact)
      | DisplayMissedContacts (Array Contact)

      --suggestion
      | FetchMoreSuggestions
      | ResumeSuggesting
      | DisplayMoreSuggestions (Array Suggestion)

      | ToggleSuggestionsFromOnline
      | ResumeSuggestionChat Int
      | ToggleSuggestionChatInput Int

      --chat
      | SetSelectedImage SelectedImage
      | ToggleContactProfile
      | ToggleMiniChatInput
      | ToggleLargeAvatar
      | DropFile Event
      | ClearWebSocketMessages
      | ResumeWebSocketMessage (Maybe WebSocketPayloadServer)
      | ToggleMessageEnter
      | FocusInput ElementId
      | QuoteMessage String (Either Touch (Maybe Event))
      | EditMessage String Int
      | DeleteMessage Int
      | HideBuildProfile
      | EnterSendMessage ElementId Event
      | ForceSendMessage ElementId
      | ResizeChatInput Event
      | SendMessage ElementId Content DateTimeWrapper
      | SetEmoji ElementId Event
      | BeforeAudioMessage
      | AudioMessage Touch
      | SendAudioMessage String
      | SetTyping String
      | NoTyping Int
      | TypingId TimeoutId
      | MessageFromExperiment Int String

      --changelog
      | DisplayChangelog (Array Changelog)
      | FetchChangelog
      | PerformChangelogAction (Maybe ChangelogAction) (Maybe Int)
      | ToggleChangelog

      --posts
      | DisplayPosts Int (Array Post)
      | ToggleSuggestionPostForm
      | SetPostText (Maybe String)
      | SetPostLink (Maybe String)
      | SetPostCaption (Maybe String)
      | SendPost
      | SetPostImage SelectedImage
      | ToggleShowing Int For ProfileTab
      | SetPostMode PostMode
      | PreparePostImage Event
      | AfterSendPost Int

      --asks
      | SetAsk (Maybe String)
      | SendAsk Int
      | AfterSendAsk Int Boolean

      --main
      | ReloadPage
      | ToggleUserContextMenu Event
      | Refocus FocusEvent
      | ToggleScrollChatDown Boolean Int
      | SetTheme Theme
      | SpecialRequest RetryableRequest
      | RemoveChatBackground
      | ReconnectWebSocket
      | PushedMessages (Array ClientMessagePayload)
      | SetSmallScreen
      | ReceiveMessage WebSocketPayloadClient Boolean
      | TrackAvailability
      | PreventStop Event
      | AskNotification
      | ToggleAskNotification
      | CloseWebSocket When
      | SetNameFromProfile String
      | SetChatBackgroundFromProfile Boolean (Maybe String)
      | SetAvatarFromProfile (Maybe String)
      | SetCompletedFields (Array ProfileColumn)
      | CheckUserExpiration
      | StartPwa
      | UpdateWebSocketStatus WebSocketConnectionStatus
      | SetField (ImModel → ImModel)
      | TerminateTemporaryUser
      | ToggleFortune Boolean
      | ToggleCollapsedMiniSuggestions
      | DisplayFortune String
      | RequestFailed RequestFailure
      | SetPrivacySettings PrivacySettings
      | CreateUserFromTemporary
      | FinishTutorial
      | SetRegistered

data For = ForSuggestions | ForContacts

data PostMode = TextOnly | LinkOnly | ImageOnly

data Theme = Light | Dark

data When = Always | Desktop

type StatusUpdate =
      { status ∷ MessageStatus
      , ids ∷ Array (Tuple Int (Array Int))
      }

data WebSocketPayloadServer
      = UpdateHash
      | UpdatePrivileges
      | UpdateAvailability { online ∷ Boolean }
      | Posted { id ∷ Int }
      | TrackAvailabilityFor { ids ∷ Array Int }
      | Ping
      | Typing { id ∷ Int }
      | OutgoingMessage OutgoingRecord
      | EditedMessage EditedRecord
      | DeletedMessage DeletedRecord
      | ChangeStatus StatusUpdate
      | UnavailableFor { id ∷ Int }
      | Ban { id ∷ Int, secret ∷ String }

type OutgoingRecord =
      BasicMessage
            ( userId ∷ Int
            , userName ∷ String
            , content ∷ Content
            , turn ∷ Maybe Turn
            )

type Challenge =
      { algorithm ∷ String
      , challenge ∷ String
      , maxnumber ∷ Number
      , salt ∷ String
      , signature ∷ String
      }

type EditedRecord = { id ∷ Int, userId ∷ Int, content ∷ Content }

type DeletedRecord = { id ∷ Int, userId ∷ Int }

data FullWebSocketPayloadClient
      = Pong
      | Content WebSocketPayloadClient
      | CloseConnection AfterLogout

data WebSocketPayloadClient
      = CurrentHash String
      | CurrentPrivileges { karma ∷ Int, privileges ∷ Array Privilege }
      | NewIncomingMessage ClientMessagePayload
      | NewEditedMessage EditedMessagePayload
      | NewPost { userId ∷ Int, post ∷ Post }
      | NewDeletedMessage DeletedMessagePayload
      | TrackedAvailability { id ∷ Int, availability ∷ Availability }
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
      | BadMessage { userId ∷ Int, temporaryMessageId ∷ Maybe Int } --either lacks privilege or forbidden html tags
      | PayloadError { origin ∷ WebSocketPayloadServer, context ∷ Maybe DatabaseError }

data MessageError = UserUnavailable | InvalidMessage

data FocusEvent = VisibilityChange | FocusBlur

data SuggestionsFrom = ThisWeek | LastTwoWeeks | LastMonth | All | OnlineOnly

instance EncodeQueryParam SuggestionsFrom where
      encodeQueryParam = Just <<< show <<< DE.fromEnum

instance DecodeQueryParam SuggestionsFrom where
      decodeQueryParam query key =
            case FO.lookup key query of
                  Nothing → Left $ QueryParamNotFound { key, queryObj: query }
                  Just [ value ] → DM.maybe (errorDecoding query key) Right (DI.fromString value >>= DE.toEnum)
                  _ → errorDecoding query key

derive instance Eq SuggestionsFrom
derive instance Eq FocusEvent
derive instance Eq ReactWith
derive instance Eq PostMode
derive instance Eq When
derive instance Eq WebSocketConnectionStatus

derive instance Ord ReportReason
derive instance Ord PostMode
derive instance Ord MessageStatus
derive instance Ord SuggestionsFrom

instance Bounded SuggestionsFrom where
      bottom = ThisWeek
      top = All

instance Bounded MessageStatus where
      bottom = Received
      top = Read

instance Bounded ReportReason where
      bottom = DatingContent
      top = OtherReason

instance BoundedEnum SuggestionsFrom where
      cardinality = Cardinality 1
      fromEnum = case _ of
            OnlineOnly → 0
            ThisWeek → 1
            LastTwoWeeks → 2
            LastMonth → 3
            All → 4
      toEnum = case _ of
            0 → Just OnlineOnly
            1 → Just ThisWeek
            2 → Just LastTwoWeeks
            3 → Just LastMonth
            4 → Just All
            _ → Nothing

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

instance Enum SuggestionsFrom where
      succ = case _ of
            OnlineOnly → Just ThisWeek
            ThisWeek → Just LastTwoWeeks
            LastTwoWeeks → Just LastMonth
            LastMonth → Just All
            All → Nothing
      pred = case _ of
            OnlineOnly → Nothing
            ThisWeek → Just OnlineOnly
            LastTwoWeeks → Just ThisWeek
            LastMonth → Just LastTwoWeeks
            All → Just LastMonth

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

instance DecodeJson SuggestionsFrom where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson WebSocketPayloadServer where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson AfterLogout where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson PostMode where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson WebSocketPayloadClient where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ShowContextMenu where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson RetryableRequest where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ReportReason where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson MessageStatus where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson WebSocketConnectionStatus where
      decodeJson = DADGR.genericDecodeJson

instance DecodeJson ReactWith where
      decodeJson = DADGR.genericDecodeJson

instance EncodeJson WebSocketConnectionStatus where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ReactWith where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson TimeoutIdWrapper where
      encodeJson = UC.unsafeCoerce

instance EncodeJson SuggestionsFrom where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson AfterLogout where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson WebSocketPayloadServer where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson WebSocketPayloadClient where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson PostMode where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson ShowContextMenu where
      encodeJson = DAEGR.genericEncodeJson

instance EncodeJson RetryableRequest where
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

instance Show WebSocketPayloadClient where
      show = DGRS.genericShow

instance Show WebSocketPayloadServer where
      show = DGRS.genericShow

instance Show MessageError where
      show = DGRS.genericShow

derive instance Eq ShowContextMenu
derive instance Eq AfterLogout
derive instance Eq MessageError
derive instance Eq RetryableRequest
derive instance Eq ReportReason
derive instance Eq MessageStatus

derive instance Generic MessageStatus _
derive instance Generic WebSocketConnectionStatus _
derive instance Generic PostMode _
derive instance Generic SuggestionsFrom _
derive instance Generic AfterLogout _
derive instance Generic ReportReason _
derive instance Generic MessageError _
derive instance Generic ReactWith _
derive instance Generic WebSocketPayloadClient _
derive instance Generic FullWebSocketPayloadClient _
derive instance Generic WebSocketPayloadServer _
derive instance Generic ShowContextMenu _
derive instance Generic RetryableRequest _

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
