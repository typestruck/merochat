module Shared.IM.Types where

import Data.Maybe(Maybe(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Generic.Rep (class Generic)
import Data.Show.Generic as DGRS
import Prelude

import Data.Either(Either(..))
import Droplet.Language
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode as DAD
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Shared.User


type IMUser = Record IU

newtype DateTimeWrapper = DateTimeWrapper DateTime


type Suggestion = IMUser

type BasicMessage fields = {
      id :: Int,
      experimenting :: Maybe ExperimentPayload |
      fields
}

type ClientMessagePayload = (BasicMessage (
      content :: String,
      userID :: Int,
      date :: DateTimeWrapper
))

type Contact = {
      shouldFetchChatHistory :: Boolean, -- except for the last few messages, chat history is loaded when clicking on a contact for the first time
      user :: IMUser,
      available :: Boolean,
      chatAge :: Number, --Days,
      chatStarter :: Int,
      impersonating :: Maybe Int,
      history :: Array HistoryMessage
}

type HistoryMessage = {
      id :: Int,
      sender :: Int,
      recipient :: Int,
      date :: DateTimeWrapper,
      content :: String,
      status :: MessageStatus
}

data MessageStatus =
      Errored |
      Sent |
      Received |
      Delivered |
      Read

type MessageIDTemporary = {
      id :: Int,
      temporaryID :: Int
}

--refactor: move wrappers to server/types
newtype MessageIDTemporaryWrapper = MessageIDTemporaryWrapper MessageIDTemporary

type MissedEvents = {
      contacts :: Array Contact,
      messageIDs :: Array MessageIDTemporary
}


--refactor: these fields can be grouped into inner objects (eg. report: { reason, comment })
type IM = (
      suggestions :: Array Suggestion,
      contacts :: Array Contact,
      --in case a message from someone blocked was already midway
      blockedUsers :: Array Int,
      temporaryID :: Int,
      freeToFetchChatHistory :: Boolean,
      freeToFetchContactList :: Boolean,
      freeToFetchSuggestions :: Boolean,
      selectedImage :: Maybe String,
      imageCaption :: Maybe String,
      messageEnter :: Boolean,
      link :: Maybe String,
      suggestionsPage :: Int,
      linkText :: Maybe String,
      isWebSocketConnected :: Boolean,
      erroredFields :: Array String,
      fortune :: Maybe String,
      failedRequests :: Array RequestFailure,
      errorMessage :: String,
      experimenting :: Maybe ExperimentData,
      modalsLoaded :: Array ShowUserMenuModal,
      reportReason :: Maybe ReportReason,
      reportComment :: Maybe String,
      --the current logged in user
      user :: IMUser,
      --indexes
      suggesting :: Maybe Int,
      chatting :: Maybe Int,
      smallScreen :: Boolean,
      --used to signal that the page should be reloaded
      hash :: String,
      --visibility switches
      initialScreen :: Boolean, --used on mobile to switch screens
      hasTriedToConnectYet :: Boolean,
      fullContactProfileVisible :: Boolean,
      imUpdated :: Boolean,
      enableNotificationsVisible :: Boolean,
      toggleContextMenu :: ShowContextMenu,
      toggleModal :: ShowUserMenuModal,
      toggleChatModal :: ShowChatModal
)

type IMModel = Record IM

data ShowChatModal =
      HideChatModal |
      ShowSelectedImage |
      ShowPreview |
      ShowEmojis |
      ShowLinkForm

data ShowContextMenu =
      HideContextMenu |
      ShowUserContextMenu |
      ShowSuggestionContextMenu |
      ShowCompactProfileContextMenu |
      ShowFullProfileContextMenu

data ShowUserMenuModal =
      HideUserMenuModal |
      ConfirmLogout |
      ConfirmTermination |
      ShowExperiments |
      ShowProfile |
      ShowSettings |
      ShowLeaderboard |
      ShowHelp |
      ShowBacker |
      ShowReport Int


type Stats = {
    characters :: Number,
    interest :: Number
}

type Turn = {
    senderStats :: Stats,
    recipientStats:: Stats,
    chatAge :: Number, -- Days,
    replyDelay :: Number --Seconds
}

data ProfilePresentation =
      FullContactProfile |
      CurrentSuggestion |
      OtherSuggestion

data MessageContent =
      Image String String | --caption & base64
      Text String

data Markup =
      Bold |
      Italic |
      Strike |
      Heading |
      OrderedList |
      UnorderedList



type RequestFailure = {
      request :: RetryableRequest,
      errorMessage :: String
}

data RetryableRequest =
      FetchHistory Boolean |
      FetchContacts Boolean |
      CheckMissedEvents |
      ToggleModal ShowUserMenuModal |
      BlockUser Int |
      PreviousSuggestion |
      NextSuggestion |
      ReportUser Int

data ReportReason = DatingContent | Harrassment | HateSpeech | Spam | OtherReason

data IMMessage =
      --history
      CheckFetchHistory |
      DisplayHistory (Array HistoryMessage)|
      --user menu
      ToggleInitialScreen Boolean |
      Logout |
      SetContextMenuToggle ShowContextMenu |
      SetModalContents (Maybe String) ElementID String |
      --contact
      ResumeChat (Tuple Int (Maybe Int)) |
      UpdateReadCount |
      CheckFetchContacts |
      DisplayContacts (Array Contact) |
      DisplayNewContacts (Array Contact) |
      DisplayImpersonatedContact Int HistoryMessage (Array Contact) |
      ResumeMissedEvents MissedEvents |
      --suggestion
      FetchMoreSuggestions |
      ResumeSuggesting |
      DisplayMoreSuggestions (Array Suggestion) |
      --chat
      SetSelectedImage (Maybe String) |
      ToggleContactProfile |
      DropFile Event |
      ToggleMessageEnter |
      FocusInput ElementID |
      EnterBeforeSendMessage Event |
      ForceBeforeSendMessage |
      ResizeChatInput Event |
      BeforeSendMessage MessageContent |
      SendMessage MessageContent DateTimeWrapper |
      SetMessageContent (Maybe Int) String |
      Apply Markup |
      SetSmallScreen |
      SetEmoji Event |
      InsertLink |
      --main
      AskChatExperiment |
      SetChatExperiment (Maybe ExperimentData) |
      ReloadPage |
      ToggleUserContextMenu Event |
      SpecialRequest RetryableRequest |
      ReceiveMessage WebSocketPayloadClient Boolean |
      PreventStop Event |
      AskNotification |
      ToggleAskNotification |
      SetNameFromProfile String |
      ToggleConnected Boolean |
      SetField (IMModel -> IMModel) |
      ToggleFortune Boolean |
      DisplayFortune String |
      RequestFailed RequestFailure |
      ToggleChatModal ShowChatModal

data WebSocketPayloadServer =
      UpdateHash |
      Ping |
      OutgoingMessage (BasicMessage (
            userID :: Int,
            content :: MessageContent,
            turn :: Maybe Turn
      )) |
      ChangeStatus {
            userID :: Int,
            status :: MessageStatus,
            persisting :: Boolean, -- in some cases status changs should be not persisted to the database
            --alternatively, update by user?
            ids :: Array Int
      } |
      ToBlock {
            id :: Int
      }

data FullWebSocketPayloadClient =
      Pong |
      Content WebSocketPayloadClient

data WebSocketPayloadClient =
      CurrentHash String |
      NewIncomingMessage ClientMessagePayload |
      ServerReceivedMessage {
          previousID :: Int,
          id :: Int,
          userID :: Int
      } |
      ServerChangedStatus {
            ids :: Array Int,
            status :: MessageStatus,
            userID :: Int
      } |
      BeenBlocked { id :: Int } |
      PayloadError { origin :: WebSocketPayloadServer, context :: Maybe DatabaseError }

data DatabaseError = MissingForeignKey
