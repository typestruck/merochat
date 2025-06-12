module Shared.Im.View.ChatHistory where

import Prelude
import Shared.Availability
import Shared.Im.Types
import Shared.User

import Client.Im.Swipe as CIT
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype as DN
import Effect (Effect)
import Effect.Uncurried (EffectFn5)
import Effect.Uncurried as EU
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.View.Retry as SIVR
import Shared.Im.View.SuggestionProfile as SIVP
import Shared.Markdown as SM
import Shared.Unsafe as SU
import Shared.Im.Scroll as SIS
import Web.Event.Internal.Types (Event)

data ScrollAction = ResetScrollDown | SetScrollDown | Fetch | None

foreign import runScrollEvent_ ∷ EffectFn5 Event ScrollAction ScrollAction ScrollAction ScrollAction ScrollAction

--see foreign file
runScrollEvent ∷ Event → ScrollAction → ScrollAction → ScrollAction → ScrollAction → Effect ScrollAction
runScrollEvent = EU.runEffectFn5 runScrollEvent_

--what we are trying to accomplish here is (using scrollend when possible) twofold:
-- determine weather or not to scroll down to incoming messages
-- check if older chat history should be fetched
onScrollEvent ∷ Int → NodeData ImMessage
onScrollEvent userId = HA.createRawEvent SIS.scrollEventName handler
      where
      handler event = do
            what ← runScrollEvent event None SetScrollDown ResetScrollDown Fetch
            pure $ case what of
                  None → Nothing
                  SetScrollDown → Just $ ToggleScrollChatDown true userId
                  ResetScrollDown → Just $ ToggleScrollChatDown false userId
                  Fetch → Just <<< SpecialRequest $ FetchHistory userId true

-- | Messages in a chat history
chatHistory ∷ ImModel → Html ImMessage
chatHistory model =
      case SIC.maybeFindContact model.chatting model.contacts of
            Just chatting →
                  HE.div
                        [ HA.id $ show MessageHistory
                        , HA.class' "message-history"
                        , onScrollEvent chatting.user.id
                        ] $
                        if chatting.user.availability == Unavailable then []
                        else
                              let
                                    entries = retryOrWarning chatting.user.id : temporaryChatWarning <> displayChatHistory chatting
                              in
                                    if chatting.shouldFetchChatHistory || not model.freeToFetchChatHistory then HE.div' (HA.class' "loading") : entries
                                    else entries
            Nothing → HE.div'
                  [ HA.id $ show MessageHistory
                  , HA.class' "message-history hidden"
                  ]
      where
      retryOrWarning id = SIVR.retry "Failed to load chat history" (FetchHistory id true) model.failedRequests
      temporaryChatWarning = if model.user.temporary && isNotTutorial then [ SIVP.signUpCall model.user.joined ] else []
      isNotTutorial = case model.toggleModal of
            Tutorial _ → false
            _ → true

      displayChatHistory contact = DA.mapWithIndex (\i → chatHistoryEntry contact $ map _.sender (contact.history !! (i - 1))) contact.history
      chatHistoryEntry contact previousSender { id, status, date, sender, content, edited } =
            let
                  incomingMessage = sender /= model.user.id
                  noTimestamps = not model.user.messageTimestamps || not contact.user.messageTimestamps
                  noReadReceipts = not model.user.readReceipts || not contact.user.readReceipts
                  isContextMenuVisible = model.toggleContextMenu == ShowMessageContextMenu id
            in
                  HE.div
                        [ HA.class'
                                { message: true
                                , "outgoing-message": sender == model.user.id
                                , "incoming-message": incomingMessage
                                , "same-bubble-message": previousSender == Just sender -- only the first message in a row has a bubble handle
                                }
                        , HA.onDblclick' (QuoteMessage content <<< Right <<< Just)
                        ]
                        [ HE.div
                                [ HA.class' { "message-content": true, "editing-message": Just id == model.editing }, HA.id $ "m" <> show id, CIT.onTouchStart Nothing, CIT.onTouchEnd (QuoteMessage content <<< Left) ] -- id is used to scroll into view
                                [ HE.div [ HA.class' "message-content-in" ]
                                        [ HE.div' [ HA.innerHtml $ SM.parse content ]
                                        , HE.div (HA.class' "message-context-options")
                                                [ HE.div [ HA.class' { "message-context-menu outer-user-menu": true, visible: isContextMenuVisible }, HA.onClick <<< SetContextMenuToggle $ ShowMessageContextMenu id ]
                                                        [ HE.svg [ HA.class' "svg-32 svg-duller", HA.viewBox "0 0 16 16" ]
                                                                [ HE.polygon' [ HA.transform "rotate(90,7.6,8)", HA.points "11.02 7.99 6.53 3.5 5.61 4.42 9.17 7.99 5.58 11.58 6.5 12.5 10.09 8.91 10.1 8.91 11.02 7.99" ]
                                                                ]
                                                        , HE.div [ HA.class' { "user-menu in-message": true, visible: isContextMenuVisible, "menu-up": isContextMenuVisible && isBottomMessage contact.history id } ]
                                                                [ HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick (QuoteMessage content (Right Nothing)) ] "Reply"
                                                                , HE.div [ HA.class' { "user-menu-item menu-item-heading": true, "hidden": sender /= model.user.id || status < Received }, HA.onClick $ EditMessage content id ] "Edit"
                                                                , HE.div [ HA.class' { "user-menu-item menu-item-heading": true, "hidden": sender /= model.user.id || status < Received }, HA.onClick $ DeleteMessage id ] "Unsend"
                                                                ]
                                                        ]
                                                ]
                                        ]
                                , HE.div
                                        ( HA.class'
                                                { "error-message": status == Errored
                                                , "message-status": true
                                                }
                                        )
                                        [ HE.span (HA.class' { hidden: not edited }) "Edited - "
                                        , HE.span (HA.class' { hidden: noTimestamps }) $ SD.agoWithTime (DN.unwrap date)
                                        , HE.span (HA.class' { hidden: incomingMessage || noReadReceipts && status /= Errored }) (" - " <> show status)
                                        ]
                                ]
                        ]

      isBottomMessage history id = (SU.fromJust $ DA.findIndex ((_ == id) <<< _.id) history) >= DA.length history - 2

