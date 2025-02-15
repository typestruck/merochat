module Shared.Im.View.ChatHistory where

import Prelude
import Shared.Availability
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.User

import Client.Im.Swipe as CIT
import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Im.View.Retry as SIVR
import Shared.Im.View.SuggestionProfile as SIVP
import Shared.Markdown as SM

-- | Messages in a chat history
chatHistory ∷ ImModel → Html ImMessage
chatHistory model@{ user: { id: loggedUserId, messageTimestamps, joined, temporary, readReceipts }, toggleModal, toggleContextMenu, failedRequests, freeToFetchChatHistory } =
      HE.div
            ([ HA.id $ show MessageHistory
            , HA.class' { "message-history": true, hidden: DM.isNothing model.chatting }
            ] <> DM.maybe [] (DA.singleton <<< HA.onScroll <<< CheckFetchHistory <<< _.id <<< _.user) model.chatting )
            chatHistoryWindow
      where
      chatHistoryWindow =
            case model.chatting of
                  Nothing → [HE.createEmptyElement "div"]
                  Just chatting →
                        if chatting.user.availability == Unavailable then []
                        else
                              let
                                    entries = retryOrWarning chatting.user.id : temporaryChatWarning <> displayChatHistory chatting
                              in
                                    if chatting.shouldFetchChatHistory || not freeToFetchChatHistory then HE.div' (HA.class' "loading") : entries
                                    else entries

      retryOrWarning id = SIVR.retry "Failed to load chat history" (FetchHistory id true) failedRequests

      temporaryChatWarning = if temporary && isNotTutorial then [ SIVP.signUpCall joined ] else []

      isNotTutorial = case toggleModal of
            Tutorial _ → false
            _ → true

      displayChatHistory { history, user } = DA.mapWithIndex (\i → chatHistoryEntry user $ map _.sender (history !! (i - 1))) history

      bottomMessage id =
            let
                  index = DM.fromMaybe (-100) do
                        history ← _.history <$> model.chatting
                        DA.findIndex (\c → c.id == id) history
                  length = DA.length $ DM.fromMaybe [] (_.history <$> model.chatting)
            in
                  index >= length - 2

      chatHistoryEntry chatPartner previousSender { id, status, date, sender, content, edited } =
            let
                  incomingMessage = sender /= loggedUserId
                  noTimestamps = not messageTimestamps || not chatPartner.messageTimestamps
                  noReadReceipts = not readReceipts || not chatPartner.readReceipts
                  isContextMenuVisible = toggleContextMenu == ShowMessageContextMenu id
            in
                  HE.div
                        [ HA.class'
                                { message: true
                                , "outgoing-message": sender == loggedUserId
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
                                                        , HE.div [ HA.class' { "user-menu in-message": true, visible: isContextMenuVisible, "menu-up": isContextMenuVisible && bottomMessage id } ]
                                                                [ HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick (QuoteMessage content (Right Nothing)) ] "Reply"
                                                                , HE.div [ HA.class' { "user-menu-item menu-item-heading": true, "hidden": sender /= model.user.id || status < Received }, HA.onClick $ EditMessage content id ] "Edit"
                                                                , HE.div [ HA.class' { "user-menu-item menu-item-heading": true, "hidden": sender /= model.user.id || status < Received }, HA.onClick $ DeleteMessage id ] "Unsend"
                                                                ]
                                                        ]
                                                ]
                                        ]
                                , HE.div
                                        ( HA.class'
                                                { duller: status /= Errored
                                                , "error-message": status == Errored
                                                , "message-status": true
                                                }
                                        )
                                        [ HE.span (HA.class' { hidden: not edited }) "Edited - "
                                        , HE.span (HA.class' { hidden: noTimestamps }) $ SD.agoWithTime (DN.unwrap date)
                                        , HE.span (HA.class' { hidden: incomingMessage || noTimestamps || noReadReceipts && status /= Errored }) " - "
                                        , HE.span (HA.class' { hidden: incomingMessage || noReadReceipts && status /= Errored }) $ show status
                                        ]
                                ]
                        ]
