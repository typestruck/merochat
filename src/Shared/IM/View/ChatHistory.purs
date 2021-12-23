module Shared.IM.View.ChatHistory where

import Prelude

import Data.Array ((!!), (:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Flame (Html)
import Flame.Html.Attribute as HA
import Shared.User
import Flame.Html.Element as HE
import Shared.Experiments.Types
import Shared.IM.Types
import Shared.DateTime as SD
import Shared.IM.View.Retry as SIVR
import Shared.Markdown as SM

-- | Messages in a chat history
chatHistory ∷ IMModel → Maybe Contact → Html IMMessage
chatHistory { user: { id: loggedUserId, messageTimestamps, readReceipts }, experimenting, failedRequests, freeToFetchChatHistory } contact =
      HE.div
            [ HA.id $ show MessageHistory
            , HA.class' { "message-history": true, hidden: DM.isNothing contact }
            , HA.onScroll CheckFetchHistory
            ]
            chatHistoryWindow
      where
      chatHistoryWindow =
            case contact of
                  Nothing → [ retryOrWarning ]
                  Just profile@{ shouldFetchChatHistory, user: { availability } } →
                        if availability == Unavailable then []
                        else
                              let
                                    entries = retryOrWarning : displayChatHistory profile
                              in
                                    if shouldFetchChatHistory || not freeToFetchChatHistory then HE.div' (HA.class' "loading") : entries
                                    else entries

      -- | If we don't have a contact, either an error occurred or we are impersonating, which can't fail
      retryOrWarning = case experimenting of
            Just (Impersonation (Just { name })) →
                  HE.div (HA.class' "imp impersonation-warning-history")
                        [ HE.div_
                                [ HE.text "You are impersonating "
                                , HE.strong_ name
                                ]
                        ]
            _ → SIVR.retry "Failed to load chat history" (FetchHistory true) failedRequests

      displayChatHistory { history, user } = DA.mapWithIndex (\i → chatHistoryEntry user $ map _.sender (history !! (i - 1))) history

      chatHistoryEntry chatPartner previousSender { id, status, date, sender, content } =
            let
                  incomingMessage = sender /= loggedUserId
                  noTimestamps = not messageTimestamps || not chatPartner.messageTimestamps
                  noReadReceipts = not readReceipts || not chatPartner.readReceipts
            in
                  HE.div
                        ( HA.class'
                                { message: true
                                , "outgoing-message": sender == loggedUserId
                                , "incoming-message": incomingMessage
                                , "same-bubble-message": previousSender == Just sender -- only the first message in a row has a bubble handle
                                }
                        )
                        [ HE.div
                                [ HA.class' "message-content", HA.id $ "m" <> show id ] -- id is used to scroll into view
                                [ HE.div' [ HA.innerHtml $ SM.parse content ]
                                , HE.div
                                        ( HA.class'
                                                { duller: status /= Errored
                                                , "error-message": status == Errored
                                                , "message-status": true
                                          }
                                        )
                                        [ HE.span (HA.class' { hidden: noTimestamps }) $ SD.agoWithTime (DN.unwrap date)
                                        , HE.span (HA.class' { hidden: incomingMessage || noTimestamps || noReadReceipts }) " - "
                                        , HE.span (HA.class' { hidden: incomingMessage || noReadReceipts }) $ show status
                                        ]
                                ]
                        ]
