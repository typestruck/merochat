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
import Data.Maybe as DM
import Data.Newtype as DN
import Effect (Effect)
import Effect.Uncurried (EffectFn5)
import Effect.Uncurried as EU
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Safe.Coerce as SC
import Shared.DateTime as SD
import Shared.Element (ElementId(..))
import Shared.Im.Contact as SIC
import Shared.Im.Scroll as SIS
import Shared.Im.View.ChatInput as SICI
import Shared.Im.View.Profile as SIVP
import Shared.Im.View.Retry as SIVR
import Shared.Markdown as SM
import Shared.Options.Reaction (maxReactionCharacters)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU
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
                                    if chatting.shouldFetchChatHistory || not model.freeToFetchChatHistory then HE.div' [ HA.class' "loading" ] : entries
                                    else entries
            Nothing → HE.div'
                  [ HA.id $ show MessageHistory
                  , HA.class' "message-history hidden"
                  ]
      where
      retryOrWarning id = SIVR.retry "Failed to load chat history" (FetchHistory id true) model.failedRequests
      temporaryChatWarning = if model.user.temporary then [ SIVP.signUpCall model.user.joined ] else []

      displayChatHistory contact = chatHistoryEntry contact <$> contact.history
      isBottomMessage history id = (SU.fromJust $ DA.findIndex ((_ == id) <<< _.id) history) >= DA.length history - 2
      chatHistoryEntry contact entry
            | entry.status == Errored =
                    --currently the only way a message could fail is if it was unsanitary, otherwise we wouldnt hear from the server
                    -- when this is better we can add a resend logic
                    HE.div
                          [ HA.class' "message outgoing-message" ]
                          [ HE.div
                                  [ HA.class' "message-content", HA.id $ "m" <> show entry.id ]
                                  [ HE.div [ HA.class' "message-content-in" ]
                                          [ HE.div' [ HA.innerHtml $ SM.parse entry.content ]
                                          ]
                                  , HE.div [ HA.class' "message-status error-message" ]
                                          [ HE.span [ HA.class' { hidden: not model.user.messageTimestamps || not contact.user.messageTimestamps } ] [ HE.text <<< SD.agoWithTime $ SC.coerce entry.date ]
                                          , HE.span_ [ HE.text " - Failed to send" ]
                                          ]
                                  ]
                          ]
            | otherwise =
                    let
                          incomingMessage = entry.recipient == model.user.id
                          noTimestamps = not model.user.messageTimestamps || not contact.user.messageTimestamps
                          noReadReceipts = not model.user.readReceipts || not contact.user.readReceipts
                          reactionTextId = "reaction-text-" <> show entry.id
                          isContextMenuVisible = model.toggleContextMenu == ShowMessageContextMenu entry.id
                          mobileEvents
                                | model.smallScreen = [ CIT.onTouchStart Nothing, CIT.onTouchEnd (QuoteMessage entry.content <<< Left), HA.onClick <<< SetContextMenuToggle $ ShowMessageContextMenu entry.id ]
                                | otherwise = []
                    in
                          HE.div
                                [ HA.class'
                                        { message: true
                                        , "outgoing-message": entry.sender == model.user.id
                                        , "incoming-message": incomingMessage
                                        }
                                , HA.onDblclick' (QuoteMessage entry.content <<< Right <<< Just)
                                ]
                                ( [ HE.div
                                          ([ HA.class' { "message-content": true, "editing-message": Just entry.id == model.editing }, HA.id $ "m" <> show entry.id ] <> mobileEvents) -- id is used to scroll into view
                                          [ HE.div [ HA.class' "message-content-in" ]
                                                  [ HE.div [ HA.class' { "user-menu in-message reactions-menu": true, visible: isContextMenuVisible, "menu-up": isContextMenuVisible && isBottomMessage contact.history entry.id } ]
                                                          [ if model.react == WithEmoji then
                                                                  HE.div [ HA.class' "reaction-emojis" ]
                                                                        [ HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "👍") ] [ HE.text "👍" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "❤️") ] [ HE.text "❤️" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "😂") ] [ HE.text "😂" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "😮") ] [ HE.text "😮" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "👾") ] [ HE.text "👾" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "😡") ] [ HE.text "😡" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "👀") ] [ HE.text "👀" ]
                                                                        , HE.div [ HA.onClick' (React contact.user.id entry.id $ Right "🤔") ] [ HE.text "🤔" ]
                                                                        , HE.div [ HA.onClick' (SetReactWithText reactionTextId), HA.class' "reaction-add-text", HA.placeholder "React with text" ] [ HE.text "+" ]
                                                                        ]
                                                            else
                                                                  HE.div [ HA.class' "reaction-text" ]
                                                                        [ HE.input [ HA.id reactionTextId, HA.placeholder $ "Max " <> show maxReactionCharacters <> " characters", HA.maxlength maxReactionCharacters, HA.type' "text", HA.class' "reaction-input" ]
                                                                        , HE.svg [ HA.class' "send-button", HA.onClick' (React contact.user.id entry.id $ Left reactionTextId), HA.viewBox "0 0 16 16" ] $ SICI.sendButtonElements "React with text"
                                                                        ]

                                                          ]
                                                  , HE.div' [ HA.innerHtml $ SM.parse entry.content ]
                                                  , HE.div [ HA.class' "message-context-options" ]
                                                          [ HE.div [ HA.class' { "message-context-menu outer-user-menu": true, visible: isContextMenuVisible }, HA.onClick <<< SetContextMenuToggle $ ShowMessageContextMenu entry.id ]
                                                                  [ HE.svg [ HA.class' "svg-32 svg-duller", HA.viewBox "0 0 16 16" ]
                                                                          [ HE.polygon' [ HA.transform "rotate(90,7.6,8)", HA.points "11.02 7.99 6.53 3.5 5.61 4.42 9.17 7.99 5.58 11.58 6.5 12.5 10.09 8.91 10.1 8.91 11.02 7.99" ]
                                                                          ]
                                                                  , HE.div [ HA.class' { "user-menu in-message": true, visible: isContextMenuVisible, "menu-up": isContextMenuVisible && isBottomMessage contact.history entry.id } ]
                                                                          [ HE.div [ HA.class' "user-menu-item menu-item-heading", HA.onClick (QuoteMessage entry.content (Right Nothing)) ] [ HE.text "Reply" ]
                                                                          , HE.div [ HA.class' { "user-menu-item menu-item-heading": true, "hidden": incomingMessage || entry.status < Received }, HA.onClick $ EditMessage entry.content entry.id ] [ HE.text "Edit" ]
                                                                          , HE.div [ HA.class' { "user-menu-item menu-item-heading": true, "hidden": incomingMessage || entry.status < Received }, HA.onClick $ DeleteMessage entry.id ] [ HE.text "Unsend" ]
                                                                          ]
                                                                  ]
                                                          ]
                                                  ]
                                          , HE.div [ HA.class' "message-status" ]
                                                  [ HE.span [ HA.class' { hidden: not entry.edited } ] [ HE.text "Edited - " ]
                                                  , HE.span [ HA.class' { hidden: noTimestamps } ] [ HE.text <<< SD.agoWithTime $ SC.coerce entry.date ]
                                                  , HE.span [ HA.class' { hidden: incomingMessage || noReadReceipts } ] [ HE.text $ " - " <> show entry.status ]
                                                  ]

                                          ]

                                  ] <> case entry.reaction of
                                        Nothing → []
                                        Just r →
                                              [ HE.div [ HA.class' "reactions" ]
                                                      [ HE.text r
                                                      ]
                                              ]
                                )

