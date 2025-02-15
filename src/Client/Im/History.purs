module Client.Im.History where

import Client.Im.Flame
import Debug
import Prelude
import Shared.Im.Types

import Client.Common.Dom as CCD
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Scroll as CIS
import Control.Alt ((<|>))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Flame as F
import Shared.Element (ElementId(..))
import Shared.Unsafe as SU
import Web.DOM.Element as WDE

checkFetchHistory ∷ Int → ImModel → MoreMessages
checkFetchHistory userId model =
      model /\
            if model.freeToFetchChatHistory then [ Just <<< SpecialRequest <<< (FetchHistory userId) <$> getScrollTop ] else []
      where
      getScrollTop = liftEffect do
            element ← CCD.unsafeGetElementById MessageHistory
            (_ < 1.0) <$> WDE.scrollTop element

--to avoid issues with older missed unread messages just get the whole chat history on first load
fetchHistory ∷ Int → Boolean → ImModel → MoreMessages
fetchHistory userId shouldFetch model
      | shouldFetch =
              case model.chatting <|> DA.find ((userId == _) <<< _.id <<< _.user) model.contacts of
                    Nothing → F.noMessages model
                    Just contact →
                          model
                                { freeToFetchChatHistory = false
                                } /\
                                [ CCN.retryableResponse
                                        (FetchHistory contact.user.id true)
                                        (DisplayHistory contact.user.id (spy "overwritting" contact.shouldFetchChatHistory))
                                        (request.im.history { query: { with: contact.user.id, skip: if contact.shouldFetchChatHistory then 0 else DA.length contact.history } })
                                ]
      | otherwise = F.noMessages model

displayHistory ∷ Int → Boolean → Array HistoryMessage → ImModel → NoMessages
displayHistory userId overwrite history model =
      updatedModel /\
            if shouldFetchChatHistory then
                  [ liftEffect CIS.scrollLastMessage *> pure Nothing ]
            else if not $ DA.null history then
                  [ liftEffect (CIS.scrollIntoView <<< SU.fromJust $ DA.last history) *> pure Nothing ]
            else
                  []
      where
      shouldFetchChatHistory = case model.chatting of
            Nothing → DM.maybe false (_.shouldFetchChatHistory) $ DA.find ((userId == _) <<< _.id <<< _.user) model.contacts
            Just chatting → chatting.shouldFetchChatHistory

      updateContact contact
            | contact.user.id == userId = updateHistory contact
            | otherwise = contact
      updateHistory contact = contact
            { shouldFetchChatHistory = false
            , history = if overwrite then history else (history <> contact.history) --see fetchHistory
            }

      updatedModel = case model.chatting of
            Nothing → model
                  { freeToFetchChatHistory = true
                  , contacts = map updateContact model.contacts
                  }

            Just chatting → model
                  { freeToFetchChatHistory = true
                  , chatting =  Just $ updateHistory chatting
                  }

