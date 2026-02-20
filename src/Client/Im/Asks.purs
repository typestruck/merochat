module Client.Im.Asks where

import Prelude

import Client.Im.Flame (MoreMessages, NextMessage, NoMessages)
import Client.Network (routes)
import Client.Network as CCN
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Shared.Ask (Ask)
import Shared.Im.Types (For(..), ImMessage(..), ImModel, RetryableRequest(..))
import Shared.Modal (Modal(..), SpecialModal(..))
import Shared.Unsafe as SU
import Shared.User (ProfileTab(..))

displayAsks ∷ Int → Array Ask → ImModel → NoMessages
displayAsks userId asks model =
      model
            { asks = model.asks { freeToFetch = true }
            , suggestions = map updateSuggestion model.suggestions
            , contacts = map updateContact model.contacts
            } /\ []
      where
      updateSuggestion suggestion
            | suggestion.id == userId = suggestion { asks = suggestion.asks <> asks }
            | otherwise = suggestion
      updateContact contact
            | contact.user.id == userId = contact { user = contact.user { asks = contact.user.asks <> asks } }
            | otherwise = contact

fetchAsks ∷ Int → ImModel → MoreMessages
fetchAsks userId model = model { asks = model.asks { freeToFetch = false } } /\ [ fetch ]
      where
      fetch = CCN.retryableRequest (FetchAsks userId) (DisplayAsks userId) $ routes.asks.get { query: { answerer: userId } }

setAsk ∷ Maybe String → ImModel → MoreMessages
setAsk value model = model { asks = model.asks { question = value } } /\ []

sendAsk ∷ Int → ImModel → MoreMessages
sendAsk userId model = model { asks = model.asks { freeToSend = false } } /\ [ send ]
      where
      send = do
            response ← CCN.silentRequest $ routes.asks.post { body: { userId, question: SU.fromJust model.asks.question } }
            pure <<< Just $ AfterSendAsk userId response.allowed

afterSendAsk ∷ Int → Boolean → ImModel → MoreMessages
afterSendAsk userId allowed model =
      model
            { asks = model.asks
                    { freeToSend = true
                    , question = Nothing
                    , sent = updatedSent
                    , unallowed = updatedUnallowed
                    }
            } /\ []
      where
      updatedSent /\ updatedUnallowed =
            if allowed then
                  (userId : model.asks.sent) /\ model.asks.unallowed
            else
                  model.asks.sent /\ (userId : model.asks.unallowed)

toggleShowing ∷ Int → For → ImModel → MoreMessages
toggleShowing userId for model =
      case for of
            ForSuggestions → toggleShowingSuggestions userId model
            ForContacts → toggleShowingContacts userId model

toggleShowingSuggestions ∷ Int → ImModel → MoreMessages
toggleShowingSuggestions userId model =
      model
            { suggestions = map update model.suggestions
            --we need this bookkeeping for big suggestion cards
            , suggesting = Just userId
            , modal = Special $ ShowSuggestionCard userId
            , asks = model.asks { question = Nothing, freeToFetch = not shouldFetch }
            } /\ effects
      where
      found = DA.find ((_ == userId) <<< _.id) model.suggestions
      shouldFetch = Just ShowAsks /= (_.showing <$> found) && Just 0 == (DA.length <<< _.asks <$> found)

      update suggestion
            | suggestion.id == userId = suggestion { showing = ShowAsks }
            | otherwise = suggestion

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchAsks userId ]
            | otherwise = []

toggleShowingContacts ∷ Int → ImModel → MoreMessages
toggleShowingContacts userId model =
      model
            { contacts = map update model.contacts
            , asks = model.asks { question = Nothing, freeToFetch = not shouldFetch }
            , fullContactProfileVisible = true
            } /\ effects
      where
      found = _.user <$> DA.find ((_ == userId) <<< _.id <<< _.user) model.contacts
      shouldFetch = Just ShowAsks /= (_.showing <$> found) && Just 0 == (DA.length <<< _.asks <$> found)

      update contact
            | contact.user.id == userId = contact { user = contact.user { showing = ShowAsks } }
            | otherwise = contact

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchAsks userId ]
            | otherwise = []

