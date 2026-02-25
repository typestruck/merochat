module Client.Im.Praise where

import Prelude

import Client.Im.Flame (NoMessages, MoreMessages)
import Client.Network (routes)
import Client.Network as CCN
import Data.Array ((:))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple as DT
import Data.Tuple.Nested ((/\))
import Shared.Im.Types (For(..), ImMessage(..), ImModel, RetryableRequest(..))
import Shared.Modal (Modal(..), SpecialModal(..))
import Shared.Praise (Praise, PraisedFor(..), PraiseDisplay)
import Shared.Unsafe as SU
import Shared.User (PraiseStatus(..), ProfileTab(..))

fetchPraise ∷ Int → ImModel → MoreMessages
fetchPraise userId model = model { praise = model.praise { freeToFetch = false } } /\ [ fetch ]
      where
      fetch = CCN.retryableRequest (FetchPraise userId) (DisplayPraise userId) $ routes.praise.get { query: { praised: userId } }

displayPraise ∷ Int → PraiseDisplay → ImModel → NoMessages
displayPraise userId praise model =
      model
            { praise = model.praise { freeToFetch = true }
            , suggestions = map updateSuggestion model.suggestions
            , contacts = map updateContact model.contacts
            } /\ []
      where
      status = if praise.alreadyPraised then HasPraised else HasNotPraised

      updateSuggestion suggestion
            | suggestion.id == userId = suggestion { praiseStatus = status, praise = suggestion.praise <> map _.content praise.praise }
            | otherwise = suggestion
      updateContact contact
            | contact.user.id == userId = contact { user = contact.user { praiseStatus = status, praise = contact.user.praise <> map _.content praise.praise } }
            | otherwise = contact

togglePraise ∷ Int → PraisedFor → ImModel → NoMessages
togglePraise userId for model = model { praise = model.praise { selected = Just (userId /\ toggle model.praise.selected) } } /\ []
      where
      toggle = case _ of
            Nothing → [ for ]
            Just (_ /\ selected) → if DA.elem for selected then DA.filter (_ /= for) selected else for : selected

setOtherPraise ∷ Maybe String → ImModel → NoMessages
setOtherPraise praise model = model { praise = model.praise { other = praise } } /\ []

savePraise ∷ ImModel → NoMessages
savePraise model = model { praise = model.praise { freeToSave = false } } /\ [ save ]
      where
      userId /\ for = SU.fromJust model.praise.selected
      save = do
            response ← CCN.silentRequest $ routes.praise.post { body: { for: DA.filter (_ /= Other "") for, userId } }
            pure <<< Just $ AfterSavePraise userId response.allowed

afterSavePraise ∷ Int → Boolean → ImModel → NoMessages
afterSavePraise userId allowed model =
      model
            { contacts = map updateContact model.contacts
            , suggestions = map updateSuggestion model.suggestions
            , praise = model.praise { freeToSave = true, selected = Nothing, other = Nothing }
            } /\ []
      where
      status = if allowed then HasPraised else PraiseNotAccepted
      praise = DT.snd $ SU.fromJust model.praise.selected

      updateSuggestion suggestion
            | suggestion.id == userId = suggestion { praiseStatus = status, praise = suggestion.praise <> praise }
            | otherwise = suggestion
      updateContact contact
            | contact.user.id == userId = contact { user = contact.user { praiseStatus = status, praise = contact.user.praise <> praise } }
            | otherwise = contact

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
            , praise = model.praise { selected = Nothing, other = Nothing, freeToSave = true }
            } /\ effects
      where
      found = DA.find ((_ == userId) <<< _.id) model.suggestions
      shouldFetch = Just ShowPraise /= (_.showing <$> found) && Just 0 == (DA.length <<< _.praise <$> found)

      update suggestion
            | suggestion.id == userId = suggestion { showing = ShowPraise }
            | otherwise = suggestion

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchPraise userId ]
            | otherwise = []

toggleShowingContacts ∷ Int → ImModel → MoreMessages
toggleShowingContacts userId model =
      model
            { contacts = map update model.contacts
            , praise = model.praise { other = Nothing, selected = Nothing, freeToFetch = not shouldFetch }
            , fullContactProfileVisible = true
            } /\ effects
      where
      found = _.user <$> DA.find ((_ == userId) <<< _.id <<< _.user) model.contacts
      shouldFetch = Just ShowPraise /= (_.showing <$> found) && Just 0 == (DA.length <<< _.praise <$> found)

      update contact
            | contact.user.id == userId = contact { user = contact.user { showing = ShowPraise } }
            | otherwise = contact

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchPraise userId ]
            | otherwise = []
