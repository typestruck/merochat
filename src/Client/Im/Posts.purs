module Client.Im.Posts where

import Prelude

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (MoreMessages, NoMessages)
import Control.Alt ((<|>))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Shared.Im.Types (For(..), ImMessage(..), ImModel, RetryableRequest(..))
import Shared.Modal.Types (Modal(..), SpecialModal(..))
import Shared.Post (Post)
import Shared.Unsafe as SU
import Shared.User (ProfilePost(..), ProfileVisibility(..))

displayPosts ∷ Int → Array Post → ImModel → NoMessages
displayPosts userId posts model = model { freeToFetchPosts = true, suggestions = map updateSuggestion model.suggestions, contacts = map updateContact model.contacts } /\ []
      where
      updateSuggestion suggestion
            | suggestion.id == userId = suggestion { posts = suggestion.posts <> posts }
            | otherwise = suggestion
      updateContact contact
            | contact.user.id == userId = contact { user = contact.user { posts = contact.user.posts <> posts } }
            | otherwise = contact

fetchPosts ∷ Int → ImModel → MoreMessages
fetchPosts userId model = model { freeToFetchPosts = false } /\ [ fetch ]
      where
      fetch = CCN.retryableResponse (FetchPosts userId) (DisplayPosts userId) $ request.posts.get { query: { poster: userId } }

togglePostForm ∷ ImModel → NoMessages
togglePostForm model = model { showSuggestionsPostForm = not model.showSuggestionsPostForm } /\ []

setPostContent ∷ Maybe String → ImModel → NoMessages
setPostContent content model =
      model
            { postContent = if content == Just "" then Nothing else content
            } /\ []

afterSendPost ∷ ImModel → NoMessages
afterSendPost model =
      model
            { user = model.user { totalPosts = model.user.totalPosts + 1 }
            , freeToPost = true
            , postContent = Nothing
            , modal = HideModal
            } /\ []

sendPost ∷ ImModel → MoreMessages
sendPost model = model { freeToPost = false } /\ [ send ]
      where
      send = do
            void <<< CCN.silentResponse $ request.posts.post { body: { content: SU.fromJust model.postContent } }
            pure $ Just AfterSendPost

toggleShowing ∷ Int → ProfilePost → For → ImModel → MoreMessages
toggleShowing userId toggle for model =
      case for of
            ForSuggestions → toggleShowingSuggestions userId toggle model
            ForContacts → toggleShowingContacts userId toggle model

toggleShowingSuggestions ∷ Int → ProfilePost → ImModel → MoreMessages
toggleShowingSuggestions userId toggle model =
      model
            { suggestions = map update model.suggestions
            --we need this bookkeeping for big suggestion cards
            , suggesting = Just userId
            , modal = Special $ ShowSuggestionCard userId
            , freeToFetchPosts = not shouldFetch
            } /\ effects
      where
      found = DA.find ((_ == userId) <<< _.id) model.suggestions
      shouldFetch = toggle == ShowPosts && Just ShowInfo == (_.showing <$> found) && Just 0 == (DA.length <<< _.posts <$> found)

      update suggestion
            | suggestion.id == userId = suggestion { showing = toggle }
            | otherwise = suggestion

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchPosts userId ]
            | otherwise = []

toggleShowingContacts ∷ Int → ProfilePost → ImModel → MoreMessages
toggleShowingContacts userId toggle model =
      model
            { contacts = map update model.contacts
            , freeToFetchPosts = not shouldFetch
            , fullContactProfileVisible = true
            } /\ effects
      where
      found = _.user <$> DA.find ((_ == userId) <<< _.id <<< _.user) model.contacts
      shouldFetch = toggle == ShowPosts && Just ShowInfo == (_.showing <$> found) && Just 0 == (DA.length <<< _.posts <$> found)

      update contact
            | contact.user.id == userId = contact { user = contact.user { showing = toggle } }
            | otherwise = contact

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchPosts userId ]
            | otherwise = []
