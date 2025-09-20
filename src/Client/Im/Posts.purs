module Client.Im.Posts where

import Prelude

import Client.Im.Flame (NoMessages)
import Data.Tuple.Nested ((/\))
import Shared.Im.Types (ImModel)
import Shared.Post (Post)

displayPosts ∷ Int → Array Post → ImModel → NoMessages
displayPosts userId posts model = model { freeToFetchPosts = true, suggestions = map updateSuggestion model.suggestions, contacts = map updateContact model.contacts } /\ []
      where
      updateSuggestion suggestion
            | suggestion.id == userId = suggestion { posts = suggestion.posts <> posts }
            | otherwise = suggestion
      updateContact contact
            | contact.user.id == userId = contact { user = contact.user { posts = contact.user.posts <> posts } }
            | otherwise = contact