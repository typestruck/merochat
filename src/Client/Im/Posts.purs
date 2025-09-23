module Client.Im.Posts where

import Prelude

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Im.Flame (NoMessages, MoreMessages)
import Control.Alt ((<|>))
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Shared.Im.Types (ImMessage(..), ImModel, RetryableRequest(..), ShowPostForm)
import Shared.Post (Post)
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))

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
fetchPosts userId model = model { freeToFetchPosts = not arePostsVisible } /\ effects
      where
      found = SU.fromJust (DA.find ((_ == userId) <<< _.id) model.suggestions <|> (_.user <$> DA.find ((_ == userId) <<< _.id <<< _.user) model.contacts))
      arePostsVisible = found.postsVisibility == Everyone || (found.postsVisibility == NoTemporaryUsers && not model.user.temporary) || (found.postsVisibility == Contacts && found.isContact)

      effects
            | arePostsVisible = [ CCN.retryableResponse (FetchPosts userId) (DisplayPosts userId) $ request.posts.get { query: { poster: userId } } ]
            | otherwise = []

togglePostForm ∷ ShowPostForm → ImModel → NoMessages
togglePostForm toggle model = model { showPostForm = toggle } /\ []

setPostContent ∷ Maybe String → ImModel → NoMessages
setPostContent content model = model { postContent = content, freeToPost = model.freeToPost || DM.isNothing content } /\ []

sendPost ∷ ImModel → MoreMessages
sendPost model = model { freeToPost = false, user = model.user { totalPosts = model.user.totalPosts + 1 } } /\ [ send ]
      where
      send = do
            void <<< CCN.silentResponse $ request.posts.post { body: { content: SU.fromJust model.postContent } }
            pure <<< Just $ SetPostContent Nothing