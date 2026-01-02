module Client.Im.Posts where

import Prelude

import Client.AppId (imAppId)
import Client.File as CCF
import Client.Network (routes)
import Client.Network as CCN
import Client.Im.Flame (MoreMessages, NoMessages, NextMessage)
import Client.Im.WebSocket as CIW
import Control.Alt ((<|>))
import Data.Array as DA
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol as TDS
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Class as EC
import Shared.Content (Content(..))
import Shared.Im.Types (For(..), ImMessage(..), ImModel, PostMode(..), RetryableRequest(..), SelectedImage, WebSocketPayloadServer(..))
import Shared.Modal (Modal(..), SpecialModal(..))
import Shared.Post (Post)
import Shared.Resource (maxImageSize)
import Shared.Unsafe as SU
import Shared.User (ProfileTab(..))
import Type.Proxy (Proxy(..))
import Web.Event.Event as WEE
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement as WDE
import Web.HTML.HTMLInputElement as WHI
import Web.Socket.WebSocket (WebSocket)

displayPosts ∷ Int → Array Post → ImModel → NextMessage
displayPosts userId posts model =
      model
            { posts = model.posts { freeToFetch = true }
            , suggestions = map updateSuggestion model.suggestions
            , contacts = map updateContact model.contacts
            } /\ effects
      where
      updateSuggestion suggestion
            | suggestion.id == userId = suggestion { posts = suggestion.posts <> posts, unseenPosts = 0 }
            | otherwise = suggestion
      updateContact contact
            | contact.user.id == userId = contact { user = contact.user { posts = contact.user.posts <> posts, unseenPosts = 0 } }
            | otherwise = contact

      effects
            | DA.null posts = []
            | otherwise =
                    [ do
                            void <<< CCN.silentRequest $ routes.posts.seen { body: { id: (SU.fromJust $ DA.head posts).id, poster: userId } }
                            pure Nothing
                    ]

fetchPosts ∷ Int → ImModel → MoreMessages
fetchPosts userId model = model { posts = model.posts { freeToFetch = false } } /\ [ fetch ]
      where
      fetch = CCN.retryableRequest (FetchPosts userId) (DisplayPosts userId) $ routes.posts.get { query: { poster: userId } }

togglePostForm ∷ ImModel → NoMessages
togglePostForm model = model { showSuggestionsPostForm = not model.showSuggestionsPostForm } /\ []

setPostText ∷ Maybe String → ImModel → NoMessages
setPostText content model =
      model
            { posts = model.posts { text = content }
            } /\ []

setPostLink ∷ Maybe String → ImModel → NoMessages
setPostLink content model =
      model
            { posts = model.posts { link = content }
            } /\ []

setPostCaption ∷ Maybe String → ImModel → NoMessages
setPostCaption content model =
      model
            { posts = model.posts { caption = content }
            } /\ []

afterSendPost ∷ Int → WebSocket → ImModel → NoMessages
afterSendPost id webSocket model =
      model
            { user = model.user { totalPosts = model.user.totalPosts + 1 }
            , posts = model.posts
                    { freeToSend = true
                    , mode = TextOnly
                    , link = Nothing
                    , caption = Nothing
                    , text = Nothing
                    }
            , modal = HideModal
            } /\ [ alert ]
      where
      alert = EC.liftEffect do
            CIW.sendPayload webSocket $ Posted { id }
            pure Nothing

sendPost ∷ ImModel → MoreMessages
sendPost model = model { posts = model.posts { freeToSend = not maySend } } /\ effects
      where
      maySend = model.posts.mode == TextOnly && DM.isJust model.posts.text || model.posts.mode == LinkOnly && DM.isJust model.posts.link || model.posts.mode == ImageOnly && DM.isJust model.posts.image
      send = do
            response ← CCN.silentRequest $ routes.posts.post
                  { body:
                          { content: case model.posts.mode of
                                  LinkOnly → Link (SU.fromJust model.posts.link) model.posts.caption
                                  ImageOnly → let s = SU.fromJust model.posts.image in Image (DM.fromMaybe "" model.posts.caption) s.width s.height s.base64
                                  _ → Text $ SU.fromJust model.posts.text
                          }
                  }
            pure <<< Just $ AfterSendPost response.id
      effects
            | maySend = [ send ]
            | otherwise = []

toggleShowing ∷ Int → ProfileTab → For → ImModel → MoreMessages
toggleShowing userId toggle for model =
      case for of
            ForSuggestions → toggleShowingSuggestions userId toggle model
            ForContacts → toggleShowingContacts userId toggle model

toggleShowingSuggestions ∷ Int → ProfileTab → ImModel → MoreMessages
toggleShowingSuggestions userId toggle model =
      model
            { suggestions = map update model.suggestions
            --we need this bookkeeping for big suggestion cards
            , suggesting = Just userId
            , modal = Special $ ShowSuggestionCard userId
            , posts = model.posts { freeToFetch = not shouldFetch }
            } /\ effects
      where
      found = DA.find ((_ == userId) <<< _.id) model.suggestions
      shouldFetch = toggle == ShowPosts && Just ShowPosts /= (_.showing <$> found)

      update suggestion
            | suggestion.id == userId = suggestion { showing = toggle }
            | otherwise = suggestion

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchPosts userId ]
            | otherwise = []

toggleShowingContacts ∷ Int → ProfileTab → ImModel → MoreMessages
toggleShowingContacts userId toggle model =
      model
            { contacts = map update model.contacts
            , posts = model.posts { freeToFetch = not shouldFetch }
            , fullContactProfileVisible = true
            } /\ effects
      where
      found = _.user <$> DA.find ((_ == userId) <<< _.id <<< _.user) model.contacts
      shouldFetch = toggle == ShowPosts && Just ShowPosts /= (_.showing <$> found)

      update contact
            | contact.user.id == userId = contact { user = contact.user { showing = toggle } }
            | otherwise = contact

      effects
            | shouldFetch = [ pure <<< Just <<< SpecialRequest $ FetchPosts userId ]
            | otherwise = []

setPostMode ∷ PostMode → ImModel → NoMessages
setPostMode mode model = model { posts = model.posts { mode = mode } } /\ []

preparePostImage ∷ Event → ImModel → MoreMessages
preparePostImage event model = model /\ [ before ]
      where
      before = do
            CCF.compressImage imAppId event false $ \width height base64 → SetPostImage $ Just { width, height, base64 }
            pure Nothing

setPostImage ∷ SelectedImage → ImModel → NoMessages
setPostImage selected model =
      if isTooLarge $ DM.maybe "" _.base64 selected then
            model
                  { erroredFields = [ TDS.reflectSymbol (Proxy ∷ Proxy "posts") ]
                  } /\ []
      else
            model
                  { posts = model.posts { image = selected }
                  } /\ []
      where
      isTooLarge contents = maxImageSize < CCF.fileSize contents