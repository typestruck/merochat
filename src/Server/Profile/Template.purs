module Server.Profile.Template where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame as F
import Record as R
import Shared.Ask (Ask)
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Html (Html(..))
import Shared.Profile.Mode (ProfileMode)
import Shared.Profile.Types (ProfileAsk)
import Shared.Profile.View as SPV

template ∷ _ → ProfileMode → Effect Html
template payload mode = do
      Html <$> F.preMount (SE.toQuerySelector ProfileEditionForm)
            { view: SPV.view
            , model:
                    { nameInputed: Just payload.user.name
                    , headlineInputed: Just payload.user.headline
                    , ageInputed: payload.user.age
                    , genderInputed: payload.user.gender
                    , mode
                    , fromTemporary: false
                    , countryInputed: payload.user.country
                    , posts: payload.posts
                    , asks: extend <$> payload.asks
                    , praise: payload.praise
                    , languagesInputed: payload.user.languages
                    , visible: true
                    , contextMenuFor: Nothing
                    , generated: []
                    , avatarInputed: payload.user.avatar
                    , tagsInputed: payload.user.tags
                    , descriptionInputed: Just payload.user.description
                    , loading: false
                    , registrationMessage: false
                    , updateRequestStatus: Nothing
                    , user: payload.user
                    , countries: payload.countries
                    , languages: payload.languages
                    }
            }
      where
      extend ask = (R.merge (ask ∷ Ask) { typedAnswer: Nothing ∷ Maybe String }) ∷ ProfileAsk