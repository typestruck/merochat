module Server.Profile.Template where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug (spy)
import Effect (Effect)
import Flame as F
import Record as R
import Shared.Ask (Ask, A)
import Shared.Element (ElementId(..))
import Shared.Element as SE
import Shared.Html (Html(..))
import Shared.Profile.Types (ProfileMode(..), ProfileAsk)
import Shared.Profile.View as SPV

template ∷ _ → Effect Html
template payload = do
      Html <$> F.preMount (SE.toQuerySelector ProfileEditionForm)
            { view: SPV.view
            , model:
                    { nameInputed: Just payload.user.name
                    , headlineInputed: Just payload.user.headline
                    , ageInputed: payload.user.age
                    , genderInputed: payload.user.gender
                    , mode: if DA.any (DM.isNothing <<< _.answer) payload.asks then Asked else Edit
                    , fromTemporary: false
                    , countryInputed: payload.user.country
                    , posts: payload.posts
                    , asks: extend <$> payload.asks
                    , languagesInputed: payload.user.languages
                    , visible: true
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
      extend ask = (R.merge (ask :: Ask) { typedAnswer : Nothing :: Maybe String}) :: ProfileAsk