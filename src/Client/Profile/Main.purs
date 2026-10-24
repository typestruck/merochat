module Client.Profile.Main where

import Prelude

import Shared.Theme as ST

import Client.AppId (profileAppId)
import Client.Lynx.Room as CLR
import Client.Network (routes)
import Client.Network as CN
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Exception as EE
import Flame (Application)
import Flame as F
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Record as R
import Shared.Profile.Mode (ProfileMode(..))
import Shared.Profile.Types (ProfileMessage(..), ProfileModel)
import Shared.Profile.View as SPV
import Shared.User (User)
import Shared.Post (Post)
import Shared.Ask (Ask)
import Shared.Praise (Praise)
import Data.Maybe as DM

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount profileAppId

application ∷ Aff (Application ProfileModel ProfileMessage)
application = do
      user ← (CLR.select ∷ Aff (Maybe User)) >>= case _ of
            Nothing → EA.throwError $ EE.error "Merochat cache is invalid. Please log in again"
            Just user → pure user
      payload ← CN.silentRequest $ routes.profile.initialize {}
      CLR.insert payload.posts
      CLR.insert payload.asks
      CLR.insert payload.praise
      posts ← DM.fromMaybe [] <$> (CLR.select ∷ Aff (Maybe (Array Post)))
      asks ← DM.fromMaybe [] <$> (CLR.select ∷ Aff (Maybe (Array Ask)))
      praise ← DM.fromMaybe [] <$> (CLR.select ∷ Aff (Maybe (Array Praise)))
      let
            model =
                  { nameInputed: Just user.name
                  , headlineInputed: Just user.headline
                  , ageInputed: user.age
                  , genderInputed: user.gender
                  , mode: Edit
                  , fromTemporary: false
                  , countryInputed: user.country
                  , posts: posts
                  , asks: map (R.merge { typedAnswer: Nothing ∷ Maybe String }) asks
                  , praise: praise
                  , languagesInputed: user.languages
                  , visible: true
                  , choice: Nothing
                  , contextMenuFor: Nothing
                  , generated: []
                  , avatarInputed: user.avatar
                  , tagsInputed: user.tags
                  , descriptionInputed: Just user.description
                  , loading: false
                  , registrationMessage: false
                  , updateRequestStatus: Nothing
                  , user
                  }
      pure
            { model
            , view: \current → HE.scrollView [ HA.id "profile", ST.onLayoutChange "profile", HA.class' "profile-route", HA.scrollOrientation "vertical" ]
                    [ SPV.view current
                    ]
            , update: CPU.update
            , subscribe: []
            }
