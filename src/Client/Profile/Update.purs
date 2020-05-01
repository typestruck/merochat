module Client.Profile.Update where

import Prelude
import Shared.Profile.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.Common.Notification as CCNO
import Data.Int as DI
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (Key)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Record as R
import Shared.Newtype as SN
import Shared.Router as SR
import Shared.Types (Ok(..), Route(..))
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.HTML.HTMLElement as WHH

getFileInput :: Effect Element
getFileInput = CCD.querySelector "#avatar-file-input"

update :: AffUpdate ProfileModel ProfileMessage
update { model, message } =
        case message of
                SelectAvatar -> selectAvatar
                SetAvatar base64 -> setProfileField (SProxy :: SProxy "avatar") base64
                SetName name -> setProfileField (SProxy :: SProxy "name") name
                SetHeadline headline -> setProfileField (SProxy :: SProxy "headline") headline
                SetCountry country -> setHideProfileField (SProxy :: SProxy "isCountryVisible") (SProxy :: SProxy "country") $ DI.fromString (spy "country is" country)
                NameEnter (Tuple key _) -> blurOnEnter key "#profile-edition-name"
                HeadlineEnter (Tuple key _) -> blurOnEnter key "#profile-edition-headline"
                ToggleCountry visible -> setModelField (SProxy :: SProxy "isCountryVisible") visible
                ToggleGender visible -> setModelField (SProxy :: SProxy "isGenderVisible") visible
                ToggleLanguages visible -> setModelField (SProxy :: SProxy "isLanguagesVisible") visible
                ToggleAge visible -> setModelField (SProxy :: SProxy "isAgeVisible") visible
                SaveProfile -> saveProfile model

blurOnEnter :: Key -> String -> Aff (ProfileModel -> ProfileModel)
blurOnEnter key id = do
        when (key == "Enter") $ liftEffect do
                element <- CCD.querySelector id
                WHH.blur <<< SU.unsafeFromJust "blurOnEnter" $ WHH.fromElement element
        FAE.noChanges

selectAvatar :: Aff (ProfileModel -> ProfileModel)
selectAvatar = do
        liftEffect do
                input <- getFileInput
                WHH.click <<< SU.unsafeFromJust "selectAvatar" $ WHH.fromElement input
        FAE.noChanges

setModelField field value = pure $ \model -> SN.updateProfileModel model (R.set field value)

setProfileField field value =
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _ {
                user = SN.updateProfile user (R.set field value)
        }

setHideProfileField visibilityField field value =
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ \record ->
                R.set visibilityField true $ record {
                        user = SN.updateProfile user (R.set field value)
                }

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile (ProfileModel { user }) = do
        Ok <- CCN.post' (SR.fromRouteAbsolute Profile) user
        liftEffect $ CCNO.alert "Profile updated"
        FAE.noChanges

