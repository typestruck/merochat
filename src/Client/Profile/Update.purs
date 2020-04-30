module Client.Profile.Update where

import Prelude
import Shared.Profile.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.Common.Notification as CCNO
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
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
                SetAvatar base64 -> setField (SProxy :: SProxy "avatar") base64
                SetName name -> setField (SProxy :: SProxy "name") name
                SetHeadline headline -> setField (SProxy :: SProxy "headline") headline
                NameEnter (Tuple key _) -> blurOnEnter key "#profile-edition-name"
                HeadlineEnter (Tuple key _) -> blurOnEnter key "#profile-edition-headline"
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

setField field value =
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _ {
                user = SN.updateProfile user $ \record -> R.set field value record
        }

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile (ProfileModel { user }) = do
        Ok <- CCN.post' (SR.fromRouteAbsolute Profile) user
        liftEffect $ CCNO.alert "Profile updated"
        FAE.noChanges

