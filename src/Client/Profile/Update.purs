module Client.Profile.Update where

import Prelude
import Shared.Profile.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
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
                SetAvatar base64 -> setAvatar base64
                SaveProfile -> saveProfile model

selectAvatar :: Aff (ProfileModel -> ProfileModel)
selectAvatar = do
        liftEffect do
                input <- getFileInput
                WHH.click <<< SU.unsafeFromJust "selectAvatar" $ WHH.fromElement input
        FAE.noChanges

setAvatar :: String -> Aff (ProfileModel -> ProfileModel)
setAvatar base64 =
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _ {
                user = SN.updateProfile user $ _ {
                        avatar = base64
                }
        }

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile (ProfileModel { user }) = do
        Ok <- CCN.post' (SR.fromRouteAbsolute Profile) user
        FAE.noChanges

