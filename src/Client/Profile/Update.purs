module Client.Profile.Update where

import Prelude
import Shared.Types

import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Notification as CCNO
import Data.Array as DA
import Data.Date as DD
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.String.Read as DSR
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Record as R
import Shared.Setter as SS
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event as WEE

getFileInput :: Effect Element
getFileInput = CCD.querySelector "#avatar-file-input"

update :: AffUpdate ProfileModel ProfileMessage
update { model, message } =
      case message of
            SelectAvatar -> selectAvatar
            SetPField setter -> pure setter
            SetAvatar base64 -> pure <<< SS.setUserField (SProxy :: SProxy "avatar") $ Just base64
            SetName -> setOrGenerateField Name (SProxy :: SProxy "nameInputed") (SProxy :: SProxy "name") 50 model.nameInputed
            SetHeadline  -> setOrGenerateField Headline (SProxy :: SProxy "headlineInputed") (SProxy :: SProxy "headline") 200 model.headlineInputed
            SetDescription -> setOrGenerateField Description (SProxy :: SProxy "descriptionInputed") (SProxy :: SProxy "description") 10000 model.descriptionInputed
            SaveProfile -> saveProfile model

setOrGenerateField what field userField characters value = do
      let trimmed = DS.trim $ DM.fromMaybe "" value
      toSet <- if DS.null trimmed then do
                  CCN.response $ request.profile.generate { query: { what } }
                else
                  pure trimmed
      pure (R.set field Nothing <<< SS.setUserField userField (DS.take characters toSet))

selectAvatar :: Aff (ProfileModel -> ProfileModel)
selectAvatar = do
      liftEffect do
            input <- getFileInput
            CCF.triggerFileSelect input
      FAE.noChanges

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile { user: user@{ name }} = do
      void $ request.profile.post { body: user }
      liftEffect do
            CCNO.alert "Profile updated"
            --let im know that the name has changed
            CCD.dispatchCustomEvent $ CCD.createCustomEvent nameChanged name
      FAE.noChanges
