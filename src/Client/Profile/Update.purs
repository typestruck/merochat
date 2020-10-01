module Client.Profile.Update where

import Prelude
import Shared.Types

import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.File as CCF
import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Notification as CCNO
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Record as R
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters)
import Shared.Setter as SS
import Type.Data.Symbol as TDS
import Web.DOM (Element)

getFileInput :: Effect Element
getFileInput = CCD.unsafeQuerySelector "#avatar-file-input"

update :: AffUpdate ProfileModel ProfileMessage
update { model, message } =
      case message of
            SelectAvatar -> selectAvatar
            SetPField setter -> pure setter
            SetAvatar base64 -> pure <<< SS.setUserField (SProxy :: SProxy "avatar") $ Just base64
            SetName -> setGenerated model Name (SProxy :: SProxy "name") nameMaxCharacters
            SetHeadline  -> setGenerated model Headline (SProxy :: SProxy "headline") headlineMaxCharacters
            SetDescription -> setGenerated model Description (SProxy :: SProxy "description") descriptionMaxCharacters
            SaveProfile -> saveProfile model

setGenerated model what field characters = do
      let   fieldInputed = TDS.append field (SProxy :: SProxy "Inputed")
            trimmed = DS.trim <<< DM.fromMaybe "" $ R.get fieldInputed model

      toSet <- if DS.null trimmed then do
                  CCN.response $ request.profile.generate { query: { what } }
            else
                  pure trimmed
      pure (R.set fieldInputed Nothing <<< SS.setUserField field (DS.take characters toSet))

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
