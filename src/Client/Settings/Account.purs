module Client.Settings.Account where

import Prelude

import Client.Common.Logout as CCL
import Client.Common.Network as CCNT
import Client.Common.Notification as CCN
import Data.String as DS
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Record as R
import Shared.Newtype as SN
import Shared.Router as SRO
import Shared.Settings.Types (SettingsMessage(..), SettingsModel(..))
import Shared.Types (Ok(..), Route(..))

update :: AffUpdate SettingsModel SettingsMessage
update { model, message } =
        case message of
                SetEmail email -> setField (SProxy :: SProxy "email") email
                SetEmailConfirmation emailConfirmation -> setField (SProxy :: SProxy "emailConfirmation") emailConfirmation
                SetPassword password -> setField (SProxy :: SProxy "password") password
                SetPasswordConfirmation passwordConfirmation -> setField (SProxy :: SProxy "passwordConfirmation") passwordConfirmation
                ChangeEmail -> changeEmail model
                ChangePassword -> changePassword model
                TerminateAccount -> FAE.noChanges

changeEmail :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
changeEmail model@(SettingsModel { email, emailConfirmation }) = do
        if DS.null email || DS.null emailConfirmation then
                liftEffect $ CCN.alert "Fill in email and confirmation"
         else if email /= emailConfirmation then
                liftEffect $ CCN.alert "Email and confirmation do not match"
         else do
                Ok <- CCNT.post' (SRO.fromRouteAbsolute AccountEmail) model
                liftEffect $ CCN.alert "Email changed"
        FAE.noChanges

changePassword :: SettingsModel -> Aff (SettingsModel -> SettingsModel)
changePassword model@(SettingsModel { password, passwordConfirmation }) = do
        if DS.null password || DS.null passwordConfirmation then
                liftEffect $ CCN.alert "Fill in password and confirmation"
         else if password /= passwordConfirmation then
                liftEffect $ CCN.alert "Password and confirmation do not match"
         else do
                Ok <- CCNT.post' (SRO.fromRouteAbsolute AccountPassword) model
                liftEffect $ do
                        CCN.alert "Password changed, you will be logged out"
                        CCL.logout
        FAE.noChanges

setField field value = pure $ \model -> SN.updateSettingsModel model (R.set field value)
