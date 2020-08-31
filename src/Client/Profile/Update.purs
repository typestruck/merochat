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
import Shared.PrimaryKey as SP
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
            SetCountry country -> setAndDisplayField (SProxy :: SProxy "isCountryVisible") (SProxy :: SProxy "country") $ SP.fromString country
            SetGender gender -> setAndDisplayField (SProxy :: SProxy "isGenderVisible") (SProxy :: SProxy "gender") (DSR.read gender :: Maybe Gender)
            SetYear year -> setYear $ DI.fromString year
            SetMonth month -> setMonth $ DI.fromString month
            SetDay day -> setDay $ DI.fromString day
            SetName name -> setOrGenerateField Name (SProxy :: SProxy "name") 50 name
            SetHeadline headline -> setOrGenerateField Headline (SProxy :: SProxy "headline") 200 headline
            SetDescription description -> setOrGenerateField Description (SProxy :: SProxy "description") 10000 description
            SetTagEnter (Tuple key tag) -> addTag key tag

            AddLanguage language -> addLanguage <<< SP.fromInt <<< SU.fromJust $ DI.fromString language

            RemoveLanguage language event -> removeLanguage language event
            RemoveTag tag event -> removeTag tag event

            SaveProfile -> saveProfile model

selectAvatar :: Aff (ProfileModel -> ProfileModel)
selectAvatar = do
      liftEffect do
            input <- getFileInput
            CCF.triggerFileSelect input
      FAE.noChanges

setYear :: Maybe Int -> Aff (ProfileModel -> ProfileModel)
setYear year = updateBirthday setYear'
      where setYear' (Tuple _ rest) = Tuple year rest

setMonth :: Maybe Int -> Aff (ProfileModel -> ProfileModel)
setMonth month = updateBirthday setMonth'
      where setMonth' (Tuple year (Tuple _ day)) = Tuple year (Tuple month day)

setDay :: Maybe Int -> Aff (ProfileModel -> ProfileModel)
setDay day = updateBirthday setDay'
      where setDay' (Tuple year (Tuple month _)) = Tuple year (Tuple month day)

updateBirthday :: ((Tuple (Maybe Int) (Tuple (Maybe Int) (Maybe Int))) -> (Tuple (Maybe Int) (Tuple (Maybe Int) (Maybe Int)))) -> Aff (ProfileModel -> ProfileModel)
updateBirthday updater = pure $ \model ->
      let updatedBirthday = updater model.birthday
      in model {
            birthday = updatedBirthday,
            isAgeVisible = isAgeVisible' updatedBirthday,
            user = model.user {
                  birthday = setBirthday model.user.birthday updatedBirthday
            }
      }
      where isAgeVisible' =
                  case _ of
                        Tuple Nothing _ -> true
                        Tuple (Just _) (Tuple (Just _) (Just _)) -> true
                        _ -> false
            setBirthday birthday =
                  case _ of
                        Tuple (Just year) (Tuple (Just month) (Just day)) -> DateWrapper <$> DD.exactDate (SU.toEnum year) (SU.toEnum month) (SU.toEnum day)
                        Tuple Nothing _ -> Nothing -- so the age can be cleared
                        _ -> birthday

addLanguage language = appendAndDisplayField (SProxy :: SProxy "isLanguagesVisible") (SProxy :: SProxy "languages") language

removeLanguage language event = do
      --I am not sure if this is correct behavior: the span which the event bubbles to is removed from the dom
      -- should the event still occur?
      liftEffect $ WEE.stopPropagation event
      filterAndDisplayField (SProxy :: SProxy "isLanguagesVisible") (SProxy :: SProxy "languages") language

addTag key tag =
      case key of
            "Enter" -> appendAndDisplayField (SProxy :: SProxy "isTagsVisible") (SProxy :: SProxy "tags") tag
            _ -> FAE.noChanges

removeTag tag event = do
      liftEffect $ WEE.stopPropagation event
      filterAndDisplayField (SProxy :: SProxy "isTagsVisible") (SProxy :: SProxy "tags") tag

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile { user: user@{ name }} = do
      void $ request.profile.post { body: user }
      liftEffect do
            CCNO.alert "Profile updated"
            --let im know that the name has changed
            CCD.dispatchCustomEvent $ CCD.createCustomEvent nameChanged name
      FAE.noChanges

setAndDisplayField visibilityField userField value = pure (R.set visibilityField true <<< SS.setUserField userField value)

setOrGenerateField what userField characters value = do
      let trimmed = DS.trim value
      toSet <- if DS.null trimmed then do
                  CCN.response $ request.profile.generate { query: { what } }
                else
                  pure trimmed
      pure (SS.setUserField userField $ DS.take characters toSet)

appendAndDisplayField visibilityField userField value =
      pure $ \model@{user} -> R.set visibilityField true $ model {
            user = R.set userField (DA.snoc (R.get userField user) value) user
      }

filterAndDisplayField visibilityField userField value =
      pure $ \model@{user} -> R.set visibilityField true $ model {
            user = R.set userField (DA.filter (_ /= value) $ R.get userField user) user
      }