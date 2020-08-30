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

import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event as WEE

getFileInput :: Effect Element
getFileInput = CCD.querySelector "#avatar-file-input"

update :: AffUpdate ProfileModel ProfileMessage
update { model, message } =
      case message of
            SelectAvatar -> selectAvatar
            SetField setter -> pure setter
            SetAvatar base64 -> setProfileField (SProxy :: SProxy "avatar") $ Just base64
            SetCountry country -> setHideProfileField (SProxy :: SProxy "isCountryVisible") (SProxy :: SProxy "country") $ SP.fromString country
            SetGender gender -> setHideProfileField (SProxy :: SProxy "isGenderVisible") (SProxy :: SProxy "gender") (DSR.read gender :: Maybe Gender)
            SetYear year -> setYear $ DI.fromString year
            SetMonth month -> setMonth $ DI.fromString month
            SetDay day -> setDay $ DI.fromString day
            SetName name -> setFieldOrGenerate Name (SProxy :: SProxy "name") 50 name
            SetHeadline headline -> setFieldOrGenerate Headline (SProxy :: SProxy "headline") 200 headline
            SetDescription description -> setFieldOrGenerate Description (SProxy :: SProxy "description") 10000 description
            SetTagEnter (Tuple key tag) -> addTag key tag

            AddLanguage language -> addLanguage <<< SP.fromInt <<< SU.fromJust $ DI.fromString language

            RemoveLanguage language event -> removeLanguage language event
            RemoveTag tag event -> removeTag tag event

            SaveProfile -> saveProfile model

setFieldOrGenerate what field characters value = do
      let trimmed = DS.trim value
      toSet <- if DS.null trimmed then do
                  CCN.response $ request.profile.generate { query: { what } }
                else
                  pure trimmed
      setProfileField field $ DS.take characters toSet

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
      let  updatedBirthday = updater model.birthday in model {
            birthday = updatedBirthday,
            isAgeVisible = isAgeVisible' updatedBirthday,
            user = model.user {
                  birthday = setBirthday model.user.birthday updatedBirthday
            }
      }
      where toDateComponent :: forall d. BoundedEnum d => Int -> d
            toDateComponent = SU.fromJust <<< DE.toEnum
            isAgeVisible' =
                  case _ of
                        Tuple Nothing _ -> true
                        Tuple (Just _) (Tuple (Just _) (Just _)) -> true
                        _ -> false
            setBirthday birthday =
                  case _ of
                        Tuple (Just year) (Tuple (Just month) (Just day)) -> DateWrapper <$> DD.exactDate (toDateComponent year) (toDateComponent month) (toDateComponent day)
                        Tuple Nothing _ -> Nothing -- so the age can be cleared
                        _ -> birthday

setProfileField field value =
      pure $ \model@({ user }) -> model {
            user = R.set field value user
      }

setHideProfileField visibilityField field value =
      pure $ \model@({ user }) ->
            R.set visibilityField true $ model {
                  user = R.set field value user
            }
--REFACTOR: abstract with the tag functions bellow
addLanguage language =
      pure $ \model@({ user }) -> model
            {
                  isLanguagesVisible = true,
                  user = user {
                        languages = DA.snoc user.languages language
                  }
            }
removeLanguage language event = do
      --I am not sure if this is correct behavior: the span which the event bubbles to is removed from the dom
      -- should the event still occur?
      liftEffect $ WEE.stopPropagation event
      pure $ \model@({ user }) -> model
            {
                  isLanguagesVisible = true,
                  user = user {
                        languages = SU.fromJust  do
                              index <- DA.findIndex ( _ == language) user.languages
                              DA.deleteAt index user.languages
                  }
            }

addTag key tag =
      case key of
            "Enter" ->
                  pure $ \model@({ user }) -> model
                        {
                              isTagsVisible = true,
                              user = user {
                                    tags = DA.snoc user.tags tag
                              }
                        }
            _ -> FAE.noChanges
removeTag tag event = do
      liftEffect $ WEE.stopPropagation event
      pure $ \model@({ user }) -> model
            {
                  isTagsVisible = true,
                  user = user {
                        tags = SU.fromJust do
                              index <- DA.findIndex ( _ == tag) user.tags
                              DA.deleteAt index user.tags
                  }
            }

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile model@({ user: user@{ name }}) = do
      void $ request.profile.post { body: user }
      liftEffect do
            CCNO.alert "Profile updated"
            --let im know that the name has changed
            CCD.dispatchCustomEvent $ CCD.createCustomEvent nameChanged name
      FAE.noChanges

