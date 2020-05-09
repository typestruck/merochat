module Client.Profile.Update where

import Prelude
import Shared.Profile.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.Common.Notification as CCNO
import Data.Array as DA
import Data.Date (Month)
import Data.Date as DD
import Data.Enum (class BoundedEnum)
import Data.Enum as DE
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.String.Read as DSR
import Data.String.Read as DST
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Flame (Key)
import Flame.Application.Effectful (AffUpdate)
import Flame.Application.Effectful as FAE
import Record as R
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Router as SR
import Shared.Types (Ok(..), Route(..))
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event as WEE
import Web.HTML.HTMLElement as WHH

foreign import setEditorContents_ :: EffectFn2 Editor String Unit

getFileInput :: Effect Element
getFileInput = CCD.querySelector "#avatar-file-input"

update :: AffUpdate ProfileModel ProfileMessage
update { model, message } =
        case message of
                SelectAvatar -> selectAvatar
                SetAvatar base64 -> setProfileField (SProxy :: SProxy "avatar") base64
                SetCountry country -> setHideProfileField (SProxy :: SProxy "isCountryVisible") (SProxy :: SProxy "country") $ DI.fromString country
                SetGender gender -> setHideProfileField (SProxy :: SProxy "isGenderVisible") (SProxy :: SProxy "gender") (DSR.read gender :: Maybe Gender)
                SetYear year -> setYear $ DI.fromString year
                SetMonth month -> setMonth $ DI.fromString month
                SetDay day -> setDay $ DI.fromString day
                SetName event -> setFieldFromInnerText event (SProxy :: SProxy "name") 50
                SetHeadline event -> setFieldFromInnerText event (SProxy :: SProxy "headline") 200
                SetDescription description -> setProfileField (SProxy :: SProxy "description") $ DS.take 10000 description
                SetInitialDescription editor -> setEditorContents editor model

                ToggleName (Tuple key _) -> blurEditionOnEnter key "#profile-edition-name"
                ToggleHeadline (Tuple key _) -> blurEditionOnEnter key "#profile-edition-headline"
                SetTagEnter (Tuple key tag) -> addTag key tag

                AddLanguage language -> addLanguage <<< SP.fromInt <<< SU.unsafeFromJust "addLanguage" $ DI.fromString language

                RemoveLanguage language event -> removeLanguage language event
                RemoveTag tag event -> removeTag tag event

                ToggleAge visible -> setModelField (SProxy :: SProxy "isAgeVisible") visible
                ToggleCountry visible -> setModelField (SProxy :: SProxy "isCountryVisible") visible
                ToggleGender visible -> setModelField (SProxy :: SProxy "isGenderVisible") visible
                ToggleLanguages visible -> setModelField (SProxy :: SProxy "isLanguagesVisible") visible
                ToggleTags visible -> setModelField (SProxy :: SProxy "isTagsVisible") visible

                SaveProfile -> saveProfile model

setEditorContents editor (ProfileModel { user: ProfileUser { description } }) = do
        liftEffect $ EU.runEffectFn2 setEditorContents_ editor description
        FAE.noChanges

setFieldFromInnerText event field characters = do
        value <- liftEffect $ CCD.innerTextFromTarget event
        setProfileField field $ DS.take characters value

blurEditionOnEnter :: Key -> String -> Aff (ProfileModel -> ProfileModel)
blurEditionOnEnter key id = do
        when (key == "Enter") $ liftEffect do
                element <- CCD.querySelector id
                WHH.blur <<< SU.unsafeFromJust "blurEditionOnEnter" $ WHH.fromElement element
        FAE.noChanges

selectAvatar :: Aff (ProfileModel -> ProfileModel)
selectAvatar = do
        liftEffect do
                input <- getFileInput
                WHH.click <<< SU.unsafeFromJust "selectAvatar" $ WHH.fromElement input
        FAE.noChanges

setYear :: Maybe Int -> Aff (ProfileModel -> ProfileModel)
setYear year = updateBirthday setYear'
        where setYear' (Tuple _ rest) = Tuple year rest

setMonth :: Maybe Int -> Aff (ProfileModel -> ProfileModel)
setMonth month = updateBirthday setMonth'
        where setMonth' (Tuple year (Tuple _ day)) = Tuple year (Tuple month day)

setDay :: Maybe Int -> Aff (ProfileModel -> ProfileModel)
setDay day = updateBirthday setDay'
        where   setDay' (Tuple year (Tuple month _)) = Tuple year (Tuple month day)

updateBirthday :: ((Tuple (Maybe Int) (Tuple (Maybe Int) (Maybe Int))) -> (Tuple (Maybe Int) (Tuple (Maybe Int) (Maybe Int)))) -> Aff (ProfileModel -> ProfileModel)
updateBirthday updater = pure $ \model -> SN.updateProfileModel model $ \record ->
        let  updatedBirthday = updater record.birthday in record {
                birthday = updatedBirthday,
                isAgeVisible = isAgeVisible' updatedBirthday,
                user = SN.updateProfile record.user $ \userRecord -> userRecord {
                        birthday = setBirthday userRecord.birthday updatedBirthday
                }
        }
        where   toDateComponent :: forall d. BoundedEnum d => Int -> d
                toDateComponent = SU.unsafeFromJust "setBirthday" <<< DE.toEnum
                isAgeVisible' =
                        case _ of
                                Tuple Nothing _ -> true
                                Tuple (Just _) (Tuple (Just _) (Just _)) -> true
                                _ -> false
                setBirthday birthday =
                        case _ of
                                Tuple (Just year) (Tuple (Just month) (Just day)) -> MDate <$> DD.exactDate (toDateComponent year) (toDateComponent month) (toDateComponent day)
                                Tuple Nothing _ -> Nothing -- so the age can be cleared
                                _ -> birthday

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
--REFACTOR: abstract with the tag functions bellow
addLanguage language =
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _
                {
                        isLanguagesVisible = true,
                        user = SN.updateProfile user $ \record -> record {
                                languages = DA.snoc record.languages language
                        }
                }
removeLanguage language event = do
        --I am not sure if this is correct behavior: the span which the event bubbles to is removed from the dom
        -- should the event still occur?
        liftEffect $ WEE.stopPropagation event
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _
                {
                        isLanguagesVisible = true,
                        user = SN.updateProfile user $ \record -> record {
                                languages = SU.unsafeFromJust "remove language" do
                                        index <- DA.findIndex ( _ == language) record.languages
                                        DA.deleteAt index record.languages
                        }
                }

addTag key tag =
        case key of
                "Enter" ->
                        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _
                                {
                                        isTagsVisible = true,
                                        user = SN.updateProfile user $ \record -> record {
                                                tags = DA.snoc record.tags tag
                                        }
                                }
                _ -> FAE.noChanges
removeTag tag event = do
        liftEffect $ WEE.stopPropagation event
        pure $ \model@(ProfileModel { user }) -> SN.updateProfileModel model $ _
                {
                        isTagsVisible = true,
                        user = SN.updateProfile user $ \record -> record {
                                tags = SU.unsafeFromJust "remove tag" do
                                        index <- DA.findIndex ( _ == tag) record.tags
                                        DA.deleteAt index record.tags
                        }
                }

saveProfile :: ProfileModel -> Aff (ProfileModel -> ProfileModel)
saveProfile (ProfileModel { user }) = do
        --displays back auto generated fields
        updatedUser <- CCN.post' (SR.fromRouteAbsolute Profile) user
        liftEffect $ CCNO.alert "Profile updated"
        FAE.diff { user : updatedUser }

