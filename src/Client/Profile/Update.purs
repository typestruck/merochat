module Client.Profile.Update where

import Prelude
import Shared.Profile.Types
import Shared.Types

import Client.Common.DOM (nameChanged)
import Client.Common.DOM as CCD
import Client.Common.Network as CCN
import Client.Common.Notification as CCNO
import Data.Array as DA
import Data.Date (Month)
import Data.Date as DD
import Data.Either (Either(..))
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
import Flame.Types (NodeData(..))
import Record as R
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Router as SR
import Shared.Types (Ok(..), Route(..))
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.DOM.Element as WDE
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.HTML.HTMLElement as WHH

foreign import setEditorContent_ :: EffectFn2 Editor String Unit

getFileInput :: Effect Element
getFileInput = CCD.querySelector "#avatar-file-input"

update :: AffUpdate ProfileModel ProfileMessage
update { model: model@(ProfileModel { editors }), message } =
        case message of
                SelectAvatar -> selectAvatar

                SetAvatar base64 -> setProfileField (SProxy :: SProxy "avatar") $ Just base64
                SetCountry country -> setHideProfileField (SProxy :: SProxy "isCountryVisible") (SProxy :: SProxy "country") $ DI.fromString country
                SetGender gender -> setHideProfileField (SProxy :: SProxy "isGenderVisible") (SProxy :: SProxy "gender") (DSR.read gender :: Maybe Gender)
                SetYear year -> setYear $ DI.fromString year
                SetMonth month -> setMonth $ DI.fromString month
                SetDay day -> setDay $ DI.fromString day
                SetName name -> setEditorFieldOrGenerate Name (SProxy :: SProxy "name") 50 name editors.name
                SetHeadline headline -> setEditorFieldOrGenerate Headline (SProxy :: SProxy "headline") 200 headline editors.headline
                SetDescription description -> setEditorFieldOrGenerate Description (SProxy :: SProxy "description") 10000 description editors.description
                SetEditors editor -> setEditors editor model
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

setEditorContent :: Editor -> String -> Aff Unit
setEditorContent editor = liftEffect <<< EU.runEffectFn2 setEditorContent_ editor

setEditors :: Editors Editor Editor Editor -> ProfileModel -> Aff (ProfileModel -> ProfileModel)
setEditors editor (ProfileModel { user: ProfileUser { name, headline, description } }) = do
        setEditorContent editor.name name
        setEditorContent editor.headline headline
        setEditorContent editor.description description
        FAE.diff {
                editors: {
                        name: Just editor.name,
                        headline: Just editor.headline,
                        description: Just editor.description
                }
        }

setEditorFieldOrGenerate what field characters value editor = do
        let trimmed = DS.trim value
        toSet <- if DS.null trimmed then do
                        JSONResponse name <- CCN.get' $ Generate { what }
                        pure name
                  else pure trimmed
        setEditorContent (SU.unsafeFromJust "setEditorFieldOrGenerate" editor) toSet
        setProfileField field $ DS.take characters toSet

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
saveProfile model@(ProfileModel { user: user@(ProfileUser { name }) }) = do
        Ok <- CCN.post' (SR.fromRoute Profile) $ Just user
        liftEffect $ do
                CCNO.alert "Profile updated"
                --let im know that the name has changed
                CCD.dispatchCustomEvent $ CCD.createCustomEvent nameChanged name
        FAE.noChanges

