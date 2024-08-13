module Client.Profile.Update where

import Prelude
import Shared.Experiments.Types as SET
import Shared.Im.Types as SIT
import Shared.Profile.Types

import Client.Common.Dom as CCD
import Client.Common.File as CCF
import Client.Common.Network (request)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as EA
import Effect.Class (liftEffect)
import Flame.Application.Effectful (AffUpdate, Environment)
import Flame.Application.Effectful as FAE
import Flame.Subscription as FS
import Payload.ResponseTypes (Response(..))
import Shared.Element (ElementId(..))
import Shared.Network (RequestStatus(..))
import Shared.Network as SN
import Shared.Options.MountPoint (imId)
import Web.DOM (Element)

getFileInput ∷ Effect Element
getFileInput = CCD.unsafeGetElementById AvatarFileInput

update ∷ AffUpdate ProfileModel ProfileMessage
update rc@{ message } =
      case message of
            SelectAvatar → selectAvatar
            SetPField setter → pure setter
            Save field → saveField rc field
            AfterRegistration → setRegistrationMessage
            UpdatePrivileges kp → updatePrivileges kp

updatePrivileges ∷ _ → Aff (ProfileModel → ProfileModel)
updatePrivileges { karma, privileges } = pure (_ { user { karma = karma, privileges = privileges } })

saveField ∷ Environment ProfileModel ProfileMessage → Field → Aff (ProfileModel → ProfileModel)
saveField rc field = do
      rc.display $ _ { loading = true }
      case field of
            Generated what → saveGeneratedField rc what
            Avatar base64 → saveAvatar rc base64
            Age → saveAge rc
            Gender → saveGender rc
            Country → saveCountry rc
            Languages → saveLanguages rc
            Tags → saveTags rc
      EA.delay $ Milliseconds 4000.0
      pure (_ { updateRequestStatus = Nothing })

saveGeneratedField ∷ Environment ProfileModel ProfileMessage → What → Aff Unit
saveGeneratedField rc@{ display } what =
      case what of
            Name → do
                  saveWith display (req rc.model.nameInputed) $ \name → do
                        liftEffect <<< FS.send imId $ SIT.SetNameFromProfile name
                        display
                              ( _
                                      { nameInputed = Nothing
                                      , user { name = name }
                                      }
                              )
            Headline → do
                  saveWith display (req rc.model.headlineInputed) $ \headline →
                        display
                              ( _
                                      { headlineInputed = Nothing
                                      , user { headline = headline }
                                      }
                              )
            Description → do
                  saveWith display (req rc.model.descriptionInputed) $ \description →
                        display
                              ( _
                                      { descriptionInputed = Nothing
                                      , user { description = description }
                                      }
                              )
      where
      req value = request.profile.field.generated { body: { field: what, value: if value == Just "" then Nothing else value } }

saveAvatar ∷ Environment ProfileModel ProfileMessage → Maybe String → Aff Unit
saveAvatar { display } base64 = do
      saveWith display (request.profile.field.avatar { body: { base64 } }) <<< const $ do
            liftEffect <<< FS.send imId $ SIT.SetAvatarFromProfile base64
            display $ _ { user { avatar = base64 } }

saveAge ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveAge { display, model: { ageInputed } } = do
      let birthday = DM.fromMaybe Nothing ageInputed
      response ← request.profile.field.age { body: { birthday } }
      _ ← display $ _ { loading = false }
      case response of
            Right _ →
                  display $ _
                        { updateRequestStatus = Just Success
                        , ageInputed = Nothing
                        , user { age = birthday }
                        }
            Left err →
                  display $ _ { updateRequestStatus = Just <<< Failure $ SN.errorMessage err }

saveGender ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveGender { display, model: { genderInputed } } = do
      let gender = DM.fromMaybe Nothing genderInputed
      saveWith display (request.profile.field.gender { body: { gender } }) <<< const $
            display
                  ( _
                          { genderInputed = Nothing
                          , user { gender = gender }
                          }
                  )

saveCountry ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveCountry { display, model: { countryInputed } } = do
      let country = DM.fromMaybe Nothing countryInputed
      saveWith display (request.profile.field.country { body: { country } }) <<< const $
            display
                  ( _
                          { countryInputed = Nothing
                          , user { country = country }
                          }
                  )

saveLanguages ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveLanguages { display, model: { languagesInputedList } } = do
      saveWith display (request.profile.field.language { body: { ids: languagesInputedList } }) <<< const $
            display
                  ( _
                          { languagesInputed = Nothing
                          , languagesInputedList = Nothing
                          , user { languages = DM.fromMaybe [] languagesInputedList }
                          }
                  )

saveTags ∷ Environment ProfileModel ProfileMessage → Aff Unit
saveTags { display, model: { tagsInputedList } } = do
      saveWith display (request.profile.field.tag { body: { tags: tagsInputedList } }) <<< const $
            display
                  ( _
                          { tagsInputed = Nothing
                          , tagsInputedList = Nothing
                          , user { tags = DM.fromMaybe [] tagsInputedList }
                          }
                  )

saveWith display request success = do
      response ← save display request
      case response of
            Nothing → pure unit
            Just r → success r

save display request = do
      result ← request
      _ ← display $ _ { loading = false }
      case result of
            Right (Response { body }) → do
                  --compiler bug? we need _ <- here even tho display is Unit
                  _ ← display $ _ { updateRequestStatus = Just Success }
                  pure $ Just body
            Left err → do
                  _ ← display $ _ { updateRequestStatus = Just <<< Failure $ SN.errorMessage err }
                  pure Nothing

selectAvatar ∷ Aff (ProfileModel → ProfileModel)
selectAvatar = do
      liftEffect do
            field ← getFileInput
            CCF.triggerFileSelect field
      FAE.noChanges

setRegistrationMessage ∷ Aff (ProfileModel → ProfileModel)
setRegistrationMessage = pure $ _ { registrationMessage = true }