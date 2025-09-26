module Shared.Resource where

import Prelude

import Data.Array as DA
import Data.Either (Either(..))
import Data.Either as DE
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Set (Set)
import Data.Set as DS
import Environment (production)

allowedMediaTypes ∷ HashMap String String
allowedMediaTypes = DH.fromFoldable <<< DA.zip [ "data:image/png;base64", "data:image/jpeg;base64", "data:image/tiff;base64", "data:image/bmp;base64", "data:image/gif;base64", "data:audio/webm;base64", "data:video/mp4;base64" ] $ DS.toUnfoldable allowedExtensions

allowedExtensions ∷ Set String
allowedExtensions = DS.fromFoldable [ ".png", ".jpg", ".tiff", ".bmp", ".gif", ".webm", ".mp4" ]

kb ∷ Int
kb = 1024

maxImageSize ∷ Int
maxImageSize = kb * 1000

maxImageSizeKB ∷ String
maxImageSizeKB = show kb <> " KB"

onlineBasePath ∷ String
onlineBasePath = "https://static.mero.chat/file/"

localBasePath ∷ String
localBasePath = "file/"

uploadFolder ∷ String
uploadFolder = "upload/"

bundleFolder ∷ String
bundleFolder = "bundle/"

defaultFolder ∷ String
defaultFolder = "default/"

data ResourceType = Css | Js | Png | Ico | Ignore | Svg

derive instance Eq ResourceType

data Bundle
      = Common
      | Emoji
      | Experiments
      | Backer
      | Base
      | Help
      | Im
      | InternalHelp
      | InternalBacker
      | Landing
      | Feedback
      | KarmaPrivileges
      | Login
      | Profile
      | Recover
      | Settings

data Media
      = LogoSmall
      | Logo
      | Favicon
      | Favicon1
      | Favicon2
      | Favicon3
      | Favicon4
      | Favicon5
      | Favicon6
      | Favicon7
      | Favicon8
      | Favicon9
      | Favicon10
      | Favicon10Plus
      | Loading
      | Avatar
      | NicolasCageHiclipart
      | SocratesStingWikimedia
      | BatmanNounProjectAnushaNarvekar
      | BackerAvatar
      | Upload String

bundlePath ∷ Bundle → ResourceType → String
bundlePath b = resourcePath (Right b)

resourcePath ∷ Either Media Bundle → ResourceType → String
resourcePath res tp = path <> named <> replaced <> resourceType tp
      where
      named = resourceName res
      replaced = DE.either (const "") (flip replacement tp) res
      basePath
            | production = onlineBasePath
            | otherwise = localBasePath
      path
            | tp == Js || tp == Css =
                    basePath <> bundleFolder
            | otherwise =
                    --pictures
                    case res of
                          Left (Upload _) → basePath <> uploadFolder
                          _ → basePath <> defaultFolder

resourceName ∷ Either Media Bundle → String
resourceName = case _ of
      Right Common → "common"
      Right Emoji → "emoji"
      Right Experiments → "experiments"
      Right Backer → "backer"
      Right Base → "base"
      Right Help → "help"
      Right Im → "im"
      Right InternalHelp → "internalHelp"
      Right InternalBacker → "internalBacker"
      Right Feedback → "feedback"
      Right Landing → "landing"
      Right KarmaPrivileges → "karmaPrivileges"
      Right Login → "login"
      Right Profile → "profile"
      Right Recover → "recover"
      Right Settings → "settings"
      Left LogoSmall → "logo-small"
      Left Logo → "logo"
      Left Favicon → "favicon"
      Left Favicon1 → "favicon-1"
      Left Favicon2 → "favicon-2"
      Left Favicon3 → "favicon-3"
      Left Favicon4 → "favicon-4"
      Left Favicon5 → "favicon-5"
      Left Favicon6 → "favicon-6"
      Left Favicon7 → "favicon-7"
      Left Favicon8 → "favicon-8"
      Left Favicon9 → "favicon-9"
      Left Favicon10 → "favicon-10"
      Left Favicon10Plus → "favicon-10-plus"
      Left BackerAvatar → "backer-avatar"
      Left Avatar → "avatar"
      Left NicolasCageHiclipart → "nicolas_cage_hiclipart"
      Left BatmanNounProjectAnushaNarvekar → "batman_noun_project_Anusha_Narvekar"
      Left Loading → "loading"
      Left SocratesStingWikimedia → "socrates_Sting_wikimedia"
      Left (Upload up) → up

resourceType ∷ ResourceType → String
resourceType = case _ of
      Ico → ".ico"
      Svg → ".svg"
      Png → ".png"
      Css → ".css"
      Js → ".bundle.js"
      Ignore → ""

replacement ∷ Bundle → ResourceType → String
replacement bundle tp
      | production =
              case bundle of -- for production we replace this with the actual file content hash, so it has to be hardcoded and must match the resource name
                    Common → reps ".[common-js-contenthash]" ".[common-css-contenthash]"
                    Emoji → reps ".[emoji-js-contenthash]" ".[emoji-css-contenthash]"
                    Experiments → reps ".[experiments-js-contenthash]" ".[experiments-css-contenthash]"
                    InternalBacker → reps ".[internalBacker-js-contenthash]" ".[internalBacker-css-contenthash]"
                    Backer → reps ".[backer-js-contenthash]" ".[backer-css-contenthash]"
                    Base → reps ".[base-js-contenthash]" ".[base-css-contenthash]"
                    Help → reps ".[help-js-contenthash]" ".[help-css-contenthash]"
                    Im → reps ".[im-js-contenthash]" ".[im-css-contenthash]"
                    Feedback → reps ".[feedback-js-contenthash]" ".[feedback-css-contenthash]"
                    InternalHelp → reps ".[internalHelp-js-contenthash]" ".[internalHelp-css-contenthash]"
                    Landing → reps ".[landing-js-contenthash]" ".[landing-css-contenthash]"
                    KarmaPrivileges → reps ".[karmaPrivileges-js-contenthash]" ".[karmaPrivileges-css-contenthash]"
                    Login → reps ".[login-js-contenthash]" ".[login-css-contenthash]"
                    Profile → reps ".[profile-js-contenthash]" ".[profile-css-contenthash]"
                    Recover → reps ".[recover-js-contenthash]" ".[recover-css-contenthash]"
                    Settings → reps ".[settings-js-contenthash]" ".[settings-css-contenthash]"
              where
              reps js css = if tp == Js then js else css
      | otherwise = ""

updateHash ∷ String
updateHash = replacement Common Js <> replacement Im Js <> replacement Im Css <> replacement Profile Js <> replacement Experiments Js <> replacement Settings Js <> replacement InternalBacker Js