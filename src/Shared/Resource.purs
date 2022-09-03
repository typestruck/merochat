module Shared.Resource where

import Prelude

import Data.Either (Either(..))
import Data.Either as DE
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Tuple (Tuple(..))
import Environment (production)

allowedMediaTypes ∷ HashMap String String
allowedMediaTypes = DH.fromFoldable [ Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]

base ∷ Int
base = 500

maxImageSize ∷ Int
maxImageSize = base * 1024

maxImageSizeKB ∷ String
maxImageSizeKB = show base <> " KB"

productionBasePath ∷ String
productionBasePath = "https://static.melan.chat/file/ourmelon/"

developmentImageBasePath ∷ String
developmentImageBasePath = "/client/media/"

developmentJsBasePath ∷ String
developmentJsBasePath = "/client/javascript/"

developmentCssBasePath ∷ String
developmentCssBasePath = "/client/css/"

data ResourceType = Css | Js | Png | Ico | Included

derive instance Eq ResourceType

data Bundle
      = Common
      | Emoji
      | Experiments
      | Backer
      | Base
      | External
      | Help
      | Im
      | InternalHelp
      | Landing
      | Leaderboard
      | Login
      | Profile
      | Recover
      | Settings

data Media
      = Logo3Small
      | LogoSmall
      | Logo
      | Favicon
      | Loading
      | Point1
      | Point2
      | Point3
      | Point4
      | Point5
      | Point6
      | Point7
      | Works1
      | Works2
      | Works3
      | Avatar1
      | Avatar2
      | Avatar3
      | Avatar4
      | Avatar5
      | Avatar6
      | Avatar7
      | Avatar8
      | NicolasCageHiclipart
      | SocratesStingWikimedia
      | BatmanNounProjectAnushaNarvekar
      | Upload String

bundlePath ∷ Bundle → ResourceType → String
bundlePath b = resourcePath (Right b)

mediaPath ∷ Media → ResourceType → String
mediaPath b = resourcePath (Left b)

resourcePath ∷ Either Media Bundle → ResourceType → String
resourcePath res tp = path <> named <> DE.either (const "") replacement res <> resourceType tp
      where
      named = resourceName res

      path
            | production = productionBasePath
            | otherwise = case tp of
                    Js → developmentJsBasePath
                    Css → developmentCssBasePath
                    _ → developmentImageBasePath

uploadPath ∷ String
uploadPath = "upload/"

uploadedImagePath ∷ String
uploadedImagePath = (if production then productionBasePath else developmentImageBasePath) <> uploadPath

resourceName ∷ Either Media Bundle → String
resourceName = case _ of
      Right Common → "common"
      Right Emoji → "emoji"
      Right Experiments → "experiments"
      Right Backer → "backer"
      Right Base → "base"
      Right External → "external"
      Right Help → "help"
      Right Im → "im"
      Right InternalHelp → "internalHelp"
      Right Landing → "landing"
      Right Leaderboard → "leaderboard"
      Right Login → "login"
      Right Profile → "profile"
      Right Recover → "recover"
      Right Settings → "settings"
      Left Logo3Small → "logo-3-small"
      Left LogoSmall → "logo-small"
      Left Logo → "logo"
      Left Favicon → "favicon"
      Left Loading → "loading"
      Left Point1 → "point1"
      Left Point2 → "point2"
      Left Point3 → "point3"
      Left Point4 → "point4"
      Left Point5 → "point5"
      Left Point6 → "point6"
      Left Point7 → "point7"
      Left Works1 → "works1"
      Left Works2 → "works2"
      Left Works3 → "works3"
      Left Avatar1 → "avatar-1"
      Left Avatar2 → "avatar-2"
      Left Avatar3 → "avatar-3"
      Left Avatar4 → "avatar-4"
      Left Avatar5 → "avatar-5"
      Left Avatar6 → "avatar-6"
      Left Avatar7 → "avatar-7"
      Left Avatar8 → "avatar-8"
      Left NicolasCageHiclipart → "nicolas_cage_hiclipart"
      Left SocratesStingWikimedia → "socrates_Sting_wikimedia"
      Left BatmanNounProjectAnushaNarvekar → "batman_noun_project_Anusha_Narvekar"
      Left (Upload up) → up

resourceType ∷ ResourceType → String
resourceType = case _ of
      Js → ".bundle.js"
      Css → ".css"
      Png → ".png"
      Ico → ".ico"
      Included → ""

replacement ∷ Bundle → String
replacement bundle
      | production = case bundle of ---- for production we replace this with the actual file content hash, so it has to be hardcoded and must match the resource name
            Common → ".[common-contenthash]"
            Emoji → ".[emoji-contenthash]"
            Experiments → ".[experiments-contenthash]"
            Backer → ".[backer-contenthash]"
            Base → ".[base-contenthash]"
            External → ".[external-contenthash]"
            Help → ".[help-contenthash]"
            Im → ".[im-contenthash]"
            InternalHelp → ".[internalHelp-contenthash]"
            Landing → ".[landing-contenthash]"
            Leaderboard → ".[leaderboard-contenthash]"
            Login → ".[login-contenthash]"
            Profile → ".[profile-contenthash]"
            Recover → ".[recover-contenthash]"
            Settings → ".[settings-contenthash]"
      | otherwise = ""

updateHash ∷ String
updateHash = replacement Common <> replacement Im <> replacement Profile <> replacement Experiments <> replacement Settings