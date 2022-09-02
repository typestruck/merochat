module Shared.Resource where

import Environment (production)
import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Tuple (Tuple(..))

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

data Resource
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
      | Logo3Small
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

resourcePath ∷ Resource → ResourceType → String
resourcePath res tp = path <> named <> replacement res tp <> resourceType tp
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

resourceName ∷ Resource → String
resourceName = case _ of
      Common → "common"
      Emoji → "emoji"
      Experiments → "experiments"
      Backer → "backer"
      Base → "base"
      External → "external"
      Help → "help"
      Im → "im"
      InternalHelp → "internalHelp"
      Landing → "landing"
      Leaderboard → "leaderboard"
      Login → "login"
      Profile → "profile"
      Recover → "recover"
      Settings → "settings"
      Logo3Small → "logo-3-small"
      LogoSmall → "logo-small"
      Logo → "logo"
      Favicon → "favicon"
      Loading → "loading"
      Point1 → "point1"
      Point2 → "point2"
      Point3 → "point3"
      Point4 → "point4"
      Point5 → "point5"
      Point6 → "point6"
      Point7 → "point7"
      Works1 → "works1"
      Works2 → "works2"
      Works3 → "works3"
      Avatar1 → "avatar-1"
      Avatar2 → "avatar-2"
      Avatar3 → "avatar-3"
      Avatar4 → "avatar-4"
      Avatar5 → "avatar-5"
      Avatar6 → "avatar-6"
      Avatar7 → "avatar-7"
      Avatar8 → "avatar-8"
      NicolasCageHiclipart → "nicolas_cage_hiclipart"
      SocratesStingWikimedia → "socrates_Sting_wikimedia"
      BatmanNounProjectAnushaNarvekar → "batman_noun_project_Anusha_Narvekar"
      Upload up → up

resourceType ∷ ResourceType → String
resourceType = case _ of
      Js → ".bundle.js"
      Css → ".css"
      Png → ".png"
      Ico → ".ico"
      Included → ""

-- for production we replace this with the actual file content hash
replacement ∷ Resource → ResourceType → String
replacement res tp
      | production && (tp == Js || tp == Css) = ".[" <> resourceName res <> "-contenthash]"
      | otherwise = ""

updateHash ∷ String
updateHash = replacement Common Js <> replacement Im Js <> replacement Im Css <> replacement Profile Js <> replacement Experiments Js <> replacement Settings Js