module Server.Profile.Action where

import Prelude
import Server.Types
import Shared.Profile.Types
import Shared.Types

import Data.Array as DA
import Data.Foldable as FD
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Data.UUID as DU
import Debug.Trace (spy)
import Effect.Console as EC
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Server.Bender as SB
import Server.Profile.Database as SPD
import Server.Response as SR
import Shared.Newtype as SN
import Shared.Unsafe as SU

invalidImageMessage :: String
invalidImageMessage = "Invalid image"

imageTooBigMessage :: String
imageTooBigMessage = "Max allowed size for avatar is 500kb"

allowedMediaTypes :: HashMap String String
allowedMediaTypes = DH.fromFoldable [Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]

saveProfile :: PrimaryKey -> ProfileUser -> ServerEffect Ok
saveProfile id profileUser@(ProfileUser { name, headline, description, avatar, languages }) = do
        updatedAvatar <- base64From $ DS.split (Pattern ",") avatar
        updatedName <- nameOrGenerated name
        updatedHeadline <- headlineOrGenerated headline
        updatedDescription <- descriptionOrGenerated description

        SPD.saveProfile {
                user: SN.updateProfile profileUser $ _ {
                        id = id,
                        avatar =  updatedAvatar,
                        name = updatedName,
                        headline = updatedHeadline,
                        description = updatedDescription
                },
                languages
        }
        pure Ok

        where   base64From =
                        case _ of
                                [mediaType, base64] -> do
                                        if FD.any (_ == mediaType) $ DH.keys allowedMediaTypes then do
                                                buffer <- R.liftEffect $ NB.fromString base64 Base64
                                                bufferSize <- R.liftEffect $ NB.size buffer
                                                if bufferSize > 500 * 1024 then
                                                        SR.throwBadRequest imageTooBigMessage
                                                 else do
                                                        uuid <- R.liftEffect (DU.toString <$> DU.genUUID)
                                                        let fileName = uuid <> SU.unsafeFromJust "base64From" (DH.lookup mediaType allowedMediaTypes)
                                                        R.liftEffect $ NFS.writeFile ("src/Client/media/upload/" <> fileName) buffer

                                                        pure fileName
                                         else
                                                SR.throwBadRequest invalidImageMessage
                                _ -> pure <<< SU.unsafeFromJust "base64from" <<< DA.last $ DS.split (Pattern "/") avatar

                textOrGenerated gen text  =
                        case text of
                                "" -> gen
                                _ -> pure $ DS.trim text
                nameOrGenerated = textOrGenerated SB.generateName
                headlineOrGenerated = textOrGenerated SB.generateHeadline
                descriptionOrGenerated = textOrGenerated SB.generateDescription

