module Server.Settings.Action where

import Prelude
import Server.Types hiding (BenderAction(..))
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
import Server.Response as SRR
import Shared.Newtype as SN
import Shared.Unsafe as SU

invalidImageMessage :: String
invalidImageMessage = "Invalid image"

imageTooBigMessage :: String
imageTooBigMessage = "Max allowed size for avatar is 500kb"

allowedMediaTypes :: HashMap String String
allowedMediaTypes = DH.fromFoldable [Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]

generate :: Generate -> ServerEffect JSONString
generate =
        map JSONString <<< case _ of
                Name -> SB.generateName
                Headline -> SB.generateHeadline
                Description -> SB.generateDescription

saveProfile :: PrimaryKey -> ProfileUser -> ServerEffect Ok
saveProfile id profileUser@(ProfileUser { name, headline, description, avatar, languages, tags }) = do
        when (isNull name || isNull headline || isNull description) $ SRR.throwBadRequest "Missing required info fields"

        updatedAvatar <- base64From $ DS.split (Pattern ",") avatar
        let updatedUser =
                SN.updateProfile profileUser $ _ {
                        id = id,
                        avatar = updatedAvatar
                }
        SPD.saveProfile {
                user: updatedUser,
                languages,
                tags
        }
        pure Ok

        where   isNull = DS.null <<< DS.trim

                base64From =
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
