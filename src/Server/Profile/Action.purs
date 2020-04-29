module Server.Profile.Action where

import Prelude
import Server.Types
import Shared.Profile.Types
import Shared.Types

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
import Server.Response as SR
import Shared.Newtype as SN
import Server.Profile.Database as SPD
import Shared.Unsafe as SU

invalidImageMessage :: String
invalidImageMessage = "Invalid image"

imageTooBigMessage :: String
imageTooBigMessage = "Max allowed size for avatar is 500kb"

allowedMediaTypes :: HashMap String String
allowedMediaTypes = DH.fromFoldable [Tuple "data:image/png;base64" ".png", Tuple "data:image/jpeg;base64" ".jpg", Tuple "data:image/tiff;base64" ".tiff", Tuple "data:image/bmp;base64" ".bmp" ]

saveProfile :: PrimaryKey -> ProfileUser -> ServerEffect Ok
saveProfile id profileUser@(ProfileUser { avatar }) = do
        Tuple buffer mediaType <- base64From $ DS.split (Pattern ",") avatar
        uuid <- R.liftEffect (DU.toString <$> DU.genUUID)
        let fileName = uuid <> SU.unsafeFromJust "base64From" (DH.lookup mediaType allowedMediaTypes)
        R.liftEffect $ NFS.writeFile ("src/Client/media/upload/" <> fileName) buffer

        SPD.saveProfile $ SN.updateProfile profileUser $ _ {
                id = id,
                avatar = fileName
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
                                                 else
                                                        pure $ Tuple buffer mediaType
                                         else
                                                SR.throwBadRequest invalidImageMessage
                                _ -> SR.throwBadRequest invalidImageMessage