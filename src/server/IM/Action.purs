module Server.IM.Action where

import Prelude
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Array as DA
import Data.DateTime (DateTime(..))
import Data.Foldable as DF
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple (Tuple(..))
import Data.UUID as DU
import Database.PostgreSQL (Pool)
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Server.File (allowedMediaTypes)
import Server.IM.Database as SID
import Server.Response as SRR
import Shared.File (maxImageSize)
import Shared.Newtype as SN
import Shared.Unsafe as SU

foreign import sanitize :: String -> String

suggest :: PrimaryKey -> ServerEffect (Array IMUser)
suggest id = SID.suggest id

contactList :: PrimaryKey -> Int -> ServerEffect (Array Contact)
contactList id page = do
      contacts <- SID.presentContacts id page
      history <- SID.chatHistory id $ map (_.id <<< DN.unwrap <<< _.user <<< DN.unwrap) contacts
      let userHistory = DF.foldl intoHashMap  DH.empty history
      pure $ intoContacts userHistory <$> contacts

      where intoHashMap hashMap m@(HistoryMessage { sender, recipient }) =
                  DH.insertWith (<>) (if sender == id then recipient else sender) [m] hashMap

            intoContacts userHistory contact@(Contact { user: IMUser { id } }) = SN.updateContact contact $ _ {
                  history = SU.fromJust $ DH.lookup id userHistory
            }

singleContact :: PrimaryKey -> PrimaryKey -> ServerEffect Contact
singleContact id otherID = do
      contact <- SID.presentSingleContact id otherID
      history <- SID.chatHistoryBetween id otherID 0
      pure <<< SN.updateContact contact $ _ {
            history = history
      }

insertMessage :: forall r. PrimaryKey -> PrimaryKey -> MessageContent -> BaseEffect { pool :: Pool | r } (Tuple PrimaryKey String)
insertMessage id otherID content = do
      message <- case content of
            Text m -> pure m
            Image (Tuple caption base64) -> do
                  path <- base64From $ DS.split (Pattern ",") base64
                  pure $ "![" <> caption  <> "](" <> path <> ")"
      id <- SID.insertMessage id otherID $ sanitize message
      pure $ Tuple id message
      where base64From =
                  case _ of
                        [mediaType, base64] -> do
                              if FD.any (_ == mediaType) $ DH.keys allowedMediaTypes then do
                                    buffer <- R.liftEffect $ NB.fromString base64 Base64
                                    bufferSize <- R.liftEffect $ NB.size buffer
                                    if bufferSize > maxImageSize then
                                          invalidImage
                                     else do
                                          uuid <- R.liftEffect (DU.toString <$> DU.genUUID)
                                          let fileName = uuid <> SU.fromJust (DH.lookup mediaType allowedMediaTypes)
                                          R.liftEffect $ NFS.writeFile ("src/client/media/upload/" <> fileName) buffer

                                          pure $ "/client/media/upload/" <> fileName
                               else
                                    invalidImage
                        _ -> invalidImage
            invalidImage = SRR.throwBadRequest "Invalid image"

blockUser :: PrimaryKey -> PrimaryKey -> ServerEffect Ok
blockUser id otherID = do
      SID.insertBlock id otherID
      pure Ok

missedMessages :: PrimaryKey -> DateTime -> ServerEffect (Array Contact)
missedMessages id since = do
      history <- SID.chatHistorySince id since
      contacts <- SID.presentSelectedContacts <<< DA.nubEq $ map (_.sender <<< DN.unwrap) history
      let userHistory = DF.foldl (intoHashMap id) DH.empty history
      pure $ intoContacts userHistory <$> contacts

      where intoHashMap userID hashMap m@(HistoryMessage { recipient }) = DH.insertWith (<>) recipient [m] hashMap
            intoContacts userHistory contact@(Contact { user: IMUser { id } }) = SN.updateContact contact $ _ {
                  history = SU.fromJust $ DH.lookup id userHistory
            }