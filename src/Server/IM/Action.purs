module Server.IM.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.DateTime (DateTime)
import Data.Foldable as DF
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Newtype as DN
import Data.String (Pattern(..))
import Data.String as DS
import Server.Response as SR
import Data.Tuple (Tuple(..))
import Data.UUID as DU
import Database.PostgreSQL (Pool)
import Node.Buffer as NB
import Node.Encoding (Encoding(..))
import Node.FS.Sync as NFS
import Run as R
import Shared.Options.File (allowedMediaTypes)
import Server.IM.Database as SID
import Server.Ok (ok)
import Server.Wheel as SW
import Shared.Options.File (maxImageSize)
import Shared.Newtype as SN
import Shared.Unsafe as SU

foreign import sanitize :: String -> String

suggest :: PrimaryKey -> ServerEffect (Array Suggestion)
suggest = SN.unwrapAll <<< SID.suggest

listContacts :: PrimaryKey -> Int -> ServerEffect (Array Contact)
listContacts loggedUserID page = do
      contacts <- SN.unwrapAll $ SID.presentContacts loggedUserID page
      history <- SN.unwrapAll <<< SID.chatHistoryFor loggedUserID $ map (_.id <<< _.user) contacts
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure $ intoContacts userHistory <$> contacts

      where intoHashMap hashMap m@{ sender, recipient } =
                  DH.insertWith (<>) (if sender == loggedUserID then recipient else sender) [m] hashMap

            intoContacts userHistory contact@{ user: { id: loggedUserID } } = contact {
                  history = SU.fromJust $ DH.lookup loggedUserID userHistory
            }

listSingleContact :: PrimaryKey -> PrimaryKey -> ServerEffect Contact
listSingleContact loggedUserID userID = do
      contact <- DN.unwrap <$> SID.presentSingleContact loggedUserID userID
      history <- SN.unwrapAll $ SID.chatHistoryBetween loggedUserID userID 0
      pure $ contact {
            history = history
      }

processMessage :: forall r. PrimaryKey -> PrimaryKey -> MessageContent -> BaseEffect { pool :: Pool | r } (Tuple PrimaryKey String)
processMessage loggedUserID userID content = do
      message <- case content of
            Text m -> pure m
            Image (Tuple caption base64) -> do
                  path <- base64From $ DS.split (Pattern ",") base64
                  pure $ "![" <> caption  <> "](" <> path <> ")"
      id <- SID.insertMessage loggedUserID userID $ sanitize message
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
                                          R.liftEffect $ NFS.writeFile ("src/Client/media/upload/" <> fileName) buffer

                                          pure $ "/client/media/upload/" <> fileName
                               else
                                    invalidImage
                        _ -> invalidImage
            invalidImage = SR.throwBadRequest "Invalid image"

processKarma :: forall r. PrimaryKey -> PrimaryKey -> Turn -> BaseEffect { pool :: Pool | r } Unit
processKarma loggedUserID userID turn = SID.insertKarma loggedUserID userID $ SW.karmaFrom turn

blockUser :: PrimaryKey -> PrimaryKey -> ServerEffect Ok
blockUser loggedUserID userID = do
      SID.insertBlock loggedUserID userID
      pure ok

listMissedContacts :: PrimaryKey -> DateTime -> ServerEffect (Array Contact)
listMissedContacts loggedUserID since = do
      history <- SN.unwrapAll $ SID.chatHistorySince loggedUserID since
      contacts <- SN.unwrapAll <<< SID.presentSelectedContacts <<< DA.nubEq $ map _.sender history
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure $ intoContacts userHistory <$> contacts

      where intoHashMap hashMap m@{ recipient } = DH.insertWith (<>) recipient [m] hashMap
            intoContacts userHistory contact@{ user: { id } } = contact {
                  history = SU.fromJust $ DH.lookup loggedUserID userHistory
            }

resumeChatHistory :: PrimaryKey -> PrimaryKey -> Int -> ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserID userID skip = SN.unwrapAll $ SID.chatHistoryBetween loggedUserID userID skip