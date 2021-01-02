module Server.IM.Action where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Foldable as DF
import Data.Foldable as FD
import Data.HashMap as DH
import Data.Maybe (Maybe)
import Data.Maybe as DM
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
import Server.IM.Database as SID
import Server.Ok (ok)
import Server.Response as SR
import Server.Wheel as SW
import Shared.Newtype as SN
import Shared.Options.File (allowedMediaTypes, maxImageSize)
import Shared.Unsafe as SU

foreign import sanitize :: String -> String

suggest :: PrimaryKey -> Int -> ServerEffect (Array Suggestion)
suggest loggedUserID = SN.unwrapAll <<< SID.suggest loggedUserID

listContacts :: PrimaryKey -> Int -> ServerEffect (Array Contact)
listContacts loggedUserID skip = do
      contacts <- SN.unwrapAll $ SID.presentContacts loggedUserID skip
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

processMessage :: forall r. PrimaryKey -> PrimaryKey -> Int -> MessageContent -> BaseEffect { pool :: Pool | r } (Tuple PrimaryKey String)
processMessage loggedUserID userID temporaryID content = do
      message <- case content of
            Text m -> pure m
            Image (Tuple caption base64) -> do
                  path <- base64From $ DS.split (Pattern ",") base64
                  pure $ "![" <> caption  <> "](" <> path <> ")"
      let sanitized = DS.trim $ sanitize message
      id <- SID.insertMessage loggedUserID userID temporaryID sanitized
      pure $ Tuple id sanitized
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

listMissedEvents :: PrimaryKey -> Maybe Int -> Maybe Int -> ServerEffect MissedEvents
listMissedEvents loggedUserID lastSenderID lastRecipientID = do
      messageIDs <- SN.unwrapAll $ DM.maybe (pure []) (SID.messsageIDsFor loggedUserID) lastSenderID
      history <- SN.unwrapAll $ DM.maybe (pure []) (SID.chatHistorySince loggedUserID) lastRecipientID
      contacts <- SN.unwrapAll <<< SID.presentSelectedContacts loggedUserID <<< DA.nubEq $ map _.sender history
      let userHistory = DF.foldl intoHashMap DH.empty history
      pure {
            contacts: intoContacts userHistory <$> contacts,
            messageIDs
      }

      where intoHashMap hashMap m@{ recipient } = DH.insertWith (<>) recipient [m] hashMap
            intoContacts userHistory contact@{ user: { id } } = contact {
                  history = SU.fromJust $ DH.lookup loggedUserID userHistory
            }

resumeChatHistory :: PrimaryKey -> PrimaryKey -> Int -> ServerEffect (Array HistoryMessage)
resumeChatHistory loggedUserID userID skip = SN.unwrapAll $ SID.chatHistoryBetween loggedUserID userID skip