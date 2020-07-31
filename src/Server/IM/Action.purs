module Server.IM.Action where

import Prelude
import Server.Types
import Shared.IM.Types
import Shared.Types

import Data.Foldable as DF
import Data.HashMap as DH
import Data.Newtype as DN
import Server.IM.Database as SID
import Shared.Newtype as SN
import Shared.Unsafe as SU

suggest :: PrimaryKey -> ServerEffect (Array IMUser)
suggest id = SID.suggest id

contactList :: PrimaryKey -> Int -> ServerEffect (Array Contact)
contactList id page = do
        contacts <- SID.presentContacts id page
        history <- SID.chatHistory id $ map (_.id <<< DN.unwrap <<< _.user <<< DN.unwrap) contacts
        let userHistory = DF.foldl (intoHashMap id) DH.empty history
        pure $ intoContacts userHistory <$> contacts

        where   intoHashMap userID hashMap m@(HistoryMessage {sender, recipient}) =
                        DH.insertWith (<>) (if sender == userID then recipient else sender) [m] hashMap

                intoContacts userHistory user@(Contact { user: IMUser { id } }) = SN.updateContact user $ _ {
                        history = SU.fromJust $ DH.lookup id userHistory
                }

singleContact :: PrimaryKey -> PrimaryKey -> ServerEffect Contact
singleContact id otherID = do
        contact <- SID.presentSingleContact id otherID
        history <- SID.chatHistoryBetween id otherID 0
        pure <<< SN.updateContact contact $ _ {
                history = history
        }
