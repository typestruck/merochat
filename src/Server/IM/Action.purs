module Server.IM.Action where

import Prelude
import Server.Types
import Shared.Types
import Shared.IM.Types
import Data.Foldable as DF
import Data.HashMap as DH
import Server.IM.Database as SID
import Shared.Unsafe as SU
import Shared.Newtype as SN

suggest :: PrimaryKey -> ServerEffect (Array IMUser)
suggest id = SID.suggest id

contactList :: PrimaryKey -> ServerEffect (Array IMUser)
contactList id = do
        contacts <- SID.presentContacts id
        history <- SID.chatHistory id
        let userHistory = DF.foldl (intoHashMap id) DH.empty history
        pure $ intoContacts userHistory <$> contacts

        where   intoHashMap userID hashMap m@(HistoryMessage {sender, recipient}) =
                        DH.insertWith (<>) (if sender == userID then recipient else sender) [m] hashMap

                intoContacts userHistory user@(IMUser { id }) = SN.updateUser user $ _ {
                        history = SU.unsafeFromJust "contactList" $ DH.lookup id userHistory
                }