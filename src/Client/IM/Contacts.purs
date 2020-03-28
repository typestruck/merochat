module Client.IM.Contacts where

import Prelude
import Shared.Types
import Debug.Trace
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Flame (World)
import Shared.Newtype as SN
import Data.Array as DA

update :: World IMModel IMMessage -> IMModel -> ContactMessage -> Aff IMModel
update _ model =
        case _ of
                ResumeChat id -> resumeChat model id

resumeChat :: IMModel -> PrimaryKey -> Aff IMModel
resumeChat model@(IMModel {contacts}) searchID =
        pure <<< SN.updateModel model $ _ {
                suggesting = Nothing,
                chatting = DA.findIndex (\(IMUser {id}) -> searchID == id) contacts
        }
