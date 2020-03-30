module Client.IM.Contacts where

import Prelude
import Shared.IM.Types
import Shared.Types
import Debug.Trace
import Data.Maybe (Maybe(..))
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Effect.Class(liftEffect)
import Effect.Aff (Aff)
import Flame (World)
import Shared.Newtype as SN
import Effect.Console as EC
import Data.Newtype as DN
import Data.Array as DA
import Client.IM.WebSocketHandler (webSocketHandler)
import Client.Common.Types

update :: World IMModel IMMessage -> IMModel -> ContactMessage -> Aff IMModel
update _ model =
        case _ of
                ResumeChat id -> do
                        model' <- resumeChat id model
                        markRead webSocketHandler id model'

markRead :: WebSocketHandler -> PrimaryKey -> IMModel -> Aff IMModel
markRead websocketHandler searchID =
        case _ of
                model@(IMModel { webSocket: Just (WS ws), contacts, chatting: Just index }) -> do
                        let readContact@(IMUser { history }) = contacts !@ index
                        liftEffect <<< webSocketHandler.sendPayload ws $ ReadMessages { ids: DA.mapMaybe unreadID <<< _.history $ DN.unwrap readContact }
                        pure <<< SN.updateModel model $ _ {
                                contacts = SU.unsafeFromJust "markRead" $ DA.updateAt index (SN.updateUser readContact $ _ { history = map read history }) contacts
                        }
                model -> do
                        liftEffect $ EC.log "invalid markRead state"
                        pure model

        where   unreadID (HistoryMessage { id, status })
                        | status == Unread = Just id
                        | otherwise = Nothing

                read historyEntry@(HistoryMessage { id, status })
                        | status == Unread = SN.updateHistoryMessage historyEntry $ _ { status = Read }
                        | otherwise = historyEntry

resumeChat :: PrimaryKey -> IMModel -> Aff IMModel
resumeChat searchID model@(IMModel { contacts }) =
        pure <<< SN.updateModel model $ _ {
                suggesting = Nothing,
                chatting = DA.findIndex (\(IMUser {id}) -> searchID == id) contacts
        }


