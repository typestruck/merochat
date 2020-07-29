-- | This module takes care of websocket plus chat editor events.
module Client.IM.Chat where

import Debug.Trace
import Prelude
import Shared.IM.Types
import Shared.Types

import Client.Common.DOM as CCD
import Client.Common.Network as CCNT
import Client.IM.Contacts as CICN
import Client.IM.Flame (NextMessage, NoMessages, MoreMessages)
import Client.IM.Flame as CIF
import Client.IM.Scroll as CIS
import Client.IM.WebSocket as CIW
import Data.Array ((:), (!!))
import Data.Array as DA
import Data.DateTime as DT
import Data.Either (Either(..))
import Data.Either as DET
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.Newtype as DN
import Data.String.CodeUnits as DSC
import Data.Time.Duration (Seconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Effect.Now as EN
import Flame (ListUpdate, (:>))
import Flame as F
import Shared.IM.Contact as SIC
import Shared.Newtype as SN
import Shared.PrimaryKey as SP
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU

--purty is fucking terrible

startChat :: IMModel -> String -> NextMessage
startChat model@(IMModel {
      chatting
      , user: IMUser { id }
      , contacts
      , suggesting
      , suggestions
}) content = snocContact :> [ nextSendMessage ]
      where snocContact = case Tuple chatting suggesting of
                  Tuple Nothing (Just index) ->
                        let
                        chatted = suggestions !@ index
                        in
                        SN.updateModel model
                              $ _
                              { chatting = Just 0
                              , suggesting = Nothing
                              , contacts = DA.cons (SIC.defaultContact id chatted) contacts
                              , suggestions = SU.fromJust "startChat" $ DA.deleteAt index suggestions
                              }
                  _ -> model

            nextSendMessage = do
                  date <- liftEffect $ map MDateTime EN.nowDateTime
                  CIF.next $ SendMessage content date

sendMessage :: String -> MDateTime -> IMModel -> NoMessages
sendMessage content date = case _ of
      model@(IMModel {
            user: IMUser { id: senderID },
            webSocket: Just (WS webSocket),
            token: Just token,
            chatting: Just chatting,
            temporaryID,
            contacts
      }) ->
            let  recipient@(Contact { user: IMUser { id: recipientID }, history }) = contacts !@ chatting
                 newTemporaryID = temporaryID + SP.fromInt 1
                 updatedChatting = SN.updateContact recipient $ _ {
                        history = DA.snoc history $ HistoryMessage {
                              id: newTemporaryID,
                              status: Unread,
                              sender: senderID,
                              recipient: recipientID,
                              date,
                              content
                        }
                 }
                 updatedModel = SN.updateModel model $ _ {
                        temporaryID = newTemporaryID,
                        contacts = SU.fromJust "sendMessage" $ DA.updateAt chatting updatedChatting contacts
                 }
                 turn = makeTurn updatedChatting senderID
            in --needs to handle failure!
                  CIF.nothingNext updatedModel $ liftEffect do
                        CIS.scrollLastMessage
                        CIW.sendPayload webSocket $ ServerMessage {
                              id: newTemporaryID,
                              user: recipientID,
                              token: token,
                              content,
                              turn
                        }
      model -> CIF.nothingNext model <<< liftEffect $ EC.log "Invalid sendMessage state"

makeTurn :: Contact -> PrimaryKey -> Maybe Turn
makeTurn (Contact { chatStarter, chatAge, history }) sender =
      if chatStarter == sender && isNewTurn history sender then
            let senderEntry = SU.fromJust "makeTurn" $ DA.last history
                recipientEntry = SU.fromJust "makeTurn" $ history !! (DA.length history - 2)
                Tuple senderMessages recipientMessages = SU.fromJust "makeTurn" do
                      let groups = DA.groupBy sameSender history
                          size = DA.length groups
                      senderMessages <- groups !! (size - 3)
                      recipientMessages <- groups !! (size - 2)
                      pure $ Tuple senderMessages recipientMessages
                senderCharacters = DI.toNumber $ DA.foldl countCharacters 0 senderMessages
                recipientCharacters = DI.toNumber $ DA.foldl countCharacters 0 recipientMessages
            in Just $ Turn {
                  senderStats: Stats {
                        characters: senderCharacters,
                        interest: senderCharacters / recipientCharacters
                  },
                  recipientStats: Stats {
                        characters: recipientCharacters,
                        interest: recipientCharacters / senderCharacters
                  },
                  replyDelay: DN.unwrap (DT.diff (getDate senderEntry) $ getDate recipientEntry :: Seconds),
                  chatAge
            }
       else Nothing
      where isNewTurn history userID = DM.fromMaybe false do
                  last <- DA.last history
                  beforeLast <- history !! (DA.length history - 2)
                  let sender = _.sender $ DN.unwrap last
                      recipient = _.recipient $ DN.unwrap beforeLast
                  pure $ sender == userID && recipient == userID

            sameSender entry anotherEntry = _.sender (DN.unwrap entry) == _.sender (DN.unwrap anotherEntry)
            countCharacters total (HistoryMessage { content }) = total + DSC.length content
            getDate = DN.unwrap <<< _.date <<< DN.unwrap

receiveMessage :: Boolean -> IMModel -> WebSocketPayloadClient -> MoreMessages
receiveMessage isFocused model@(IMModel {
      user: IMUser { id: recipientID },
      contacts,
      suggestions
}) = case _ of
      Received { previousID, id } ->
            F.noMessages <<< SN.updateModel model $ _ {
                  contacts = DM.fromMaybe contacts $ updateTemporaryID contacts previousID id
            }
      ClientMessage m@{ user } -> case processIncomingMessage m model of
            Left userID -> model :> [Just <<< DisplayContacts <$> CCNT.get' (SingleContact {id: userID})]
            Right updatedModel@(IMModel {
                  token: Just tk
                  , webSocket: Just (WS ws)
                  , chatting: Just index
                  , contacts
            }) ->  --mark it as read if we received a message from the current chat
                  let fields = {
                        token: tk
                        , webSocket: ws
                        , chatting: index
                        , userID: recipientID
                        , contacts
                  }
                  in if isFocused && isChatting user fields then
                        CICN.updateReadHistory updatedModel fields
                      else
                        F.noMessages updatedModel
            Right updatedModel -> F.noMessages updatedModel
      where isChatting sender { contacts, chatting } =
                  let (Contact { user: IMUser { id: recipientID } }) = contacts !@ chatting in
                  recipientID == DET.either (_.id <<< DN.unwrap) identity sender

--this must be simplified to
-- if contacts present in histories must always come Left User
processIncomingMessage :: _ -> IMModel -> Either PrimaryKey IMModel
processIncomingMessage m model@(IMModel {
      user: IMUser { id: recipientID },
      suggestions,
      contacts,
      suggesting,
      chatting
}) = case updateContactList contacts recipientID m of
      New contacts' ->
            --new messages bubble the contact to the top
            let added = DA.head contacts' in Right $
                  if getUserID (map (_.user <<< DN.unwrap) added) == getUserID suggestingContact then
                        --edge case of receiving a message from a suggestion
                        SN.updateModel model $ _ {
                              contacts = contacts'
                              , suggesting = Nothing
                              , suggestions = SU.fromJust "delete receiveMesage" do
                                    index <- suggesting
                                    DA.deleteAt index suggestions
                              , chatting = Just 0
                        }
                   else
                        SN.updateModel model $ _ {
                              contacts = contacts',
                              --since the contact list is altered, the chatting index must be bumped
                              chatting = (_ + 1) <$> chatting
                        }
      Existing contacts' ->
            Right <<< SN.updateModel model $ _ { contacts = contacts' }
      ExistingToFetch userID -> Left userID

      where suggestingContact = do
                  index <- suggesting
                  suggestions !! index
            getUserID :: forall n a. Newtype n { id :: PrimaryKey | a } => Maybe n -> Maybe PrimaryKey
            getUserID = map (_.id <<< DN.unwrap)

updateContactList :: Array Contact -> PrimaryKey -> {
      id :: PrimaryKey
      , user :: Either IMUser PrimaryKey
      , date :: MDateTime
      , content :: String
} -> (ReceivedUser (Array Contact) PrimaryKey)
updateContactList contacts recipientID { id, user, date, content } = case user of
      Right userID -> case updateExisting userID of
            Nothing -> ExistingToFetch userID -- edge case of an existing contact not be yet visible on the contact list due to pagination
            Just existing -> existing
      Left user@(IMUser { id: userID }) -> New (updateHistory { userID, content, id, date } (SIC.defaultContact userID user) : contacts)

      where findUser userID (Contact { user: IMUser { id } }) = userID == id
            updateHistory { id, userID, content, date } user@(Contact { history }) =
                  SN.updateContact user $ _ {
                        history = DA.snoc history $ HistoryMessage {
                              status: Unread
                              , sender: userID
                              , recipient: recipientID
                              , id
                              , content
                              , date
                        }
                  }

            updateExisting userID = do
                  index <- DA.findIndex (findUser userID) contacts
                  Contact { history } <- contacts !! index
                  map Existing $ DA.modifyAt index (updateHistory { userID, content, id, date }) contacts

updateTemporaryID :: Array Contact -> PrimaryKey -> PrimaryKey -> Maybe (Array Contact)
updateTemporaryID contacts previousID id = do
      index <- DA.findIndex (findUser previousID) contacts
      Contact { history } <- contacts !! index
      innerIndex <- DA.findIndex (findTemporary previousID) history
      DA.modifyAt index (updateTemporary innerIndex id) contacts

      where findTemporary previousID (HistoryMessage { id }) = id == previousID
            findUser previousID (Contact { history }) = DA.any (findTemporary previousID) history
            updateTemporary index newID user@(Contact { history }) =
                  SN.updateContact user $ _ {
                        history = SU.fromJust "receiveMessage" $ DA.modifyAt index (flip SN.updateHistoryMessage (_ { id = newID })) history
                  }
