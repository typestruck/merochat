module Test.Server.IM.Action where

import Debug
import Droplet.Language
import Prelude
import Server.Database.Blocks
import Server.Database.Fields
import Server.Database.Histories
import Shared.ContentType
import Shared.IM.Types

import Data.Array as DA
import Data.BigInt as BI
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.String.Regex as DSR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSRU
import Data.Tuple (Tuple(..))
import Run as R
import Server.Database as SD
import Server.File (imageTooBigMessage, invalidImageMessage)
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.Landing.Database as SLD
import Server.Settings.Action as SSA
import Shared.Options.File (maxImageSize)
import Shared.Unsafe ((!@))
import Shared.User (ProfileVisibility(..))
import Test.Server as TS
import Test.Server.Model (baseUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Type.Proxy (Proxy(..))

tests ∷ TestSuite
tests = do
      TU.suiteOnly "im actions" do
            TU.test "suggest filters out blocked users"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          suggestions ← SIA.suggest userId 0 Nothing
                          R.liftAff <<< TUA.equal 1 $ DA.length suggestions

                          void $ SIA.blockUser userId anotherUserId
                          suggestions' ← SIA.suggest userId 0 Nothing
                          R.liftAff <<< TUA.equal 0 $ DA.length suggestions'

            TU.test "suggest respects visibility hidden visibility"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Nobody, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          suggestions ← SIA.suggest userId 0 Nothing
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "suggest respects visibility contacts only visibility"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Contacts, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          suggestions ← SIA.suggest userId 0 Nothing
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "suggest includes all users if impersonating"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          Tuple id _ ← SIA.processMessage userId anotherUserId 2 $ Text "oi"
                          suggestions ← SIA.suggest userId 0 <<< Just $ ArrayPrimaryKey []
                          R.liftAff <<< TUA.equal 1 $ DA.length suggestions

            TU.test "suggest avoids given users if impersonating"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          suggestions ← SIA.suggest userId 0 <<< Just $ ArrayPrimaryKey [ anotherUserId ]
                          R.liftAff <<< TUA.equal 0 $ DA.length suggestions

            TU.test "listContacts orders contacts by date of last message"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = "d@d.com" }
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "ola"
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "hey"
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 2 $ DA.length contacts
                          R.liftAff $ TUA.equal yetAnotherUserId (contacts !@ 0).user.id
                          R.liftAff $ TUA.equal anotherUserId (contacts !@ 1).user.id

            TU.test "listContacts fetches only last message if all read"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage anotherUserId userId 1 $ Text "1"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "4"
                          SID.changeStatus userId Read [ 1, 2, 3, 4 ]
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history
                          R.liftAff $ TUA.equal "4" ((contacts !@ 0).history !@ 0).content

            TU.test "listContacts fetches all unread messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage anotherUserId userId 1 $ Text "1"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "4"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "5"
                          SID.changeStatus userId Read [ 1 ]
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "2", "3", "4", "5" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts ignores deleted chat history for sender"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "2"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 2 }
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 0 $ DA.length contacts

            TU.test "listContacts ignores deleted chat history for recipient"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "2"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          contacts ← SIA.listContacts anotherUserId 0
                          R.liftAff <<< TUA.equal 0 $ DA.length contacts

            TU.test "listContacts ignores deleted messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "2"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 1 }
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "2" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts respects delete column owner"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "2"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "1", "2" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts groups messages by contact"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = "d@d.com" }
                          void <<< SIA.processMessage anotherUserId userId 1 $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "4"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "5"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "a"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "b"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "c"
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 2 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "a", "b", "c" ] $ map _.content (contacts !@ 0).history
                          R.liftAff <<< TUA.equal [ "1", "2", "3", "4", "5" ] $ map _.content (contacts !@ 1).history

            TU.test "listContacts paginates by contact"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = "d@d.com" }
                          void <<< SIA.processMessage anotherUserId userId 1 $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "4"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "5"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "a"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "b"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "c"
                          contacts ← SIA.listContacts userId 1
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          --sort is by last_message_date
                          R.liftAff <<< TUA.equal [ "1", "2", "3", "4", "5" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts paginates over contacts with deleted chat histories for sender"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser baseUser { email = "d@d.com" }
                          lastUserId ← SLD.createUser baseUser { email = "e@e.com" }
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "aaaaa"
                          void <<< SIA.processMessage userId yetAnotherUserId 1 $ Text "I"
                          void <<< SIA.processMessage userId lastUserId 1 $ Text "1"
                          void $ SIA.deleteChat userId { userId: yetAnotherUserId, messageId: 2 }
                          contacts ← SID.presentNContacts userId 1 1
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "aaaaa" ] $ map _.content contacts

            TU.test "listContacts paginates over contacts with deleted chat histories for recipient"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser baseUser { email = "d@d.com" }
                          lastUserId ← SLD.createUser baseUser { email = "e@e.com" }
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "aaaaa"
                          void <<< SIA.processMessage yetAnotherUserId userId 1 $ Text "I"
                          void <<< SIA.processMessage userId lastUserId 1 $ Text "1"
                          void $ SIA.deleteChat userId { userId: yetAnotherUserId, messageId: 2 }
                          contacts ← SID.presentNContacts userId 1 1
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "aaaaa" ] $ map _.content contacts

            TU.test "listSingleContact returns chat history"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "ola"
                          contacts ← SIA.listSingleContact userId anotherUserId false
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff $ TUA.equal anotherUserId (contacts !@ 0).user.id
                          R.liftAff <<< TUA.equal [ "oi", "ola" ] $ map _.content (contacts !@ 0).history

            TU.test "listSingleContact ignores deleted messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "ola"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 1 }
                          contacts ← SIA.listSingleContact userId anotherUserId false
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "ola" ] $ map _.content (contacts !@ 0).history

            TU.test "listSingleContact respects delete column owner"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "ola"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          contacts ← SIA.listSingleContact userId anotherUserId false
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "oi", "ola" ] $ map _.content (contacts !@ 0).history

            TU.test "listMissedEvents finds missed contacts"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = "d@d.com" }
                          void <<< SIA.processMessage anotherUserId userId 1 $ Text "oi"
                          void <<< SIA.processMessage yetAnotherUserId userId 2 $ Text "ola"
                          void <<< SIA.processMessage yetAnotherUserId userId 3 $ Text "hey"
                          { contacts } ← SIA.listMissedEvents userId Nothing $ Just 0
                          R.liftAff <<< TUA.equal 2 $ DA.length contacts
                          R.liftAff $ TUA.equal yetAnotherUserId (contacts !@ 0).user.id
                          R.liftAff <<< TUA.equal [ "ola", "hey" ] $ map _.content (contacts !@ 0).history
                          R.liftAff $ TUA.equal anotherUserId (contacts !@ 1).user.id
                          R.liftAff <<< TUA.equal [ "oi" ] $ map _.content (contacts !@ 1).history

            TU.test "listMissedEvents ignores delivered messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = "d@d.com" }
                          Tuple id _ ← SIA.processMessage anotherUserId userId 1 $ Text "oi"
                          Tuple anotherID _ ← SIA.processMessage yetAnotherUserId userId 2 $ Text "ola"
                          void <<< SIA.processMessage yetAnotherUserId userId 3 $ Text "hey"
                          SID.changeStatus userId Delivered [ id, anotherID ]
                          { contacts } ← SIA.listMissedEvents userId Nothing $ Just 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff $ TUA.equal yetAnotherUserId (contacts !@ 0).user.id
                          R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history

            TU.test "listMissedEvents finds temporary ids"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = "d@d.com" }
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId yetAnotherUserId 2 $ Text "ola"
                          void <<< SIA.processMessage userId yetAnotherUserId 3 $ Text "hey"
                          { messageIds } ← SIA.listMissedEvents userId (Just 0) Nothing
                          R.liftAff <<< TUA.equal [ 1, 2, 3 ] $ map _.temporaryId messageIds

            TU.test "resumeChat paginates chat history"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "ola"
                          messages ← SIA.resumeChatHistory userId anotherUserId 1
                          R.liftAff <<< TUA.equal 1 $ DA.length messages
                          R.liftAff $ TUA.equal userId (messages !@ 0).sender
                          R.liftAff <<< TUA.equal [ "oi" ] $ map _.content messages

            TU.test "resumeChat ignores deleted messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "ola"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "hey"
                          void <<< SIA.processMessage anotherUserId userId 2 $ Text "u"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 2 }
                          messages ← SIA.resumeChatHistory userId anotherUserId 1
                          R.liftAff <<< TUA.equal [ "hey" ] $ map _.content messages

            TU.test "resumeChat respects delete column owner"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId 1 $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId 2 $ Text "ola"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          messages ← SIA.resumeChatHistory userId anotherUserId 1
                          R.liftAff <<< TUA.equal [ "oi" ] $ map _.content messages

            TU.test "report also blocks user"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void $ SIA.reportUser userId { userId: anotherUserId, reason: Spam, comment: Nothing }
                          count ← SD.single $ select (count _id # as c) # from blocks # wher (_blocker .=. userId .&&. _blocked .=. anotherUserId)
                          R.liftAff $ TUA.equal (Just { c: BI.fromInt 1 }) count

            TU.test "processMessage creates history"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          Tuple id _ ← SIA.processMessage userId anotherUserId 2 $ Text "oi"
                          R.liftAff $ TUA.equal userId id
                          count ← SD.single $ select (count _id # as c) # from histories # wher (_sender .=. userId .&&. _recipient .=. anotherUserId)
                          R.liftAff $ TUA.equal (Just { c: BI.fromInt 1 }) count

            TU.test "processMessage sets chat starter"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          Tuple id _ ← SIA.processMessage userId anotherUserId 2 $ Text "oi"
                          R.liftAff $ TUA.equal userId id
                          chatStarter ← SD.single $ select _sender # from histories # orderBy _id # limit (Proxy :: _ 1)
                          R.liftAff $ TUA.equal (Just { sender: userId }) chatStarter

            TU.test "processMessage accepts files"
                  $ TS.serverActionCatch (TS.catch invalidImageMessage)
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          Tuple _ message ← SIA.processMessage userId anotherUserId 2 $ Image "hey" "data:image/png;base64,ya"
                          R.liftAff <<< TUA.assert "returns file" $ DSR.test (DSRU.unsafeRegex "!\\[hey\\]((.*)/upload/(.*).png)" noFlags) message

            TU.test "processMessage sanitizes input"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          Tuple _ message ← SIA.processMessage userId anotherUserId 2 $ Text "<script><script>"
                          R.liftAff $ TUA.equal "" message

            TU.test "processMessage does not accept files too large"
                  $ TS.serverActionCatch (TS.catch imageTooBigMessage)
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SIA.processMessage userId anotherUserId 2 <<< Image "hey" $ "data:image/png;base64," <> (DS.joinWith "" $ DA.replicate (maxImageSize * 10) "a")

      where
      setUpUsers = do
            userId ← SLD.createUser $ baseUser { email = "b@b.com" }
            anotherUserId ← SLD.createUser $ baseUser { email = "c@c.com" }
            pure $ Tuple userId anotherUserId