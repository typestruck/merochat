module Test.Server.Im.Action where

import Debug
import Droplet.Language
import Prelude
import Server.Database.Blocks
import Server.Database.Fields
import Server.Database.Histories
import Shared.Im.Types

import Data.Array as DA
import Data.BigInt as BI
import Data.Either (Either(..))
import Data.Either as DE
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Set as DST
import Data.String as DS
import Data.String.Regex as DSR
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe as DSRU
import Data.Tuple (Tuple(..))
import Run as R
import Server.Database as SD
import Server.Database.Types (Checked(..))
import Server.Database.Users (_temporary, users)
import Server.File (imageTooBigMessage, invalidImageMessage)
import Server.Im.Action as SIA
import Server.Im.Database as SID
import Server.Landing.Database as SLD
import Server.Settings.Action as SSA
import Shared.Privilege (Privilege(..))
import Shared.Resource (maxImageSize)
import Shared.Unsafe ((!@))
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility(..))
import Test.Server as TS
import Test.Server.User (baseUser)
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA
import Type.Proxy (Proxy(..))

tests ∷ TestSuite
tests = do
      TU.suite "im actions" do
            TU.test "suggest filters out blocked users"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          suggestions ← SIA.suggest userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length suggestions

                          void $ SIA.blockUser userId anotherUserId
                          suggestions' ← SIA.suggest userId 0
                          R.liftAff <<< TUA.equal 0 $ DA.length suggestions'

            TU.test "suggest respects hidden visibility"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Nobody, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          suggestions ← SIA.suggest userId 0
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "suggest respects contacts only visibility"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Contacts, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          suggestions ← SIA.suggest userId 0
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "suggest respects no temporary users only visibility"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SD.execute $ update users # set (_temporary .=. Checked true) # wher (_id .=. anotherUserId)
                          SSA.changePrivacySettings userId { profileVisibility: NoTemporaryUsers, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          suggestions ← SIA.suggest userId 0
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "suggest respects no temporary users only visibility when suggesting to temporary user"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SD.execute $ update users # set (_temporary .=. Checked true) # wher (_id .=. userId)
                          SSA.changePrivacySettings anotherUserId { profileVisibility: NoTemporaryUsers, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          suggestions ← SIA.suggest userId 0
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "suggest never shows temporary users"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SD.execute $ update users # set (_temporary .=. Checked true) # wher (_id .=. anotherUserId)
                          suggestions ← SIA.suggest userId 0
                          R.liftAff $ TUA.equal [] suggestions

            TU.test "listContacts orders contacts by date of last message"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = Just "d@d.com" }
                          void <<< SIA.processMessage userId yetAnotherUserId $ Text "ola"
                          void <<< SIA.processMessage userId anotherUserId $ Text "oi"
                          void <<< SIA.processMessage userId yetAnotherUserId $ Text "hey"
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 2 $ DA.length contacts
                          R.liftAff $ TUA.equal yetAnotherUserId (contacts !@ 0).user.id
                          R.liftAff $ TUA.equal anotherUserId (contacts !@ 1).user.id

            TU.test "listContacts fetches only last message if all read"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage anotherUserId userId $ Text "1"
                          void <<< SIA.processMessage anotherUserId userId $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId $ Text "4"
                          SID.changeStatus userId Read [ 1, 2, 3, 4 ]
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history
                          R.liftAff $ TUA.equal "4" ((contacts !@ 0).history !@ 0).content

            TU.test "listContacts fetches all unread messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage anotherUserId userId $ Text "1"
                          void <<< SIA.processMessage anotherUserId userId $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId $ Text "4"
                          void <<< SIA.processMessage anotherUserId userId $ Text "5"
                          SID.changeStatus userId Read [ 1 ]
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "2", "3", "4", "5" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts ignores deleted chat history for sender"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "2"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 2 }
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 0 $ DA.length contacts

            TU.test "listContacts ignores deleted chat history for recipient"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId $ Text "2"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          contacts ← SIA.listContacts anotherUserId 0
                          R.liftAff <<< TUA.equal 0 $ DA.length contacts

            TU.test "listContacts ignores deleted messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "2"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 1 }
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "2" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts respects delete column owner"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "2"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "1", "2" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts groups messages by contact"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = Just "d@d.com" }
                          void <<< SIA.processMessage anotherUserId userId  $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId  $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId  $ Text "4"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "5"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "a"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "b"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "c"
                          contacts ← SIA.listContacts userId 0
                          R.liftAff <<< TUA.equal 2 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "a", "b", "c" ] $ map _.content (contacts !@ 0).history
                          R.liftAff <<< TUA.equal [ "1", "2", "3", "4", "5" ] $ map _.content (contacts !@ 1).history

            TU.test "listContacts paginates by contact"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser $ baseUser { email = Just "d@d.com" }
                          void <<< SIA.processMessage anotherUserId userId  $ Text "1"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "2"
                          void <<< SIA.processMessage anotherUserId userId  $ Text "3"
                          void <<< SIA.processMessage anotherUserId userId  $ Text "4"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "5"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "a"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "b"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "c"
                          contacts ← SIA.listContacts userId 1
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          --sort is by last_message_date
                          R.liftAff <<< TUA.equal [ "1", "2", "3", "4", "5" ] $ map _.content (contacts !@ 0).history

            TU.test "listContacts paginates over contacts with deleted chat histories for sender"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser baseUser { email = Just "d@d.com" }
                          lastUserId ← SLD.createUser baseUser { email = Just "e@e.com" }
                          void <<< SIA.processMessage userId anotherUserId  $ Text "aaaaa"
                          void <<< SIA.processMessage userId yetAnotherUserId  $ Text "I"
                          void <<< SIA.processMessage userId lastUserId  $ Text "1"
                          void $ SIA.deleteChat userId { userId: yetAnotherUserId, messageId: 2 }
                          contacts ← SID.presentNContacts userId 1 1
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "aaaaa" ] $ map _.content contacts

            TU.test "listContacts paginates over contacts with deleted chat histories for recipient"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          yetAnotherUserId ← SLD.createUser baseUser { email = Just "d@d.com" }
                          lastUserId ← SLD.createUser baseUser { email = Just "e@e.com" }
                          void <<< SIA.processMessage userId anotherUserId  $ Text "aaaaa"
                          void <<< SIA.processMessage yetAnotherUserId userId  $ Text "I"
                          void <<< SIA.processMessage userId lastUserId  $ Text "1"
                          void $ SIA.deleteChat userId { userId: yetAnotherUserId, messageId: 2 }
                          contacts ← SID.presentNContacts userId 1 1
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "aaaaa" ] $ map _.content contacts

            TU.test "listSingleContact returns chat history"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "ola"
                          contacts ← SIA.listSingleContact userId anotherUserId
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff $ TUA.equal anotherUserId (contacts !@ 0).user.id
                          R.liftAff <<< TUA.equal [ "oi", "ola" ] $ map _.content (contacts !@ 0).history

            TU.test "listSingleContact ignores deleted messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "ola"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 1 }
                          contacts ← SIA.listSingleContact userId anotherUserId
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "ola" ] $ map _.content (contacts !@ 0).history

            TU.test "listSingleContact respects delete column owner"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "ola"
                          void $ SIA.deleteChat anotherUserId { userId, messageId: 2 }
                          contacts ← SIA.listSingleContact userId anotherUserId
                          R.liftAff <<< TUA.equal 1 $ DA.length contacts
                          R.liftAff <<< TUA.equal [ "oi", "ola" ] $ map _.content (contacts !@ 0).history

            TU.test "listMissedEvents finds missed contacts" $
                  TU.failure "na"
                  -- $ TS.serverAction
                  -- $ do
                  --         Tuple userId anotherUserId ← setUpUsers
                  --         yetAnotherUserId ← SLD.createUser $ baseUser { email = Just "d@d.com" }
                  --         void <<< SIA.processMessage anotherUserId userId  $ Text "oi"
                  --         void <<< SIA.processMessage yetAnotherUserId userId  $ Text "ola"
                  --         void <<< SIA.processMessage yetAnotherUserId userId  $ Text "hey"
                  --         { contacts } ← SIA.listMissedEvents userId Nothing $ Just 0
                  --         R.liftAff <<< TUA.equal 2 $ DA.length contacts
                  --         R.liftAff $ TUA.equal yetAnotherUserId (contacts !@ 0).user.id
                  --         R.liftAff <<< TUA.equal [ "ola", "hey" ] $ map _.content (contacts !@ 0).history
                  --         R.liftAff $ TUA.equal anotherUserId (contacts !@ 1).user.id
                  --         R.liftAff <<< TUA.equal [ "oi" ] $ map _.content (contacts !@ 1).history

            TU.test "listMissedEvents ignores delivered messages" $
                  TU.failure "na"
                  -- $ TS.serverAction
                  -- $ do
                  --         Tuple userId anotherUserId ← setUpUsers
                  --         yetAnotherUserId ← SLD.createUser $ baseUser { email = Just "d@d.com" }
                  --         Tuple id _ ← map SU.fromRight <<< SIA.processMessage anotherUserId userId  $ Text "oi"
                  --         Tuple anotherId _ ← map SU.fromRight <<< SIA.processMessage yetAnotherUserId userId $ Text "ola"
                  --         void <<< SIA.processMessage yetAnotherUserId userId  $ Text "hey"
                  --         SID.changeStatus userId Delivered [ id, anotherId ]
                  --         { contacts } ← SIA.listMissedEvents userId Nothing $ Just 0
                  --         R.liftAff <<< TUA.equal 1 $ DA.length contacts
                  --         R.liftAff $ TUA.equal yetAnotherUserId (contacts !@ 0).user.id
                  --         R.liftAff <<< TUA.equal 1 $ DA.length (contacts !@ 0).history

            TU.test "listMissedEvents finds temporary ids" $
                  TU.failure "na"
                  -- $ TS.serverAction
                  -- $ do
                  --         Tuple userId anotherUserId ← setUpUsers
                  --         yetAnotherUserId ← SLD.createUser $ baseUser { email = Just "d@d.com" }
                  --         void <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                  --         void <<< SIA.processMessage userId yetAnotherUserId  $ Text "ola"
                  --         void <<< SIA.processMessage userId yetAnotherUserId  $ Text "hey"
                  --         { messageIds } ← SIA.listMissedEvents userId (Just 0) Nothing
                  --         R.liftAff <<< TUA.equal [ 1, 2, 3 ] $ map _.temporaryId messageIds

            TU.test "resumeChat paginates chat history"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId $ Text "ola"
                          messages ← SIA.resumeChatHistory userId anotherUserId 1
                          R.liftAff <<< TUA.equal 1 $ DA.length messages
                          R.liftAff $ TUA.equal userId (messages !@ 0).sender
                          R.liftAff <<< TUA.equal [ "oi" ] $ map _.content messages

            TU.test "resumeChat ignores deleted messages"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId $ Text "ola"
                          void <<< SIA.processMessage userId anotherUserId $ Text "hey"
                          void <<< SIA.processMessage anotherUserId userId $ Text "u"
                          void $ SIA.deleteChat userId { userId: anotherUserId, messageId: 2 }
                          messages ← SIA.resumeChatHistory userId anotherUserId 1
                          R.liftAff <<< TUA.equal [ "hey" ] $ map _.content messages

            TU.test "resumeChat respects delete column owner"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          void <<< SIA.processMessage userId anotherUserId  $ Text "ola"
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
                          Tuple id _ ← map SU.fromRight <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff $ TUA.equal userId id
                          count ← SD.single $ select (count _id # as c) # from histories # wher (_sender .=. userId .&&. _recipient .=. anotherUserId)
                          R.liftAff $ TUA.equal (Just { c: BI.fromInt 1 }) count

            TU.test "processMessage sets chat starter"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          Tuple id _ ← map SU.fromRight <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff $ TUA.equal userId id
                          chatStarter ← SD.single $ select _sender # from histories # orderBy _id # limit (Proxy ∷ _ 1)
                          R.liftAff $ TUA.equal (Just { sender: userId }) chatStarter

            TU.test "processMessage accepts links"
                  $ TS.serverAction
                  $ do
                          message ← SIA.processMessageContent (Text "[hey](http://a.com)") $ DST.singleton SendLinks
                          R.liftAff $ TUA.equal "[hey](http://a.com)" message

            TU.test "processMessage accepts links if url is text"
                  $ TS.serverAction
                  $ do
                          message ← SIA.processMessageContent (Text "http://a.com") DST.empty
                          R.liftAff $ TUA.equal "http://a.com" message

            TU.test "processMessage accepts files"
                  $ TS.serverActionCatch (TS.catch invalidImageMessage)
                  $ do
                          message ← SIA.processMessageContent (Image "hey" "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7") $ DST.singleton SendImages
                          R.liftAff <<< TUA.assert "returns file" $ DSR.test (DSRU.unsafeRegex "!\\[hey\\]((.*)/upload/(.*).gif)" noFlags) message

            TU.test "processMessage sanitizes input"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          ret ← SIA.processMessage userId anotherUserId  $ Text "<img/>"
                          R.liftAff $ TUA.equal (Left InvalidMessage) ret

            TU.test "processMessage does not accept files too large"
                  $ TS.serverActionCatch (TS.catch imageTooBigMessage)
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SIA.processMessage userId anotherUserId  <<< Image "hey" $ "data:image/png;base64," <> (DS.joinWith "" $ DA.replicate (maxImageSize * 10) "a")

            TU.test "processMessage rejects links if user lacks privilege"
                  $ TS.serverAction
                  $ do
                          message ← SIA.processMessageContent (Text "[hey](http://a.com)") DST.empty
                          R.liftAff $ TUA.equal "" message

            TU.test "processMessage rejects images if user lacks privilege"
                  $ TS.serverAction
                  $ do
                          message ← SIA.processMessageContent (Image "hey" "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7") DST.empty
                          R.liftAff $ TUA.equal "" message

            TU.test "processMessage fails if recipient visibility is nobody"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Nobody, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          processed ← SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff $ TUA.equal (Left UserUnavailable) processed

            TU.test "processMessage fails if recipient blocked sender"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SIA.blockUser anotherUserId userId
                          processed ← SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff $ TUA.equal (Left UserUnavailable) processed

            TU.test "processMessage fails if recipient visibility is contacts and sender is not in contacts"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Contacts, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          processed ← SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff $ TUA.equal (Left UserUnavailable) processed

            TU.test "processMessage fails if recipient visibility is no temporary users and sender is a temporary user"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SD.execute $ update users # set (_temporary .=. Checked true) # wher (_id .=. userId)
                          SSA.changePrivacySettings anotherUserId { profileVisibility: NoTemporaryUsers, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          processed ← SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff $ TUA.equal (Left UserUnavailable) processed

            TU.test "processMessage does not fail if recipient visibility is no temporary users and sender is not a temporary user"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          SSA.changePrivacySettings anotherUserId { profileVisibility: NoTemporaryUsers, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          processed ← SIA.processMessage userId anotherUserId  $ Text "oi"
                          R.liftAff <<< TUA.assert "is right" $ DE.isRight processed

            TU.test "processMessage does not fail if recipient visibility is contacts and sender is in contacts"
                  $ TS.serverAction
                  $ do
                          Tuple userId anotherUserId ← setUpUsers
                          void <<< SIA.processMessage userId anotherUserId  $ Text "oi"
                          SSA.changePrivacySettings anotherUserId { profileVisibility: Contacts, onlineStatus: true, typingStatus: true, messageTimestamps: true, readReceipts: true }
                          processed ← SIA.processMessage userId anotherUserId  $ Text "ola"
                          R.liftAff <<< TUA.assert "is right" $ DE.isRight processed
      where
      setUpUsers = do
            userId ← SLD.createUser $ baseUser { email = Just "b@b.com" }
            anotherUserId ← SLD.createUser $ baseUser { email = Just "c@c.com" }
            pure $ Tuple userId anotherUserId