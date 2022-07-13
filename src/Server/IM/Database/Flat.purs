module Server.IM.Database.Flat where

import Prelude
import Shared.User

import Data.DateTime (DateTime)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Safe.Coerce as SC
import Server.Database.Flat as SDF
import Server.Database.Types (Checked(..))
import Shared.Avatar as SA
import Shared.DateTime (DateTimeWrapper(..))
import Shared.IM.Types (Contact, HM, ImUser, HistoryMessage)
import Shared.Unsafe as SU

type FlatFields rest =
      { age ∷ Maybe Number
      , avatar ∷ Maybe String
      , country ∷ Maybe String
      , description ∷ String
      , gender ∷ Maybe Gender
      , headline ∷ String
      , id ∷ Int
      , karma ∷ Int
      , karmaPosition ∷ Int
      , languages ∷ Maybe String
      , profileVisibility ∷ ProfileVisibility
      , readReceipts ∷ Checked
      , messageTimestamps ∷ Checked
      , typingStatus ∷ Checked
      , onlineStatus ∷ Checked
      , name ∷ String
      , tags ∷ Maybe String
      | rest
      }

type FlatUser = FlatFields ()

type FlatC r = FlatFields
      ( chatAge ∷ Maybe Number
      , chatStarter ∷ Maybe Int
      , lastMessageDate ∷ DateTime
      | r
      )

type FlatContact = FlatC ()

type FlatContactHistoryMessage = FlatC (HM (messageId ∷ Int))

fromFlatContact ∷ forall r . FlatC r → Contact
fromFlatContact fc =
      { shouldFetchChatHistory: true
      , chatAge: DM.fromMaybe 0.0 fc.chatAge
      , chatStarter: SU.fromJust fc.chatStarter
      , lastMessageDate : DateTimeWrapper fc.lastMessageDate
      , impersonating: Nothing
      , history: []
      , user: fromFlatUser fc
      , typing: false
      }

fromFlatUser ∷ ∀ r. FlatFields r → ImUser
fromFlatUser fc =
      { id: fc.id
      , name: fc.name
      , headline: fc.headline
      , profileVisibility: fc.profileVisibility
      , readReceipts: SC.coerce fc.readReceipts
      , messageTimestamps: SC.coerce fc.messageTimestamps
      , typingStatus: SC.coerce fc.typingStatus
      , onlineStatus: SC.coerce fc.onlineStatus
      , availability: None
      , description: fc.description
      , avatar: SA.parseAvatar fc.avatar
      , tags: SDF.splitAgg "\\n" fc.tags
      , karma: fc.karma
      , karmaPosition: fc.karmaPosition
      , gender: show <$> fc.gender
      , country: fc.country
      , languages: SDF.splitAgg "," fc.languages
      , age: DI.ceil <$> fc.age
      }

fromFlatMessage ∷ FlatContactHistoryMessage → HistoryMessage
fromFlatMessage fm =
      { id: fm.messageId
      , sender: fm.sender
      , recipient: fm.recipient
      , date: fm.date
      , content: fm.content
      , status: fm.status
      }