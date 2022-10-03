module Server.Im.Database.Flat where

import Prelude
import Shared.User

import Data.DateTime (DateTime)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Debug (spy)
import Safe.Coerce as SC
import Server.Database.Types (Checked(..))
import Shared.Avatar as SA
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Im.Types (Contact, HM, ImUser, HistoryMessage)
import Shared.Privilege (Privilege)
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
      , completedTutorial ∷ Checked
      , languages ∷ Maybe (Array String)
      , profileVisibility ∷ ProfileVisibility
      , joined ∷ DateTime
      , readReceipts ∷ Checked
      , messageTimestamps ∷ Checked
      , typingStatus ∷ Checked
      , temporary ∷ Checked
      , privileges :: Maybe (Array Privilege)
      , onlineStatus ∷ Checked
      , name ∷ String
      , tags ∷ Maybe (Array String)
      | rest
      }

type FlatUser = FlatFields ()

type FlatC r = FlatFields
      ( chatAge ∷ Maybe Number
      , chatStarter ∷ Int
      , lastMessageDate ∷ DateTime
      | r
      )

type FlatContact = FlatC ()

type FlatContactHistoryMessage = FlatC (HM (messageId ∷ Int))

fromFlatContact ∷ ∀ r. FlatC r → Contact
fromFlatContact fc =
      { shouldFetchChatHistory: true
      , chatAge: DM.fromMaybe 0.0 fc.chatAge
      , chatStarter: fc.chatStarter
      , lastMessageDate: DateTimeWrapper fc.lastMessageDate
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
      , completedTutorial: SC.coerce fc.completedTutorial
      , description: fc.description
      , privileges: DM.fromMaybe [] fc.privileges
      , temporary: SC.coerce fc.temporary
      , joined: DateTimeWrapper fc.joined
      , avatar: SA.parseAvatar fc.avatar
      , tags: DM.fromMaybe [] fc.tags
      , karma: fc.karma
      , karmaPosition: fc.karmaPosition
      , gender: show <$> fc.gender
      , country: fc.country
      , languages: DM.fromMaybe [] fc.languages
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