module Server.Im.Database.Flat where

import Prelude
import Shared.Availability
import Shared.User

import Data.BigInt (BigInt)
import Data.BigInt as BI
import Data.DateTime (DateTime)
import Data.Int as DI
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Safe.Coerce as SC
import Server.Database.Types (Checked(..))
import Shared.Badge (Badge)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.Im.Types (Contact, HM, HistoryMessage)
import Shared.Privilege (Privilege)
import Shared.ProfileColumn (ProfileColumn)

type FlatFields rest =
      { age ∷ Maybe Number
      , avatar ∷ Maybe String
      , country ∷ Maybe String
      , description ∷ String
      , gender ∷ Maybe Gender
      , ownBackground ∷ Checked
      , chatBackground ∷ Maybe String
      , backer ∷ Checked
      , totalPosts ∷ Maybe BigInt
      , totalAsks ∷ Maybe BigInt
      , headline ∷ String
      , id ∷ Int
      , bin ∷ Int
      , unseenPosts ∷ Maybe BigInt
      , favorite ∷ Boolean
      , karma ∷ Int
      , karmaPosition ∷ Int
      , postsVisibility ∷ ProfileVisibility
      , isContact ∷ Boolean
      , completedTutorial ∷ Checked
      , languages ∷ Maybe (Array String)
      , profileVisibility ∷ ProfileVisibility
      , asks_visibility ∷ ProfileVisibility
      , joined ∷ DateTime
      , readReceipts ∷ Checked
      , messageTimestamps ∷ Checked
      , typingStatus ∷ Checked
      , temporary ∷ Checked
      , completedFields ∷ Maybe (Array ProfileColumn)
      , privileges ∷ Maybe (Array Privilege)
      , badges ∷ Maybe (Array Badge)
      , onlineStatus ∷ Checked
      , name ∷ String
      , tags ∷ Maybe (Array String)
      , lastSeen ∷ DateTime
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

type FlatMessage = HM (messageId ∷ Int)

type FlatContactHistoryMessage = FlatC (FlatMessage)

fromFlatContact ∷ ∀ r. FlatC r → Contact
fromFlatContact fc =
      { shouldFetchChatHistory: true
      , chatAge: DM.fromMaybe 0.0 fc.chatAge
      , chatStarter: fc.chatStarter
      , draft: ""
      , lastMessageDate: DateTimeWrapper fc.lastMessageDate
      , history: []
      , scrollChatDown: true
      , user: fromFlatUser fc
      , typing: false
      }

fromFlatUser ∷ ∀ r. FlatFields r → User
fromFlatUser fc =
      { id: fc.id
      , name: fc.name
      , headline: fc.headline
      , posts: []
      , asks: []
      , praise : []
      , praiseStatus: HasNotPraised
      , unseenPosts: DM.fromMaybe 0 (fc.unseenPosts >>= BI.toInt)
      , postsVisibility: fc.postsVisibility
      , ownBackground: SC.coerce fc.ownBackground
      , chatBackground: fc.chatBackground
      , isContact: fc.isContact
      , showing: ShowInfo
      , totalPosts: DM.fromMaybe 0 (fc.totalPosts >>= BI.toInt)
      , totalAsks: DM.fromMaybe 0 (fc.totalAsks >>= BI.toInt)
      , bin: fc.bin
      , asksVisibility: fc.asks_visibility
      , favorite: fc.favorite
      , backer: SC.coerce fc.backer
      , profileVisibility: fc.profileVisibility
      , readReceipts: SC.coerce fc.readReceipts
      , messageTimestamps: SC.coerce fc.messageTimestamps
      , typingStatus: SC.coerce fc.typingStatus
      , onlineStatus: SC.coerce fc.onlineStatus
      , completedTutorial: SC.coerce fc.completedTutorial
      , description: fc.description
      , privileges: DM.fromMaybe [] fc.privileges
      , badges: DM.fromMaybe [] fc.badges
      , temporary: SC.coerce fc.temporary
      , joined: DateTimeWrapper fc.joined
      , completedFields: DM.fromMaybe [] fc.completedFields
      , avatar: fc.avatar
      , tags: DM.fromMaybe [] fc.tags
      , karma: fc.karma
      , availability: LastSeen $ DateTimeWrapper fc.lastSeen
      , karmaPosition: fc.karmaPosition
      , gender: show <$> fc.gender
      , country: fc.country
      , languages: DM.fromMaybe [] fc.languages
      , age: DI.ceil <$> fc.age
      }

fromFlatMessage ∷ _ → HistoryMessage
fromFlatMessage fm =
      { id: fm.messageId
      , sender: fm.sender
      , recipient: fm.recipient
      , edited: fm.edited
      , date: fm.date
      , reaction: fm.reaction
      , content: fm.content
      , status: fm.status
      }