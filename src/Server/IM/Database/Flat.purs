module Server.IM.Database.Flat where

import Prelude

import Data.DateTime (DateTime)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Server.Database.Flat as SDF
import Shared.IM.Types (Contact, IMUser)
import Shared.Unsafe as SU
import Shared.Avatar as SA
import Shared.User

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
      , name ∷ String
      , tags ∷ Maybe String
      | rest
      }

type FlatUser = FlatFields ()

type FlatContact = FlatFields
      ( chatAge ∷ Maybe Number
      , chatStarter ∷ Maybe Int
      -- only used for ordering
      , lastMessageDate ∷ Maybe DateTime
      )

fromFlatContact ∷ FlatContact → Contact
fromFlatContact fc =
      { shouldFetchChatHistory: true
      , chatAge: DM.fromMaybe 0.0 fc.chatAge
      , chatStarter: SU.fromJust fc.chatStarter
      , impersonating: Nothing
      , history: []
      , user: fromFlatUser fc
      , typing: false
      }

fromFlatUser ∷ ∀ r. FlatFields r → IMUser
fromFlatUser fc =
      { id: fc.id
      , name: fc.name
      , headline: fc.headline
      , profileVisibility: fc.profileVisibility
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
