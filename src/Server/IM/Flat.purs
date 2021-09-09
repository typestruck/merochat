module Server.IM.Flat where

import Prelude

import Data.DateTime (DateTime)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Shared.IM.Types (Contact, IMUser)
import Shared.User (Gender)

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
      , name ∷ String
      , tags ∷ Maybe String
      | rest
      }

type FlatUser = FlatFields ()

type FlatContact = FlatFields
      ( chatAge ∷ Maybe Number
      , chatStarter ∷ Int
      -- only used for ordering
      , "h.date" ∷ DateTime
      )

fromFlatContact ∷ FlatContact → Contact
fromFlatContact fc =
      { shouldFetchChatHistory: true
      , available: true
      , chatAge: DM.fromMaybe 0.0 fc.chatAge
      , chatStarter: fc.chatStarter
      , impersonating: Nothing
      , history: []
      , user: fromFlatUser fc
      }

fromFlatUser ∷ ∀ r. FlatFields r → IMUser
fromFlatUser fc =
      { id: fc.id
      , name: fc.name
      , headline: fc.headline
      , description: fc.description
      , avatar: fc.avatar
      , tags: splitAgg "\\n" fc.tags
      , karma: fc.karma
      , karmaPosition: fc.karmaPosition
      , gender: show <$> fc.gender
      , country: fc.country
      , languages: splitAgg "," fc.languages
      , age: DI.ceil <$> fc.age
      }
      where
      splitAgg pattern = DM.maybe [] (DS.split (Pattern pattern))
