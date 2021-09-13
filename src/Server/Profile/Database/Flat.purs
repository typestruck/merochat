module Server.Profile.Database.Flat where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Shared.IM.Types (DateWrapper(..))
import Shared.Profile.Types (ProfileUser)
import Shared.User (Gender)

--  ( id ∷ Int
--       , name ∷ String
--       , headline ∷ String
--       , description ∷ String
--       , avatar ∷ Maybe String
--       , tags ∷ Array String
--       , karma ∷ Int
--       , karmaPosition ∷ Int

--  gender ∷ Maybe Gender
--               , country ∷ Maybe Int
--               , languages ∷ Array Int
--               , age ∷ Maybe DateWrapper
--               )

type FlatProfileUser =
      { avatar ∷ Maybe String
      , birthday ∷ Maybe Date
      , country ∷ Maybe Int
      , description ∷ String
      , gender ∷ Maybe Gender
      , headline ∷ String
      , id ∷ Int
      , karma ∷ Int
      , karmaPosition ∷ Int
      , languages ∷ Maybe (Array Int)
      , name ∷ String
      , tags ∷ Maybe String
      }

fromFlatProfileUser ∷ FlatProfileUser → ProfileUser
fromFlatProfileUser fu =
      { avatar: fu.avatar
      , age: DateWrapper <$> fu.birthday
      , country: fu.country
      , description: fu.description
      , gender: fu.gender
      , headline: fu.headline
      , id: fu.id
      , karma: fu.karma
      , karmaPosition: fu.karmaPosition
      , languages: DM.fromMaybe [] fu.languages
      , name: fu.name
      , tags: splitAgg "\\n" fu.tags
      }
      where
      splitAgg pattern = DM.maybe [] (DS.split (Pattern pattern))