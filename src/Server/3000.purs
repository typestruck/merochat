module Server.ThreeK
      ( generateName
      , generateHeadline
      , generateDescription
      ) where

import Prelude
import Server.Types

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Effect.Random as ER
import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Run as R
import Run.Reader as RR
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters)

data ThreeKAction = Name | Description

generateName ∷ ServerEffect String
generateName = do
      size ← R.liftEffect $ ER.randomInt 10 nameMaxCharacters
      generate Name size

generateDescription ∷ ServerEffect String
generateDescription = do
      size ← R.liftEffect $ ER.randomInt 100 descriptionMaxCharacters
      generate Description size

generateHeadline ∷ ServerEffect String
generateHeadline = do
      let maxSize = headlineMaxCharacters

      size ← R.liftEffect $ ER.randomInt 1 maxSize
      headline ← generate Description size

      case DS.lastIndexOf (Pattern ".") headline of
            Nothing → pure headline
            Just index → do
                  let cutHeadline = DS.take index headline
                  chance ← R.liftEffect $ ER.randomInt 1 100
                  if DS.length cutHeadline <= maxSize - 3 && chance <= 30 then
                        pure $ cutHeadline <> "..."
                  else
                        pure headline

instance Show ThreeKAction where
      show Name = "name"
      show Description = "description"

derive instance Eq ThreeKAction

--thats a lot of work...
instance Ord ThreeKAction where
      compare Name Description = LT
      compare Description Name = GT
      compare _ _ = EQ

instance Bounded ThreeKAction where
      bottom = Name
      top = Description

instance BoundedEnum ThreeKAction where
      cardinality = Cardinality 1
      fromEnum Name = 0
      fromEnum Description = 1
      toEnum 0 = Just Name
      toEnum 1 = Just Description
      toEnum _ = Nothing

instance Enum ThreeKAction where
      succ Name = Just Description
      succ Description = Nothing
      pred Name = Nothing
      pred Description = Just Name