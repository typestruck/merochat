module Server.ThreeK
      ( generateName
      , generateHeadline
      , generateDescription
      ) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Effect.Class (liftEffect)
import Effect.Exception as EE
import Effect.Random as ER
import Run as R
import Server.ThreeK.Name as STN
import Server.Types (ServerEffect)
import Shared.Options.Profile (descriptionMaxCharacters, headlineMaxCharacters, nameMaxCharacters)

data ThreeKAction = Name | Description

-- | Names are generated according to the following patterns:
-- |  <adjective> [, <adjective>] <noun>
-- | [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]
generateName ∷ ServerEffect String
generateName  = R.liftEffect do
      size ←  ER.randomInt 10 nameMaxCharacters
      shouldHappen <- STN.shouldHappen 60
      if shouldHappen  then
            STN.simpleName size
      else
            STN.complexName size

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

generate a b = pure "AAAAAAAAAAAAAAAAAA"

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