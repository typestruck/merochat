module Server.ThreeK
      ( generateName
      , generateHeadline
      , generateDescription
      , generateConversationStarter
      ) where

import Prelude
import Shared.Options.Profile

import Data.Array as DA
import Data.Reflectable as DR
import Data.String as DS
import Effect.Class (liftEffect)
import Effect.Random as ER
import Run as R
import Server.ThreeK.Name as STN
import Server.Effect (ServerEffect)
import Server.ThreeK.Database as STBD

-- | Names are generated according to the following patterns:
-- |  <adjective> [, <adjective>] <noun>
-- | [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]
generateName ∷ ServerEffect String
generateName = R.liftEffect do
      size ← ER.randomInt 10 nameMaxCharacters
      shouldHappen ← STN.shouldHappen 60
      if shouldHappen then
            STN.simpleName size
      else
            STN.complexName size

-- | Headlines are pulled from a database of "funny" one liners
generateHeadline ∷ ServerEffect String
generateHeadline = STBD.fetchHeadline 1

generateConversationStarter ∷ ServerEffect String
generateConversationStarter = STBD.fetchConversationStarter 1

-- | Descriptions are bullet point lists of quotes/about me/conversation starters
generateDescription ∷ ServerEffect String
generateDescription = do
      n ← liftEffect $ ER.randomInt 1 6
      quotes ← STBD.fetchDescription n
      pure <<< DS.joinWith "\n" $ map (("- " <> _)) quotes
