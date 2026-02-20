-- | Generates names, headlines and descriptions and other stock text
module Server.Generate where

import Prelude
import Shared.Options.Profile (nameMaxCharacters)

import Data.String as DS
import Effect.Class as EC
import Effect.Random as ER
import Server.Generate.Name as STN
import Server.Effect (ServerEffect)
import Server.Generate.Database as STBD

-- | Names are generated according to the following patterns:
-- |  <adjective> [, <adjective>] <noun>
generateName ∷ ServerEffect String
generateName = EC.liftEffect do
      size ← ER.randomInt 10 nameMaxCharacters
      STN.simpleName size

-- | Headlines are pulled from a database of "funny" one liners
generateHeadline ∷ ServerEffect String
generateHeadline = STBD.fetchHeadline

-- | Descriptions are bullet point lists of quotes/about me/conversation starters
generateDescription ∷ ServerEffect String
generateDescription = do
      n ← EC.liftEffect $ ER.randomInt 1 6
      lines ← generateDescriptionLines n
      pure <<< DS.joinWith "\n" $ map ("- " <> _) lines

generateDescriptionLines ∷ Int → ServerEffect (Array String)
generateDescriptionLines n = STBD.fetchDescription n

generateConversationStarter ∷ ServerEffect String
generateConversationStarter = STBD.fetchConversationStarter

