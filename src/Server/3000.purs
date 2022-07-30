module Server.ThreeK
      ( generateName
      , generateHeadline
      , generateDescription
      ) where

import Droplet.Language
import Prelude
import Server.Database.StockText
import Effect.Random as ER
import Run as R
import Server.Database as SD
import Server.ThreeK.Name as STN
import Server.Types (ServerEffect)
import Shared.Options.Profile
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))

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
generateHeadline = map (_.contents <<< SU.fromJust) <<< SD.single $ select _contents # from stock_text # wher (_text_type .=. Headline) # orderBy random # limit (Proxy ∷ Proxy 1)

generateDescription ∷ ServerEffect String
generateDescription = pure "AAAAAAAAAAAAAAAAAA"
