module Server.Generate.Database where

import Droplet.Language (from, limit, orderBy, random, select, wher, (.=.))
import Prelude
import Server.Database.Fields (_contents)
import Server.Database.StockText (TextType(..), _text_type, stock_text)

import Data.Array as DA
import Data.Reflectable as DR
import Server.Database as SD
import Server.Effect (ServerEffect)
import Shared.Unsafe as SU

fetchHeadline ∷ ServerEffect String
fetchHeadline = SU.fromJust <<< DA.head <$> fetch 1 Headline

fetchConversationStarter ∷ ServerEffect String
fetchConversationStarter = SU.fromJust <<< DA.head <$> fetch 1 ConversationStarter

fetchDescription ∷ Int → ServerEffect (Array String)
fetchDescription howMany = fetch howMany Description

fetch ∷ Int → TextType → ServerEffect (Array String)
fetch howMany textType = DR.reifyType howMany (\n → map (map _.contents) <<< SD.query $ select _contents # from stock_text # wher (_text_type .=. textType) # orderBy random # limit n)