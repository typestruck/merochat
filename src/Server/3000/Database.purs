module Server.ThreeK.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.StockText

import Data.Array as DA
import Data.Reflectable as DR
import Server.Database as SD
import Server.Effect (ServerEffect)
import Shared.Unsafe as SU

fetchHeadline ∷ Int → ServerEffect String
fetchHeadline howMany = SU.fromJust <<< DA.head <$> fetch howMany Headline

fetchConversationStarter ∷ Int → ServerEffect String
fetchConversationStarter howMany = SU.fromJust <<< DA.head <$> fetch howMany ConversationStarter

fetchDescription ∷ Int → ServerEffect (Array String)
fetchDescription howMany = fetch howMany Description

fetch ∷ Int → TextType → ServerEffect (Array String)
fetch howMany textType = DR.reifyType howMany (\l → map (map _.contents) <<< SD.query $ select _contents # from stock_text # wher (_text_type .=. textType) # orderBy random # limit l)