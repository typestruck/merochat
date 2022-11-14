module Server.Database.StockText where

import Droplet.Language
import Prelude

import Control.Monad.Except as CME
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\))
import Foreign as F
import Type.Proxy (Proxy(..))

data TextType = Headline | Description

instance ToValue TextType where
      toValue = case _ of
            Headline -> F.unsafeToForeign 0
            Description -> F.unsafeToForeign 1

instance FromValue TextType where
      fromValue value =
            case CME.runExcept $ F.readInt value of
                  Right 0 -> Right Headline
                  Right 1 -> Right Description
                  n -> Left $ "Invalid TextType " <> show n

type StockText =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , contents ∷ String
      , text_type ∷ TextType
      )

stock_text ∷ Table "stock_text" StockText
stock_text = Table

_text_type ∷ Proxy "text_type"
_text_type = Proxy
