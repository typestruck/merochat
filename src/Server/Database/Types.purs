module Server.Database.Types where

import Prelude

import Data.Either (Either)
import Droplet.Language (class FromValue, class ToValue)
import Droplet.Language as DL

newtype Checked = Checked Boolean

instance FromValue Checked where
      fromValue c = Checked <$> (DL.fromValue c ∷ Either String Boolean)

instance ToValue Checked where
      toValue (Checked c) = DL.toValue c