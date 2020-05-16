module Shared.Settings.Types where

import Prelude
import Shared.Types

import Affjax.RequestBody (RequestBody(..))
import Control.Monad.Except as CME
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep as DADGR
import Data.Argonaut.Encode.Generic.Rep as DAEGR
import Data.Bifunctor as DB
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as DIN
import Data.JSDate as DJ
import Data.List.NonEmpty as DLN
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as DS
import Data.String.Read as DSR
import Data.Traversable as DT
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3)
import Database.PostgreSQL (class FromSQLRow)
import Debug.Trace (spy)
import Flame (Key)
import Foreign (Foreign)
import Foreign as F
import Shared.Types (MDate(..), Editor)
import Shared.Unsafe as SU
import Web.Event.Internal.Types (Event)

newtype SettingsModel = SettingsModel {
        email :: String,
        emailConfirmation :: String,
        password :: String,
        passwordConfirmation :: String
}

data SettingsMessage =
        SetEmail String |
        SetEmailConfirmation String |
        SetPassword String |
        SetPasswordConfirmation String |
        ChangeEmail |
        ChangePassword |
        TerminateAccount --very bad

derive instance genericSettingsModel :: Generic SettingsModel _

derive instance newtypeSettingsModel :: Newtype SettingsModel _