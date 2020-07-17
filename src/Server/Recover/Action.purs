module Server.Recover.Action where

import Prelude
import Server.Types
import Shared.Types

import Affjax as A
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut.Decode as DAD
import Data.Either (Either(..))
import Data.Either as DE
import Data.FormURLEncoded as DF
import Data.HTTP.Method (Method(..))
import Server.Token as ST
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String as DS
import Data.Tuple (Tuple(..))
import Run as R
import Run.Reader as RR
import Server.Response as SRR

invalidUserEmailMessage :: String
invalidUserEmailMessage = "Invalid email or password"

emailAlreadyRegisteredMessage :: String
emailAlreadyRegisteredMessage = "Email already registered"

recover :: RecoverPassword -> ServerEffect Ok
recover _ = pure Ok