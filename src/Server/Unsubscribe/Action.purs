module Server.Unsubscribe.Action where

import Debug
import Prelude
import Shared.Im.Types
import Shared.Privilege

import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Nullable as DN
import Data.Set (Set)
import Data.Set as DST
import Data.String as DS
import Data.Tuple (Tuple(..))
import Droplet.Driver (Pool)
import Run.Except as RE
import Server.AccountValidation as SA
import Server.Effect (BaseEffect, Configuration, ServerEffect)
import Server.Email as SE
import Server.File as SF
import Environment (production)
import Server.Im.Database as SID
import Server.Im.Database.Flat (FlatContactHistoryMessage, fromFlatContact, fromFlatMessage)
import Server.Im.Database.Flat as SIF
import Server.Im.Types (Payload)
import Server.Sanitize as SS
import Server.ThreeK as ST
import Server.Wheel as SW
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.ResponseError (ResponseError(..))
import Server.Unsubscribe.Database as SUD

unsubscribe ∷ String → ServerEffect Boolean
unsubscribe token = do
      maybeId ← SUD.fetchUnsubscriber token
      case maybeId of
            Nothing → pure false
            Just id → do
                  SUD.unsubscribe id
                  pure true
