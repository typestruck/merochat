module Server.Router.Session where

import Prelude
import Server.Types
import Shared.Types

import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import HTTPure (Method(..), Request)
import HTTPure.Lookup ((!@))
import Run.Except as RE
import Run.Reader as RR

-- | Raises an exception if there is a logged user
checkAnonymous :: ServerEffect Unit
checkAnonymous = do
      { session : { userID } } <- RR.ask
      when (DM.isJust userID) $ RE.throw $ BadRequest { reason: "" }

-- | Raises an exception if there is no logged user
checkLogin :: Request -> ServerEffect PrimaryKey
checkLogin { path, method } = do
      { session : { userID } } <- RR.ask
      case userID of
            Just id -> pure id
            _ -> RE.throw $ BadRequest {
                  reason: ""
            }
