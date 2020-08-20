module Server.IM.Router where

import Prelude
import Server.Types
import Shared.Types

import Data.Array as DA
import Data.Either (Either(..))
import Data.Either as DE
import Data.Int as DI
import Data.Int53 as DI5
import HTTPure (Request, (!@))
import HTTPure as H
import Partial.Unsafe as PU
import Server.IM.Action as SIA
import Server.IM.Database as SID
import Server.IM.Template as SIT
import Server.Response as SRR
import Server.Router.Session as SRS
import Shared.Router as SR
import Shared.Unsafe as SU


suggestions :: Request -> ResponseEffect
suggestions request = do
      userID <- SRS.checkLogin request
      suggested <- SIA.suggest userID
      SRR.json' $ SuggestionsPayload suggested

blockUser :: Request -> ResponseEffect
blockUser request = do
      userID <- SRS.checkLogin request
      case SR.toRoute $ H.fullPath request of
            Right (Block { id }) -> do
                  response <- SIA.blockUser userID id
                  SRR.json' response
            _ -> SRR.throwBadRequest "invalid parameters"

missedMessages :: Request -> ResponseEffect
missedMessages request = do
      userID <- SRS.checkLogin request
      case SR.toRoute $ H.fullPath request of
            Right (MissedMessages { since }) -> do
                  response <- SIA.missedMessages userID since
                  SRR.json' $ MissedMessagesPayload response
            _ -> SRR.throwBadRequest "invalid parameters"