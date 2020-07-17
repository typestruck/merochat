module Server.IM.Router where

import Prelude
import Server.Types
import Shared.Types

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

im :: Request -> ResponseEffect
im { path } = do
      let handler = do
            userID <- SRS.loggedUserID
            user <- SID.presentUser userID
            suggestions <- SIA.suggest userID
            contacts <- SIA.contactList userID 0
            SRR.serveTemplate $ SIT.template { contacts, suggestions, user }
      SRS.ifLogged path handler

--REFACTOR: page and other query parameters can be safely parsed by using the Shared.Router functions....
contacts :: Request -> ResponseEffect
contacts { query } = do
      userID <- SRS.loggedUserID
      list <- SIA.contactList userID <<< SU.fromJust "contacts" $ DI.fromString (query !@ "skip")
      SRR.json' $ JSONResponse list

history :: Request -> ResponseEffect
history request = do
      userID <- SRS.loggedUserID
      case SR.toRoute $ H.fullPath request of
            Right (History { skip, with }) -> do
                  list <- SID.chatHistoryBetween userID with skip
                  SRR.json' $ JSONResponse list
            _ -> SRR.throwBadRequest "invalid parameters"

suggestions :: ResponseEffect
suggestions = do
      userID <- SRS.loggedUserID
      list <- SIA.suggest userID
      SRR.json' $ JSONResponse list