module Server.IM.Router where

import Prelude
import Server.Types
import Shared.Types
import Shared.IM.Types
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

im :: Request -> ResponseEffect
im request = do
      userID <- SRS.checkLogin request
      user <- SID.presentUser userID
      suggestions <- SIA.suggest userID
      contacts <- SIA.contactList userID 0
      SRR.serveTemplate $ SIT.template { contacts, suggestions, user }

contacts :: Request -> ResponseEffect
contacts request = do
      userID <- SRS.checkLogin request
      case SR.toRoute $ H.fullPath request of
            Right (Contacts { skip }) -> do
                  contacts <- SIA.contactList userID skip
                  SRR.json' $ ContactsPayload contacts
            _ -> SRR.throwBadRequest "invalid parameters"

singleContact :: Request -> ResponseEffect
singleContact request = do
      userID <- SRS.checkLogin request
      case SR.toRoute $ H.fullPath request of
            Right (SingleContact { id }) -> do
                  contact <- DA.singleton <$> SIA.singleContact userID id
                  SRR.json' $ ContactsPayload contact
            _ -> SRR.throwBadRequest "invalid parameters"

history :: Request -> ResponseEffect
history request = do
      userID <- SRS.checkLogin request
      case SR.toRoute $ H.fullPath request of
            Right (History { skip, with }) -> do
                  chatHistory <- SID.chatHistoryBetween userID with skip
                  SRR.json' $ HistoryPayload chatHistory
            _ -> SRR.throwBadRequest "invalid parameters"

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