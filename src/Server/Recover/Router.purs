module Server.Recover.Router where

import Prelude
import Server.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import HTTPure (Method(..), Request)
import HTTPure as H
import Server.Recover.Template as SRT
import Server.Recover.Action as SRA
import Server.Response as SRR
import Server.Router.Session as SRS
import Shared.Router as SR

recover :: Request -> ResponseEffect
recover request@{ method, body } = SRS.ifAnonymous $
      if method == Get then do
            let token = case SR.toRoute $ H.fullPath request of
                  Right (Recover { token }) -> token
                  _ -> Nothing
            SRR.serveTemplate (SRT.template token)
       else
            SRR.json body SRA.recover