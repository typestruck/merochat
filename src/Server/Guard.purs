module Server.Guard where

import Prelude
import Server.Types
import Shared.Types

import Data.Either (Either(..))
import Data.Either (Either)
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Maybe as DMB
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.HTTP (Request)
import Node.HTTP as NH
import Payload.ContentType (json)
import Payload.Headers (empty)
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Guards as PSG
import Payload.Server.Response as PRS
import Payload.Server.Response as PSR
import Payload.Server.Response as PSR
import Server.Cookies (cookieName)
import Server.Domain (domain)
import Server.Token as ST


guards :: Configuration -> _
guards configuration = {
      loggedUserID: checkLoggedUser configuration,
      checkAnonymous: checkAnonymous configuration
}

checkLoggedUser :: Configuration -> Request -> Aff (Either (Response Empty) PrimaryKey)
checkLoggedUser { development, tokenSecret } request = do
      headers <- PSG.headers request
      if isPost && (not development && PH.lookup "origin" headers /= Just domain || PH.lookup "content-type" headers /= Just json) then
            badRequest
       else do
            cookies <- PSG.cookies request
            maybeUserID <- liftEffect $ ST.userIDFromToken tokenSecret <<< DMB.fromMaybe "" $ DM.lookup cookieName cookies
            case maybeUserID of
                  Just userID -> pure $ Right userID
                  _ ->
                        if isPost then
                              pure <<< Left $ PSR.unauthorized Empty
                         else
                              redirectLogin
      where isPost = NH.requestMethod request == "POST"
            redirectLogin = redirect -- $ Login { next: Just $ NH.requestURL request }

checkAnonymous :: Configuration -> Request -> Aff (Either (Response Empty) Unit)
checkAnonymous { development, tokenSecret } request = do
      headers <- PSG.headers request
      if NH.requestMethod request == "POST" && (not development && PH.lookup "origin" headers /= Just domain) then
            badRequest
       else do
            cookies <- PSG.cookies request
            maybeUserID <- liftEffect $ ST.userIDFromToken tokenSecret <<< DMB.fromMaybe "" $ DM.lookup cookieName cookies
            case maybeUserID of
                  Just userID ->
                        if isPost then
                              pure <<< Left $ PSR.forbidden Empty
                         else
                              redirectIM
                  _ -> pure $ Right unit
      where isPost = NH.requestMethod request == "POST"
            redirectIM = redirect -- $ IM

badRequest :: forall r. Aff (Either (Response Empty) r)
badRequest = pure <<< Left $ PSR.badRequest Empty

-- redirect :: forall r. Route -> Aff (Either (Response Empty) r)
-- redirect route = pure <<< Left <<< PSR.setHeaders location $ PRS.found Empty
--       where location = PH.set "Location" (SR.fromRoute route) empty

redirect :: forall r. Aff (Either (Response Empty) r)
redirect  = pure <<< Left <<< PSR.setHeaders location $ PRS.found Empty
      where location = PH.set "Location" "(SR.fromRoute route)" empty