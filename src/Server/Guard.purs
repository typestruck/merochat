module Server.Guard where

import Prelude
import Server.Effect

import Data.Either (Either(..))
import Data.Map as DM
import Data.Maybe (Maybe(..))
import Data.Maybe as DMB
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.HTTP (Request)
import Node.HTTP as NH
import Payload.Headers (empty)
import Payload.Headers as PH
import Payload.ResponseTypes (Empty(..), Response)
import Payload.Server.Guards as PSG
import Payload.Server.Response as PSR
import Run as R
import Run.Except as RE
import Run.Reader as RR
import Server.Cookies (cookieName)
import Server.Effect as SE
import Server.Token as ST
import Shared.Routes (routes)

guards ∷ ServerReader → _
guards reading =
      { loggedUserId: checkLoggedUser reading
      , checkAnonymous: checkAnonymous reading
      }

checkLoggedUser ∷ ServerReader → Request → Aff (Either (Response Empty) Int)
checkLoggedUser { configuration: { tokenSecret }, pool } request = do
      cookies ← PSG.cookies request
      maybeUserId ← SE.poolEffect pool <<< ST.userIdFromToken tokenSecret <<< DMB.fromMaybe "" $ DM.lookup cookieName cookies
      case maybeUserId of
            Just userId → pure $ Right userId
            _ →
                  if isPost then
                        pure <<< Left $ PSR.unauthorized Empty
                  else
                        redirectLogin
      where
      isPost = NH.requestMethod request == "POST"
      redirectLogin = redirect $ routes.login.get { query: { next: Just $ NH.requestURL request } }

checkAnonymous ∷ ServerReader → Request → Aff (Either (Response Empty) Unit)
checkAnonymous { configuration: { tokenSecret }, pool } request = do
      cookies ← PSG.cookies request
      maybeUserId ← SE.poolEffect pool <<< ST.userIdFromToken tokenSecret <<< DMB.fromMaybe "" $ DM.lookup cookieName cookies
      case maybeUserId of
            Just _ →
                  if isPost then
                        pure <<< Left $ PSR.forbidden Empty
                  else
                        redirectIm
            _ → pure $ Right unit
      where
      isPost = NH.requestMethod request == "POST"
      redirectIm = redirect $ routes.im.get {}

badRequest ∷ ∀ r. Aff (Either (Response Empty) r)
badRequest = pure <<< Left $ PSR.badRequest Empty

redirect ∷ ∀ r. String → Aff (Either (Response Empty) r)
redirect route = pure <<< Left <<< PSR.setHeaders location $ PSR.found Empty
      where
      location = PH.set "Location" route empty