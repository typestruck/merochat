module Server.Email
      ( sendEmail
      , Email(..)
      ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class as EC
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Foreign as FO
import Server.Effect (ServerEffect)

--mailjet template vars must always be set
data Email
      = Feedback { feedbacker ∷ Int, comments ∷ String, file ::  String }
      | Report { reported ∷ Int, reporter ∷ Int, reason ∷ String, comment ∷  String }
      | Reset { email ∷ String, user_id ∷ Int, token ∷ String }

foreign import sendEmail_ ∷ EffectFn2 String Foreign Unit

url ∷ String
url = "http://localhost:4000/"

sendEmail ∷ Email → ServerEffect Unit
sendEmail email = EC.liftEffect $ EU.runEffectFn2 sendEmail_ (url <> route) payload
      where
      route /\ payload = case email of
            Feedback f → "/feedback" /\ FO.unsafeToForeign f
            Report r → "/report" /\ FO.unsafeToForeign r
            Reset r → "/reset" /\ FO.unsafeToForeign r
