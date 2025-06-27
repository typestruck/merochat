module Server.Email
      ( sendEmail,
      Email(..)
      ) where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class as EC
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Foreign (Foreign)
import Foreign as FO
import Server.Effect (ServerEffect)

data Email = Feedback | Report | Reset { email ∷ String, user_id ∷ Int, token ∷ String }

foreign import sendEmail_ ∷ EffectFn2 String Foreign Unit

url ∷ String
url = "http://localhost:4000/"

sendEmail ∷ Email → ServerEffect Unit
sendEmail email = EC.liftEffect $ EU.runEffectFn2 sendEmail_ (url <> route) payload
      where
      route /\ payload = case email of
            Feedback → "/feedback" /\ FO.unsafeToForeign {}
            Report → "/report" /\ FO.unsafeToForeign {}
            Reset r → "/reset" /\ FO.unsafeToForeign r
