module Server.Email (sendEmail) where

import Prelude

import Effect.Uncurried (EffectFn4)
import Effect.Uncurried as EU
import Environment (production)
import Run as R
import Run.Reader as RR
import Server.Effect (ServerEffect)

foreign import sendEmail_ ∷ EffectFn4 { host ∷ String, user ∷ String, password ∷ String } String String String Unit

sendEmail ∷ String → String → String → ServerEffect Unit
sendEmail to subject content = do
      { configuration: { emailUser, emailHost, emailPassword } } ← RR.ask
      when production <<< R.liftEffect $ EU.runEffectFn4 sendEmail_ { user: emailUser, host: emailHost, password: emailPassword } to subject content
