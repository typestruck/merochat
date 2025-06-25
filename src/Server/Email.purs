module Server.Email (sendEmail, Email(..)) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn4)
import Effect.Uncurried as EU
import Server.Effect (ServerEffect)

data Email = Feedback | Report | Reset

foreign import sendEmail_ ∷ EffectFn4 String Int Int Int Unit

url :: String
url = "http://localhost:4000/send/"

sendEmail ∷ Int → Int → Email → ServerEffect Unit
sendEmail userId recordId email = liftEffect $ EU.runEffectFn4 sendEmail_ url userId recordId emailOption
      where
      emailOption = case email of
            Feedback → 1
            Report → 2
            Reset → 3
