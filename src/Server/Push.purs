module Server.Push (push, PushMessage(..)) where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as DA
import Effect (Effect)
import Effect.Uncurried (EffectFn4)
import Effect.Uncurried as EU
import Environment (production)
import Prim.Row (class Lacks, class Cons)
import Record as R
import Shared.Im.Types (ClientMessagePayload)
import Shared.Options.Topic as SOT
import Type.Proxy (Proxy(..))

data PushMessage = IncomingMessage ClientMessagePayload | MessageReadSomewhereElse { userId ∷ Int }

foreign import push_ ∷ EffectFn4 String String String String Unit

ntfyUrl ∷ String
ntfyUrl = "http://localhost:1111/"

push ∷ Int → String → PushMessage → Effect Unit
push userId title message
      | production = EU.runEffectFn4 push_ ntfyUrl (SOT.makeTopic userId) title $ DA.stringify encoded
              where
              encoded = case message of
                    IncomingMessage im → encodeJson $ addType "incoming" im
                    MessageReadSomewhereElse mrse → encodeJson $ addType "read" mrse
      | otherwise = pure unit

--the service workers uses type to tell pushs apart
addType ∷ ∀ r. Lacks "type" r ⇒ String → { | r } → { "type" ∷ String | r }
addType typeName record = R.insert (Proxy ∷ Proxy "type") typeName record

encodeJson ∷ ∀ q r. Cons "type" String q r ⇒ EncodeJson (Record r) ⇒ Record r → Json
encodeJson = DA.encodeJson

