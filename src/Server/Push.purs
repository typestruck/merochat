module Server.Push (initiazeSubscriptions, push, PushMessage(..)) where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as DA
import Data.Array as DAR
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as DAN
import Data.Foldable as DF
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Debug (spy)
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Class as EC
import Effect.Ref (Ref)
import Effect.Ref as ER
import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Environment (production)
import Prim.Row (class Lacks, class Cons)
import Record as R
import Run.Reader as RR
import Server.Database.Subscriptions as SDS
import Server.Effect (BaseEffect, BaseReader)
import Server.Effect as SE
import Shared.Im.Types (ClientMessagePayload)
import Type.Proxy (Proxy(..))

data PushMessage = IncomingMessage ClientMessagePayload | MessageReadSomewhereElse { userIds ∷ Array Int }

type PushEffect r = BaseEffect (PushReader r) Unit

type PushReader r = BaseReader
      ( allUserSubscriptionsRef ∷ Ref (HashMap Int (Array String))
      | r
      )

initiazeSubscriptions ∷ Pool → Ref (HashMap Int (Array String)) → Effect Unit
initiazeSubscriptions pool allUserSubscriptionsRef = EA.launchAff_ $ SE.poolEffect pool unit do
      subscriptions ← SDS.fetchSubscriptions
      EC.liftEffect $ ER.write (DH.fromArrayBy makeKey makeValue $ DAR.groupBy sameSub subscriptions) allUserSubscriptionsRef
      where
      sameSub s t = s.subscriber == t.subscriber
      makeKey subs = (DAN.head subs).subscriber
      makeValue subs = DAN.toArray $ map _.token subs

foreign import push_ ∷ EffectFn3 String String String Unit

push ∷ forall r. Int → String → PushMessage → PushEffect r
push userId title message = do
      context ← RR.ask
      EC.liftEffect do
            allUserSubscriptions ← ER.read context.allUserSubscriptionsRef
            case DH.lookup userId allUserSubscriptions of
                  Just subscriptions → DF.traverse_ (sendPush title message) subscriptions
                  Nothing → pure unit

sendPush ∷ String → PushMessage → String → Effect Unit
sendPush title message subscription = EU.runEffectFn3 push_ subscription title $ DA.stringify encoded
      where
      encoded = case message of
            IncomingMessage im → encodeJson $ addType "incoming" im
            MessageReadSomewhereElse mrse → encodeJson $ addType "read" mrse

--the service workers uses type to tell pushs apart
addType ∷ ∀ r. Lacks "type" r ⇒ String → { | r } → { "type" ∷ String | r }
addType typeName record = R.insert (Proxy ∷ Proxy "type") typeName record

encodeJson ∷ ∀ q r. Cons "type" String q r ⇒ EncodeJson (Record r) ⇒ Record r → Json
encodeJson = DA.encodeJson

