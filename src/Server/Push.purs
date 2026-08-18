module Server.Push (initializeSubscriptions, push, PushMessage(..)) where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Data.Argonaut as DA
import Data.Array as DAR
import Data.Array.NonEmpty as DAN
import Data.HashMap (HashMap)
import Data.HashMap as DH
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as DN
import Data.Traversable as DT
import Debug (spy)
import Droplet.Driver (Pool)
import Effect (Effect)
import Effect.Aff as EA
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as EAC
import Effect.Class as EC
import Effect.Ref (Ref)
import Effect.Ref as ER
import Prim.Row (class Lacks, class Cons)
import Record as R
import Run as RN
import Run.Reader as RR
import Server.Database.Subscriptions as SDS
import Server.Effect (BaseEffect, BaseReader)
import Server.Effect as SE
import Shared.Im.Types (ClientMessagePayload)
import Shared.Unsafe as SU
import Type.Proxy (Proxy(..))

data PushMessage = IncomingMessage ClientMessagePayload | MessageReadSomewhereElse { userIds ∷ Array Int }

type PushEffect r s = BaseEffect (PushReader r) s

type PushReader r = BaseReader
      ( allUserSubscriptionsRef ∷ Ref (HashMap Int (Array String))
      | r
      )

initializeSubscriptions ∷ Pool → Ref (HashMap Int (Array String)) → Effect Unit
initializeSubscriptions pool allUserSubscriptionsRef = EA.launchAff_ $ SE.poolEffect pool unit do
      subscriptions ← SDS.fetchSubscriptions
      EC.liftEffect $ ER.write (DH.fromArrayBy makeKey makeValue $ DAR.groupBy sameSub subscriptions) allUserSubscriptionsRef
      where
      sameSub s t = s.subscriber == t.subscriber
      makeKey subs = (DAN.head subs).subscriber
      makeValue subs = DAN.toArray $ map _.token subs

push ∷ ∀ r. Int → String → PushMessage → PushEffect r Unit
push userId title message = do
      context ← RR.ask
      allUserSubscriptions ← EC.liftEffect $ ER.read context.allUserSubscriptionsRef
      case DH.lookup userId allUserSubscriptions of
            Just subscriptions → do
                  possibleInvalids ← DT.traverse (map DN.toMaybe <<< sendPush title message) subscriptions
                  case DAR.catMaybes possibleInvalids of
                        [] → pure unit
                        invalids → do
                              EC.liftEffect $ ER.modify_ (DH.update (\s → Just $ DAR.difference s invalids) userId) context.allUserSubscriptionsRef
                              SDS.deleteSubscriptions userId <<< SU.fromJust $ DAN.fromArray (spy "invalids" invalids)
            Nothing → pure unit

foreign import push_ ∷ String → String → String → EffectFnAff (Nullable String)

sendPush ∷ ∀ r. String → PushMessage → String → PushEffect r (Nullable String)
sendPush title message subscription = RN.liftAff <<< EAC.fromEffectFnAff <<< push_ subscription title $ DA.stringify encoded
      where
      encoded = case message of
            IncomingMessage im → encodeJson $ addType "incoming" im
            MessageReadSomewhereElse mrse → encodeJson $ addType "read" mrse

--the service workers uses type to tell pushes apart
addType ∷ ∀ r. Lacks "type" r ⇒ String → { | r } → { "type" ∷ String | r }
addType typeName record = R.insert (Proxy ∷ Proxy "type") typeName record

encodeJson ∷ ∀ q r. Cons "type" String q r ⇒ EncodeJson (Record r) ⇒ Record r → Json
encodeJson = DA.encodeJson

