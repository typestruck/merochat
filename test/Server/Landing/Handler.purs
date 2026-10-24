module Test.Server.Landing.Handler where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Effect.Aff (Aff)
import Payload.Headers as PH
import Payload.ResponseTypes (Response(..))
import Run as R
import Server.Effect (ServerEffect)
import Server.Im.Action as SIA
import Server.Landing.Handler as SLH
import Shared.Login.Types (LoginResponse)
import Test.Server as TS
import Test.Server.User (email, password)
import Test.Server.User as TSU
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = TU.suite "landing responses" do
      TU.test "registration returns bootstrap data when requested" $ checkResponse true false $
            SLH.register { guards: { checkAnonymous: unit }, body: { email, password, captchaResponse: "" }, query: { boot: true } }
      TU.test "registration skips bootstrap data when not requested" $ checkResponse false false $
            SLH.register { guards: { checkAnonymous: unit }, body: { email, password, captchaResponse: "" }, query: { boot: false } }
      TU.test "guest registration returns bootstrap data when requested" $ checkResponse true true $
            SLH.temporary { guards: { checkAnonymous: unit }, query: { boot: true } }
      TU.test "guest registration skips bootstrap data when not requested" $ checkResponse false true $
            SLH.temporary { guards: { checkAnonymous: unit }, query: { boot: false } }

checkResponse ∷ Boolean → Boolean → ServerEffect (Response LoginResponse) → Aff Unit
checkResponse boot temporary action = TS.serverAction do
      Response { body, headers, status } ← action
      count ← TSU.userCount
      R.liftAff do
            TUA.equal 200 status.code
            TUA.equal 1 count
            TUA.assert "response establishes a session" <<< DM.isJust $ PH.lookup "Set-Cookie" headers
            TUA.equal boot $ DM.isJust body.user
      case body.user of
            Nothing → R.liftAff do
                  TUA.assert "contacts omitted" $ DA.null body.contacts
                  TUA.assert "suggestions omitted" $ DA.null body.suggestions
            Just user → do
                  expected ← SIA.im user.id
                  R.liftAff do
                        TUA.equal temporary user.temporary
                        TUA.equal expected.user.id user.id
                        TUA.equal (map _.user.id expected.contacts) $ map _.user.id body.contacts
                        TUA.equal (map _.id expected.suggestions) $ map _.id body.suggestions
