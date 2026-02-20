module Test.Server.Unsubscribe where

import Droplet.Language (from, insert, into, select, values, wher, (.=.))
import Prelude
import Server.Database.Fields (_contents, _id)

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Run as R
import Server.Database as SD
import Server.Database.UnsubscribeTokens (_unsubscriber, unsubscribeTokens)
import Server.Unsubscribe.Handler as SUH
import Shared.User (ReceiveEmail(..))
import Test.Server as TS
import Test.Server.User as TSU
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "/unsubscribe" do
            TU.test "updates user email setting"
                  $ TS.serverAction
                          do
                                userId ← TSU.createUser
                                SD.execute $ insert # into unsubscribeTokens (_contents /\ _unsubscriber) # values ("1234" /\ userId)
                                void $ SUH.unsubscribe { query: { token: "1234" } }
                                user ← TSU.fetchUser userId
                                R.liftAff $ TUA.equal (Just NoEmails) (_.receive_email <$> user)

            TU.test "deletes unsubscribe token"
                  $ TS.serverAction
                          do
                                userId ← TSU.createUser
                                SD.execute $ insert # into unsubscribeTokens (_contents /\ _unsubscriber) # values ("1234" /\ userId)
                                void $ SUH.unsubscribe { query: { token: "1234" } }
                                token ← SD.single $ select _id # from unsubscribeTokens # wher (_contents .=. "1234")
                                R.liftAff $ TUA.equal Nothing token
