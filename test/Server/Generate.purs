module Test.Server.Generate where

import Prelude

import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.String as DS
import Data.Tuple.Nested ((/\))
import Droplet.Language (from, insert, into, select, values, wher, (.=.))
import Run as R
import Server.Database as SD
import Server.Database.Fields (_contents, _id)
import Server.Database.UnsubscribeTokens (_unsubscriber, unsubscribeTokens)
import Server.Generate as SG
import Server.Unsubscribe.Handler as SUH
import Shared.User (ReceiveEmail(..))
import Test.Server as TS
import Test.Server.User as TSU
import Test.Unit (TestSuite)
import Test.Unit as TU
import Test.Unit.Assert as TUA

tests ∷ TestSuite
tests = do
      TU.suite "generate" do
            TU.test "generates name"
                  $ TS.serverAction
                  do
                        name <- SG.generateName
                        R.liftAff <<< TUA.assert "has name" <<< not $ DS.null name

            TU.test "generates headline"
                  $ TS.serverAction
                  do
                        headline <- SG.generateHeadline
                        R.liftAff <<< TUA.assert "has headline" <<< not $ DS.null headline

            TU.test "generates description"
                  $ TS.serverAction
                  do
                        lines <- SG.generateDescriptionLines 4
                        R.liftAff <<< TUA.assert "numbe of lines" $ DA.length lines == 4
