module Server.Fortune.Action where

import Prelude
import Server.Types

import Data.Array as DA
import Data.String as DS
import Data.Unfoldable as DU
import Effect.Random as ER
import Run as R

--select five numbers from 1 to 69, and one number from 1 to 26
fortunate :: ServerEffect String
fortunate = do
      niceOnes <- DU.replicateA 5 <<< R.liftEffect $ ER.randomInt 1 69
      lastOne <- R.liftEffect $ ER.randomInt 1 26
      pure <<< DS.joinWith " " <<< map pad $ DA.snoc niceOnes lastOne
      where pad n =
                  let digits = show n
                  in
                        if DS.length digits == 1 then
                              "0" <> digits
                         else
                              digits
