module Server.Fortune.Action where

import Prelude

import Data.Array as DA
import Data.String as DS
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3)
import Effect.Uncurried as EU
import Run as R
import Server.Effect (ServerEffect)

-- wrapper around crypto.getRandomValues
foreign import randomInt_ :: EffectFn2 Int Int Int

foreign import randomInts_ :: EffectFn3 Int Int Int (Array Int)

randomInt :: Int -> Int -> Effect Int
randomInt = EU.runEffectFn2 randomInt_

randomInts :: Int -> Int -> Int -> Effect (Array Int)
randomInts = EU.runEffectFn3 randomInts_

--select five (distinct) numbers from 1 to 69, and one number from 1 to 26
fortunate ∷ ServerEffect String
fortunate = do
      niceOnes ← R.liftEffect $ randomInts 5 1 69
      lastOne ← R.liftEffect $ randomInt 1 27
      pure <<< DS.joinWith " " <<< map pad $ DA.snoc niceOnes lastOne
      where

      pad n =
            let
                  digits = show n
            in
                  if DS.length digits == 1 then
                        "0" <> digits
                  else
                        digits
