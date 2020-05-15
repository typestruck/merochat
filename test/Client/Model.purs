module Test.Client.Model where

import Effect.Aff (Aff)
import Prelude

run :: forall m. m -> Aff (m -> m) -> Aff m
run model f = do
        f' <- f
        pure $ f' model
