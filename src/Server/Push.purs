module Server.Push (push) where

import Prelude

import Data.Argonaut as DA
import Effect (Effect)
import Effect.Uncurried (EffectFn4)
import Effect.Uncurried as EU
import Environment (production)
import Shared.Im.Types (ClientMessagePayload)
import Shared.Options.Topic as SOT

foreign import push_ :: EffectFn4 String String String String Unit

ntfyUrl ∷ String
ntfyUrl = "http://localhost:1111/"

push :: Int -> String -> ClientMessagePayload -> Effect Unit
push userId userName message
    | production = EU.runEffectFn4 push_ ntfyUrl (SOT.makeTopic userId) userName <<< DA.stringify $ DA.encodeJson message
    | otherwise = pure unit


