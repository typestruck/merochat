module Server.Push (push) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn3, EffectFn4)
import Effect.Uncurried as EU
import Environment (production)
import Shared.Options.Topic as SOT

foreign import push_ :: EffectFn4 String String String Int Unit

ntfyUrl ∷ String
ntfyUrl = "http://localhost:1111/"

push :: Int -> Int -> String -> Effect Unit
push loggedUserId userId userName
    | production = EU.runEffectFn4 push_ ntfyUrl (SOT.makeTopic userId) userName loggedUserId
    | otherwise = pure unit


