module Client.Ui.Media where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as EU

foreign import isMediaTypeSupported_ ∷ EffectFn1 String Boolean

acceptedAudioCodec ∷ Effect String
acceptedAudioCodec = do
      isIt ← EU.runEffectFn1 isMediaTypeSupported_ appleShit
      if isIt then pure appleShit else pure webm
      where
      appleShit = "video/mp4;codecs=avc1"
      webm = "audio/webm"
