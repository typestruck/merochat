module Client.File (compressImageFromFileList, compressImage, triggerFileSelect, fileSize) where

import Prelude

import Client.AppId (ClientAppId)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Flame (AppId)
import Flame.Subscription as FS
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.File.File (File)
import Web.File.FileList (FileList)
import Web.File.FileList as WFL
import Web.HTML.HTMLElement as WHH
import Web.HTML.HTMLInputElement as WHI

-- | File Size in KB
foreign import fileSize ∷ String → Int

-- | Compress an image so file sizes are manageable on the server
compressImage ∷ ∀ message. AppId ClientAppId message → Event → Boolean → (Int → Int → String → message) → Aff Unit
compressImage appId event preserveSize message = EC.liftEffect do
      maybeFileList ← WHI.files input
      compressImageFromFileList maybeFileList appId preserveSize message
      where
      input = SU.fromJust do
            target ← WEE.target event
            WHI.fromEventTarget target

foreign import compressImage_ ∷ EffectFn3 File Boolean (Int → Int → String → Effect Unit) Unit

compressImageFromFileList ∷ ∀ message. Maybe FileList → AppId ClientAppId message → Boolean → (Int → Int → String → message) → Effect Unit
compressImageFromFileList maybeFileList appId preserveSize message =
      case maybeFileList >>= WFL.item 0 of
            Nothing → pure unit
            Just file → EU.runEffectFn3 compressImage_ file preserveSize (\w h b → FS.send appId $ message w h b)

triggerFileSelect ∷ Element → Effect Unit
triggerFileSelect = WHH.click <<< SU.fromJust <<< WHH.fromElement
