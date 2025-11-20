module Client.File (setUpFileChange, compressImageFromFileList, compressImage, triggerFileSelect, fileSize) where

import Prelude

import Client.AppId (ClientAppId)
import Client.Dom as CCD
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Uncurried (EffectFn3)
import Effect.Uncurried as EU
import Flame (AppId)
import Flame.Subscription as FS
import Prim.TypeError (class Warn, Text)
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.Event (Event)
import Web.Event.Event as WEE
import Web.File.File (File)
import Web.File.FileList (FileList)
import Web.File.FileList as WFL
import Web.HTML.Event.EventTypes (change)
import Web.HTML.HTMLElement as WHH
import Web.HTML.HTMLInputElement as WHI

foreign import fileSize ∷ String → Int

compressImage ∷ ∀ message. AppId ClientAppId message → Event → Boolean → (Int → Int → String → message) → Aff Unit
compressImage appId event preserveSize message = EC.liftEffect do
      maybeFileList ← WHI.files input
      compressImageFromFileList maybeFileList appId preserveSize message
      where
      input = SU.fromJust do
            target ← WEE.target event
            WHI.fromEventTarget target

compressImageFromFileList ∷ ∀ message. Maybe FileList → AppId ClientAppId message → Boolean → (Int → Int → String → message) → Effect Unit
compressImageFromFileList maybeFileList appId preserveSize message =
      case maybeFileList >>= WFL.item 0 of
            Nothing → pure unit
            Just file → EU.runEffectFn3 compressImage_ file preserveSize (\w h b → FS.send appId $ message w h b)

foreign import compressImage_ ∷ EffectFn3 File Boolean (Int → Int → String → Effect Unit) Unit

triggerFileSelect ∷ Element → Effect Unit
triggerFileSelect = WHH.click <<< SU.fromJust <<< WHH.fromElement

setUpFileChange ∷ ∀ message. Warn (Text "Deprecated") ⇒ (Int → Int → String → message) → Element → AppId ClientAppId message → Effect Unit
setUpFileChange message input appId = do
      CCD.addEventListener input change $ \_ → do
            let htmlInput = SU.fromJust $ WHI.fromElement input
            maybeFileList ← WHI.files htmlInput
            compressImageFromFileList maybeFileList appId false message

