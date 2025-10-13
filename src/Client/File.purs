module Client.File (setUpFileChange, resizeAndSendFirstFile, resizePicture, triggerFileSelect, fileSize) where

import Prelude

import Client.AppId (ClientAppId)
import Client.Dom as CCD
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Effect.Uncurried (EffectFn2)
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

resizePicture ∷ forall message. AppId ClientAppId message → Event → (Int → Int → String → message) → Aff Unit
resizePicture appId event message = EC.liftEffect do
      maybeFileList ← WHI.files input
      resizeAndSendFirstFile maybeFileList appId message
      where
      input = SU.fromJust do
            target ← WEE.target event
            WHI.fromEventTarget target

resizeAndSendFirstFile ∷ ∀ message. Maybe FileList → AppId ClientAppId message → (Int → Int → String → message) → Effect Unit
resizeAndSendFirstFile maybeFileList appId message =
      case maybeFileList >>= WFL.item 0 of
            Nothing → pure unit
            Just file → resizeAndSendFile file (\w h b → FS.send appId $ message w h b)

resizeAndSendFile ∷ File → (Int → Int → String → Effect Unit) → Effect Unit
resizeAndSendFile = EU.runEffectFn2 resizeAndSendFile_

foreign import resizeAndSendFile_ ∷ EffectFn2 File (Int → Int → String → Effect Unit) Unit

triggerFileSelect ∷ Element → Effect Unit
triggerFileSelect = WHH.click <<< SU.fromJust <<< WHH.fromElement

setUpFileChange ∷ ∀ message. Warn (Text "Deprecated") => (Int → Int → String → message) → Element → AppId ClientAppId message → Effect Unit
setUpFileChange message input appId = do
      CCD.addEventListener input change $ \_ → do
            let htmlInput = SU.fromJust $ WHI.fromElement input
            maybeFileList ← WHI.files htmlInput
            resizeAndSendFirstFile maybeFileList appId message

