module Client.Common.File (setUpFileChange, resizeAndSendFirstFile, triggerFileSelect) where

import Prelude

import Client.Common.Dom as CCD
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn2)
import Effect.Uncurried as EU
import Flame (AppId)
import Flame.Subscription as FS
import Shared.Options.MountPoint (MountPoint)
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.File.File (File)
import Web.File.FileList (FileList)
import Web.File.FileList as WFL
import Web.HTML.Event.EventTypes (change)
import Web.HTML.HTMLElement as WHH
import Web.HTML.HTMLInputElement as WHI

foreign import resizeAndSendFile_ ∷ EffectFn2 File (String → Effect Unit) Unit

resizeAndSendFile ∷ File → (String → Effect Unit) → Effect Unit
resizeAndSendFile = EU.runEffectFn2 resizeAndSendFile_

triggerFileSelect ∷ Element → Effect Unit
triggerFileSelect = WHH.click <<< SU.fromJust <<< WHH.fromElement

setUpFileChange ∷ ∀ message. (String → message) → Element → AppId MountPoint message → Effect Unit
setUpFileChange message input appId = do
      CCD.addEventListener input change $ \_ → do
            let htmlInput = SU.fromJust $ WHI.fromElement input
            maybeFileList ← WHI.files htmlInput
            resizeAndSendFirstFile maybeFileList appId message

resizeAndSendFirstFile ∷ ∀ message. Maybe FileList → AppId MountPoint message → (String → message) → Effect Unit
resizeAndSendFirstFile maybeFileList appId message =
      case maybeFileList >>= WFL.item 0 of
            Nothing → pure unit
            Just file → resizeAndSendFile file (FS.send appId <<< message)
