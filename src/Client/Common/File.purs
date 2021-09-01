module Client.Common.File where

import Prelude

import Client.Common.DOM as CCD
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame (AppId)
import Flame.Subscription as FS
import Foreign as F
import Shared.Types (MountPoint)
import Shared.Unsafe as SU
import Web.DOM (Element)
import Web.Event.EventTarget as WET
import Web.File.File as WFF
import Web.File.FileList (FileList)
import Web.File.FileList as WFL
import Web.File.FileReader (FileReader)
import Web.File.FileReader as WFR
import Web.HTML.Event.EventTypes (change, load)
import Web.HTML.HTMLElement as WHH
import Web.HTML.HTMLInputElement as WHI

triggerFileSelect :: Element -> Effect Unit
triggerFileSelect = WHH.click <<< SU.fromJust <<< WHH.fromElement

setUpFileChange :: forall message. (String -> message) -> Element -> AppId MountPoint message -> Effect Unit
setUpFileChange message input appID = do
      fileReader <- WFR.fileReader
      setUpBase64Reader fileReader message appID
      CCD.addEventListener input change $ \_ -> do
            let htmlInput = SU.fromJust $ WHI.fromElement input
            maybeFileList <- WHI.files htmlInput
            readBase64 fileReader maybeFileList
            WHI.setValue "" htmlInput

setUpBase64Reader :: forall message. FileReader -> (String -> message) -> AppId MountPoint message -> Effect Unit
setUpBase64Reader fileReader message appID = do
      handler <- WET.eventListener $ \_ -> do
            foreignBase64 <- WFR.result fileReader
            FS.send appID $ message (F.unsafeFromForeign foreignBase64 :: String)
      WET.addEventListener load handler false $ WFR.toEventTarget fileReader

readBase64 :: FileReader -> Maybe FileList -> Effect Unit
readBase64 fileReader maybeFileList = do
      let
            maybeFile = do
                  fileList <- maybeFileList
                  WFL.item 0 fileList
      case maybeFile of
            Nothing -> pure unit
            Just file -> WFR.readAsDataURL (WFF.toBlob file) fileReader