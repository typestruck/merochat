module Client.Common.File where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.Notification as CCN
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign as F
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.DOM (Element)
import Web.Event.EventTarget as WET
import Web.File.File as WFF
import Web.File.FileList as WFL
import Web.File.FileReader as WFR
import Web.HTML.Event.EventTypes (change, load)
import Web.HTML.HTMLElement as WHH
import Web.HTML.HTMLInputElement as WHI

triggerFileSelect :: Element -> Effect Unit
triggerFileSelect = WHH.click <<< SU.fromJust <<< WHH.fromElement

setUpFileChange :: forall message. (String -> message) -> Element -> Channel message -> Effect Unit
setUpFileChange message input channel = do
      fileReader <- WFR.fileReader
      handler <- WET.eventListener $ \_ -> do
            foreignBase64 <- WFR.result fileReader
            SC.send channel $ message (F.unsafeFromForeign foreignBase64 :: String)
      WET.addEventListener load handler false $ WFR.toEventTarget fileReader

      CCD.addEventListener input change $ \_ -> do
            maybeFileList <- WHI.files (SU.fromJust $ WHI.fromElement input)
            let   file = SU.fromJust do
                        fileList <- maybeFileList
                        WFL.item 0 fileList

            if WFF.size file > 500.0 * 1024.0 then
                  CCN.alert "Max allowed size for images is 500kb"
             else
                  WFR.readAsDataURL (WFF.toBlob file) fileReader
