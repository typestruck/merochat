module Client.Common.File where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.Notification as CCN
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign as F
import Shared.File (maxImageSize, maxImageSizeKB)
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
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

setUpFileChange :: forall message. (String -> message) -> Element -> Channel message -> Effect Unit
setUpFileChange message input channel = do
      fileReader <- WFR.fileReader
      setUpBase64Reader fileReader message channel
      CCD.addEventListener input change $ \_ -> do
            maybeFileList <- WHI.files (SU.fromJust $ WHI.fromElement input)
            readBase64 fileReader maybeFileList

setUpBase64Reader :: forall message. FileReader -> (String -> message) -> Channel message -> Effect Unit
setUpBase64Reader fileReader message channel = do
      handler <- WET.eventListener $ \_ -> do
            foreignBase64 <- WFR.result fileReader
            SC.send channel $ message (F.unsafeFromForeign foreignBase64 :: String)
      WET.addEventListener load handler false $ WFR.toEventTarget fileReader

readBase64 :: FileReader -> Maybe FileList -> Effect Unit
readBase64 fileReader maybeFileList = do
      let   maybeFile = do
                  fileList <- maybeFileList
                  WFL.item 0 fileList
      case maybeFile of
            Nothing -> pure unit
            Just file ->
                  if WFF.size file > DI.toNumber maxImageSize then
                        CCN.alert $ "Max allowed size for images is " <> maxImageSizeKB
                  else
                        WFR.readAsDataURL (WFF.toBlob file) fileReader