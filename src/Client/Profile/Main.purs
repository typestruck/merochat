module Client.Profile.Main where

import Prelude

import Client.Common.DOM as CCD
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Application.Effectful as FAE
import Foreign as F
import Shared.Profile.Types (ProfileMessage(..))
import Shared.Profile.View as SPV
import Shared.Unsafe as SU
import Signal.Channel (Channel)
import Signal.Channel as SC
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.EventTarget as WET
import Web.File.File as WFF
import Web.File.FileList as WFL
import Web.File.FileReader as WFR
import Web.HTML.Event.EventTypes (change, load)
import Web.HTML.HTMLInputElement as WHI

main :: Effect Unit
main = do
        channel <- FAE.resumeMount (QuerySelector ".profile-info-edition") {
                view: SPV.view,
                init: Nothing,
                update: CPU.update
        }

        setUpAvatarChange channel

setUpAvatarChange :: Channel (Maybe ProfileMessage) -> Effect Unit
setUpAvatarChange channel = do
        fileReader <- WFR.fileReader
        handler <- WET.eventListener $ \_ -> do
                foreignBase64 <- WFR.result fileReader
                SC.send channel <<< Just $ SetAvatar (F.unsafeFromForeign foreignBase64 :: String)
        WET.addEventListener load handler false $ WFR.toEventTarget fileReader

        input <- CPU.getFileInput
        CCD.addEventListener input change $ \_ -> do
                fileList <- SU.unsafeFromJust "client.profile.main" <$> WHI.files (SU.unsafeFromJust "client.profile.main" $ WHI.fromElement input)
                let file = SU.unsafeFromJust "client.profile.main" $ WFL.item 0 fileList

                WFR.readAsDataURL (WFF.toBlob file) fileReader
