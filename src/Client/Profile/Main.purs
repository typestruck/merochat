module Client.Profile.Main where

import Prelude

import Client.Common.DOM as CCD
import Client.Common.Notification as CCN
import Client.Profile.Update as CPU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import Effect.Uncurried as EU
import Flame.Application.Effectful as FAE
import Foreign as F
import Shared.DateTime as SDT
import Shared.Profile.Types (Editors, ProfileMessage(..))
import Shared.Profile.View as SPV
import Shared.Types (Editor)
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

--loads the simplemde editors that are used for in place editing because content editable sucks balls
-- the editor for name and headline does not accept new lines
foreign import loadEditors :: Effect (Editors Editor Editor Editor)
foreign import blur_ :: EffectFn2 Editor (EffectFn1 String Unit) Unit

main :: Effect Unit
main = do
        minimumYear <- SDT.getMinimumYear
        channel <- FAE.resumeMount (QuerySelector ".profile-info-edition") {
                view: SPV.view minimumYear,
                init: Nothing,
                update: CPU.update
        }

        editors <- loadEditors
        EU.runEffectFn2 blur_ editors.name $ EU.mkEffectFn1 (SC.send channel <<< Just <<< SetName)
        EU.runEffectFn2 blur_ editors.headline $ EU.mkEffectFn1 (SC.send channel <<< Just <<< SetHeadline)
        EU.runEffectFn2 blur_ editors.description $ EU.mkEffectFn1 (SC.send channel <<< Just <<< SetDescription)
        SC.send channel <<< Just $ SetEditors editors

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
                fileList <- SU.fromJust "client.profile.main" <$> WHI.files (SU.fromJust "client.profile.main" $ WHI.fromElement input)
                let file = SU.fromJust "client.profile.main" $ WFL.item 0 fileList

                if WFF.size file > 500.0 * 1024.0 then
                        CCN.alert "Max allowed size for avatar is 500kb"
                 else
                        WFR.readAsDataURL (WFF.toBlob file) fileReader
