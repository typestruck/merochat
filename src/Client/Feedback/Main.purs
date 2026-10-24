module Client.Feedback.Main where

import Prelude

import Client.AppId (feedbackAppId)
import Client.Lynx.Screen as CLS
import Client.Feedback.Update as CFU
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application)
import Flame as F
import Shared.Feedback.Types (FeedbackMessage, FeedbackModel)
import Shared.Feedback.View as SFV

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount feedbackAppId

application ∷ Aff (Application FeedbackModel FeedbackMessage)
application = do
      let model = { feedbackStatus: Nothing, screenshot: Nothing, visible: true, comments: "", loading: false }
      pure
            { model
            , view: SFV.view >>> pure >>> CLS.view feedbackAppId
            , update: CFU.update
            , subscribe: []
            }
