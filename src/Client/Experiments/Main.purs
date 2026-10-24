module Client.Experiments.Main where

import Prelude

import Client.AppId (experimentsAppId)
import Client.Lynx.Screen as CLS
import Client.Experiments.Update as CEU
import Client.Network (routes)
import Client.Network as CN
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as EA
import Effect.Class as EC
import Flame (Application)
import Flame as F
import Shared.Experiments.Types (ExperimentsMessage, ExperimentsModel, DoppelgangerSection(..), DebateSection(..), PaperPlaneSection(..))
import Shared.Experiments.View as SEV

main ∷ Effect Unit
main = EA.launchAff_ $ application >>= EC.liftEffect <<< F.mount experimentsAppId

application ∷ Aff (Application ExperimentsModel ExperimentsMessage)
application = do
      payload ← CN.silentRequest $ routes.experiments.initialize {}
      let model =
            { experiments: payload.experiments
            , user: payload.user
            , visible: true
            , current: Nothing
            , doppelganger: { questions: [], matches: [], completed: payload.completedDoppelganger, loading: false, section: ShowDoppelganger, selectedChoice: Nothing }
            , debate: { section: ShowNewDebate, topic: Nothing, statement: Nothing, loading: false, mine: payload.mine, showFormat: false, inFavor: Nothing }
            , paperPlane: { loading: false, section: ShowNew, message: Nothing, thrown: payload.thrown, flyingBy: payload.flyingBy, caught: payload.caught }
            }
      pure
            { model
            , view: SEV.view >>> pure >>> CLS.view experimentsAppId
            , update: CEU.update
            , subscribe: []
            }
