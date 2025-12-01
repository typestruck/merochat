module Client.Im.Changelog where

import Prelude

import Client.Im.Flame (NextMessage, NoMessages, MoreMessages)
import Client.Network (request)
import Client.Network as CCN
import Client.Network as CNN
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Shared.Changelog (Changelog, ChangelogAction(..))
import Shared.Im.Types (ImMessage(..), ImModel, RetryableRequest(..))
import Shared.Modal (Modal(..), ScreenModal(..))
import Shared.Options.Doppelganger as SOD
import Shared.Unsafe as SU

fetchChangelog ∷ ImModel → NextMessage
fetchChangelog model = model /\ [ fetchIt ]
      where
      fetchIt = do
            changes ← CNN.silentResponse $ request.im.changelog.get { query: { before: _.id <$> DA.last model.changelogs } }
            pure <<< Just $ DisplayChangelog changes

displayChangelog ∷ Array Changelog → ImModel → NoMessages
displayChangelog changelogs model = model { changelogs = changelogs } /\ []

performChangelogAction ∷ Maybe ChangelogAction → Maybe Int → ImModel → MoreMessages
performChangelogAction action value model = model /\ effects
      where
      effects = case action of
            Nothing → []
            Just OpenBackerPage → [ pure <<< Just $ SpecialRequest <<< ToggleModal $ Screen ShowBacker ]
            Just OpenExperimentsPage → [ pure <<< Just $ SpecialRequest <<< ToggleModal $ Screen ShowExperiments ]
            Just OpenProfilePage → [ pure <<< Just $ SpecialRequest <<< ToggleModal $ Screen ShowProfile ]
            Just SendDoppelgangerMessage → [ pure <<< Just $ MessageFromExperiment (SU.fromJust value) SOD.message ]

toggleChangelog ∷ ImModel → NoMessages
toggleChangelog model =
      if shown then
            model { showChangelogs = true } /\ []
      else
            model { showChangelogs = false, changelogs = (_ { read = true }) <$> model.changelogs } /\ [ readIt ]
      where
      shown = not model.showChangelogs

      readIt = do
            let ids = map _.id $ DA.filter (not <<< _.read) model.changelogs
            unless (DA.null ids) <<< void <<< CCN.silentResponse $ request.im.changelog.post { body: { ids } }
            pure Nothing