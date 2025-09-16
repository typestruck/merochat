module Client.Im.Changelog where

import Prelude

import Client.Common.Network (request)
import Client.Common.Network as CCN
import Client.Common.Network as CNN
import Client.Im.Flame (NextMessage, NoMessages)
import Data.Array as DA
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Shared.Changelog (Changelog)
import Shared.Im.Types (ImMessage(..), ImModel)

fetchChangelog ∷ ImModel → NextMessage
fetchChangelog model = model /\ [ fetchIt ]
      where
      fetchIt = do
            changes ← CNN.silentResponse $ request.im.changelog.get { query: { before: _.id <$> DA.last model.changelogs } }
            pure <<< Just $ DisplayChangelog changes

displayChangelog ∷ Array Changelog → ImModel → NoMessages
displayChangelog changelogs model = model { changelogs = changelogs } /\ []

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