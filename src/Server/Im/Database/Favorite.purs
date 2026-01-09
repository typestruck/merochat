module Server.Im.Database.Favorite where

import Droplet.Driver
import Droplet.Language
import Prelude

import Data.Tuple.Nested ((/\))
import Server.Database as SD
import Server.Database.Fields (_id, _recipient, _sender)
import Server.Database.Histories (_favorite, histories)
import Shared.Im.Types (Favorited)

fetchHistory ∷ Connection → Int → Int → _
fetchHistory connection loggedUserId userId =
      SD.singleWith connection $ select (_id /\ _favorite /\ _sender) # from histories # wher (_sender .=. loggedUserId .&&. _recipient .=. userId .||. _sender .=. userId .&&. _recipient .=. loggedUserId)

updateFavorite ∷ Connection → Int -> Favorited → _
updateFavorite connection id favorited =
      SD.executeWith connection $ update histories # set (_favorite .=. favorited) # wher (_id .=. id)
