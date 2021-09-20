module Server.Settings.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.Users

import Server.Database as SD
import Server.Types (ServerEffect)
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility)

changeEmail ∷ Int → String → ServerEffect Unit
changeEmail loggedUserID email = SD.execute $ update users # set (_email .=. email) # wher (_id .=. loggedUserID)

changePassword ∷ Int → String → ServerEffect Unit
changePassword loggedUserID password = SD.execute $ update users # set (_password .=. password) # wher (_id .=. loggedUserID)

terminateAccount ∷ Int → ServerEffect Unit
terminateAccount loggedUserID = SD.execute $ delete # from users # wher (_id .=. loggedUserID) --cascades

profileVisibility ∷ Int → ServerEffect ProfileVisibility
profileVisibility loggedUserID = _.visibility <<< SU.fromJust <$> (SD.single $ select _visibility # from users # wher (_id .=. loggedUserID))

changeVisibility :: Int -> ProfileVisibility -> ServerEffect Unit
changeVisibility loggedUserId pv = SD.execute $ update users # set (_visibility .=. pv) # wher (_id .=. loggedUserId)
