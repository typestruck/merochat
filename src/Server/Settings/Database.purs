module Server.Settings.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.Users

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Now as EN
import Server.Database as SD
import Server.Database.Types (Checked(..))
import Server.Settings.Database.Flat as SSDF
import Server.Types (ServerEffect)
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU
import Shared.User (ProfileVisibility)
import Type.Proxy (Proxy(..))

changeEmail ∷ Int → String → ServerEffect Unit
changeEmail loggedUserId email = SD.execute $ update users # set (_email .=. email) # wher (_id .=. loggedUserId)

changePassword ∷ Int → String → ServerEffect Unit
changePassword loggedUserId password = SD.execute $ update users # set (_password .=. password) # wher (_id .=. loggedUserId)

terminateAccount ∷ Int → ServerEffect Unit
terminateAccount loggedUserId = SD.execute $ delete # from users # wher (_id .=. loggedUserId) --cascades

privacySettings ∷ Int → ServerEffect PrivacySettings
privacySettings loggedUserId = SSDF.toPrivacySettings <<< SU.fromJust <$>
      ( SD.single $
              select
                    ( (_visibility # as profileVisibility)
                            /\ (_readReceipts # as readReceipts)
                            /\ (_typingStatus # as typingStatus)
                            /\ (_onlineStatus # as onlineStatus)
                            /\ (_messageTimestamps # as messageTimestamps)
                    ) # from users # wher (_id .=. loggedUserId)
      )

changePrivacySettings ∷ Int → PrivacySettings → ServerEffect Unit
changePrivacySettings loggedUserId { readReceipts, typingStatus, profileVisibility, onlineStatus, messageTimestamps } = do
      now ← liftEffect EN.nowDateTime
      SD.execute $ update users
            # set
                    ( (_visibility .=. profileVisibility)
                            /\ (_visibility_last_updated .=. now)
                            /\ (_readReceipts .=. Checked readReceipts)
                            /\ (_typingStatus .=. Checked typingStatus)
                            /\ (_onlineStatus .=. Checked onlineStatus)
                            /\ (_messageTimestamps .=. Checked messageTimestamps)
                    )
            # wher (_id .=. loggedUserId)
