module Server.Settings.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.Users

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Droplet.Driver (Pool)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Server.Database as SD
import Server.Database.Types (Checked(..))
import Server.Effect (BaseEffect, ServerEffect)
import Server.Settings.Database.Flat as SSDF
import Shared.Settings.Types (PrivacySettings)
import Shared.Unsafe as SU

changeEmail ∷ Int → String → ServerEffect Unit
changeEmail loggedUserId email = SD.execute $ update users # set (_email .=. Just email) # wher (_id .=. loggedUserId)

changePassword ∷ Int → String → ServerEffect Unit
changePassword loggedUserId password = SD.execute $ update users # set (_password .=. Just password) # wher (_id .=. loggedUserId)

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
                            /\ (_postsVisibility # as postsVisibility)
                            /\ (_messageTimestamps # as messageTimestamps)
                    ) # from users # wher (_id .=. loggedUserId)
      )

changePrivacySettings ∷ ∀ r. Int → PrivacySettings → BaseEffect { pool ∷ Pool | r } Unit
changePrivacySettings loggedUserId ps = do
      now ← liftEffect EN.nowDateTime
      SD.execute $ update users
            # set
                    ( (_visibility .=. ps.profileVisibility)
                            /\ (_visibility_last_updated .=. now)
                            /\ (_readReceipts .=. Checked ps.readReceipts)
                            /\ (_typingStatus .=. Checked ps.typingStatus)
                            /\ (_onlineStatus .=. Checked ps.onlineStatus)
                            /\ (_messageTimestamps .=. Checked ps.messageTimestamps)
                            /\ (_postsVisibility .=. ps.postsVisibility)
                    )
            # wher (_id .=. loggedUserId)

saveChatBackground ∷ Int → String → ServerEffect Unit
saveChatBackground loggedUserId fileName = SD.execute $ update users # set (_chatBackground .=. Just fileName) # wher (_id .=. loggedUserId)