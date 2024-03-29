module Server.Recover.Database where

import Droplet.Language
import Prelude
import Server.Database.Fields
import Server.Database.Recoveries
import Server.Database.Users
import Server.Effect

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Server.Database as SD

--REFACTOR: consider making the uuid token the primary key
insertRecover ∷ Int → String → ServerEffect Unit
insertRecover id token = void <<< SD.execute $ insert # into recoveries (_uuid /\ _recoverer) # values (token /\ id)

--refactor: doesn't need to calculate the date with sql
selectRecoverer ∷ String → ServerEffect (Maybe Int)
selectRecoverer token = do
      result ← SD.unsafeSingle "select recoverer from recoveries where uuid = @token and active = true and created >= (utc_now()) - interval '1 day'" { token } ∷ BaseEffect _ (Maybe { recoverer ∷ Int })
      pure (_.recoverer <$> result)

recoverPassword ∷ String → Int → String → ServerEffect Unit
recoverPassword token id password = SD.withTransaction $ \connection → do
      SD.executeWith connection $ update recoveries # set (_active .=. false) # wher (_uuid .=. token)
      SD.executeWith connection $ update users # set (_password .=. Just password) # wher (_id .=. id)