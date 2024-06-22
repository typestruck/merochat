module Server.Unsubscribe.Database where

import Droplet.Language
import Prelude hiding (not, join)
import Server.Database.UnsubscribeTokens
import Server.Database.Users

import Data.Maybe (Maybe)
import Server.Database as SD
import Server.Database.Fields (_contents, _id)
import Server.Effect (ServerEffect)
import Shared.User (ReceiveEmail(..))

fetchUnsubscriber ∷ String → ServerEffect (Maybe Int)
fetchUnsubscriber token = do
      row ← SD.single $ select _unsubscriber # from unsubscribeTokens # wher (_contents .=. token)
      pure $ map _.unsubscriber row

unsubscribe ∷ Int → ServerEffect Unit
unsubscribe id = do
      SD.execute $ update users # set (_receiveEmail .=. NoEmails) # wher (_id .=. id)
      SD.execute $ delete # from unsubscribeTokens # wher (_unsubscriber .=. id)