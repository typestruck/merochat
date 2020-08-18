module Server.Spec where

import Prelude
import Shared.Types
import Server.Types

import Data.Maybe (Maybe)
import Payload.Spec (type (:), GET, Guards(..), Nil, Spec(..), POST)
import Shared.Types (PrimaryKey(..), RegisterLogin)

spec :: Spec {
      guards :: {
            loggedUserID :: PrimaryKey,
            checkAnonymous :: Unit
      },
      routes :: {
            landing :: GET "/" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  response :: Html
            },
            login :: GET "/login" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  response :: Html
            },
            login' :: POST "/login?next=<next>" {
                  query :: { next :: Maybe String },
                  body :: RegisterLogin,
                  guards :: Guards ("checkAnonymous" : Nil),
                  response :: Html
            },
            im :: GET "/im" {
                  guards :: Guards ("loggedUserID" : Nil),
                  response :: Html
            }
      }
}
spec = Spec