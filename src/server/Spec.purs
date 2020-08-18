module Server.Spec where

import Server.Types

import Payload.Spec (type (:), GET, Guards(..), Spec(..), Nil)
import Shared.Types (PrimaryKey(..))

spec :: Spec {
      guards :: {
            loggedUserID :: PrimaryKey
      },
      routes :: {
            landing :: GET "/" {
                  response :: Html
            },
            im :: GET "/im" {
                  guards :: Guards ("loggedUserID" : Nil),
                  response :: Html
            }
      }
}
spec = Spec