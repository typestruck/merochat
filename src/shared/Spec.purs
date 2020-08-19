module Shared.Spec where

import Prelude
import Server.Types
import Shared.Types

import Data.List (List)
import Payload.Server.Handlers (File)
import Payload.Spec (type (:), GET, Guards, Nil, POST, Spec(..))
import Shared.Types (PrimaryKey, RegisterLogin)

--refactor: consider having a separated spec for whats client callable
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
            register :: POST "/register" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  body :: RegisterLogin,
                  response :: AOk
            },
            login :: GET "/login" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  response :: Html
            },
            --the query parameter next is only used client side
            logon :: POST "/login" {
                  body :: RegisterLogin,
                  guards :: Guards ("checkAnonymous" : Nil),
                  response :: AOk
            },
            im :: GET "/im" {
                  guards :: Guards ("loggedUserID" : Nil),
                  response :: Html
            },

            developmentFiles :: GET "/client/<..path>" {
                  params :: { path :: List String },
                  response :: File
            }
      }
}
spec = Spec