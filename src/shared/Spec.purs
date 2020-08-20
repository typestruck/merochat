module Shared.Spec where

import Prelude
import Server.Types
import Shared.Types

import Data.List (List)
import Payload.Server.Handlers (File)
import Payload.Spec (type (:), GET, Guards, Nil, POST, Routes, Spec(..))

--refactor: consider having a separated spec for whats client callable
spec :: Spec {
      guards :: {
            loggedUserID :: PrimaryKey,
            checkAnonymous :: Unit
      },
      routes :: {
            landing :: Routes "/" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  landing :: GET "/" {
                        response :: Html
                  },
                  register :: POST "/register" {
                        body :: RegisterLogin,
                        response :: AOk
                  }
            },
            login :: Routes "/login" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  get :: GET "/" {
                        response :: Html
                  },
                  --the query parameter next is only used client side
                  post :: POST "/" {
                        body :: RegisterLogin,
                        response :: AOk
                  }
            },
            im :: Routes "/im" {
                  guards :: Guards ("loggedUserID" : Nil),
                  im :: GET "/" {
                        response :: Html
                  },
                  contacts :: GET "/contacts?skip=<skip>" {
                        query :: { skip :: Int },
                        response :: Array Contact
                  },
                  singleContact :: GET "/contact?id=<id>" {
                        query :: { id :: PrimaryKey },
                        response :: Array Contact
                  },
                  history :: GET "/history?with=<with>&skip=<skip>" {
                        query :: { skip :: Int, with :: PrimaryKey },
                        response :: Array HistoryMessage
                  }
            },
            --404 can only be matched as a catch all route
            notFound :: GET "/<..notFound>" {
                  params :: { notFound :: List String },
                  response :: Html
            },

            developmentFiles :: GET "/client/<..path>" {
                  params :: { path :: List String },
                  response :: File
            }
      }
}
spec = Spec