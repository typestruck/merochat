module Shared.Spec where

import Prelude
import Server.Types
import Shared.Types

import Data.List (List)
import Data.Maybe (Maybe)
import Payload.Server.Handlers (File)
import Payload.Spec (type (:), GET, Guards, Nil, POST, Routes, Spec(..))

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
                  response :: Ok
            },
            login :: Routes "/login" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  get :: GET "/" {
                        response :: Html
                  },
                  --the query parameter next is only used client side
                  post :: POST "/" {
                        body :: RegisterLogin,
                        response :: Ok
                  }
            },
            im :: Routes "/im" {
                  guards :: Guards ("loggedUserID" : Nil),
                  get :: GET "/" {
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
                  },
                  suggestions :: GET "/suggestions?skip=<skip>" {
                        query :: { skip :: Int },
                        response :: Array Suggestion
                  },
                  block :: GET "/block?id=<id>" {
                        query :: { id :: PrimaryKey },
                        response :: Ok
                  },
                  missedMessages :: GET "/missed?lastID=<lastID>" {
                        query :: { lastID :: Int },
                        response :: Array Contact
                  }
            },
            profile :: Routes "/profile" {
                  guards :: Guards ("loggedUserID" : Nil),
                  get :: GET "/" {
                        response :: String
                  },
                  post :: POST "/" {
                        body :: ProfileUser,
                        response :: Ok
                  },
                  generate :: GET "/generate?what=<what>" {
                        query :: { what :: Generate },
                        response :: String
                  }
            },
            settings :: Routes "/settings" {
                  guards :: Guards ("loggedUserID" : Nil),
                  get :: GET "/" {
                        response :: String
                  },
                  account :: Routes "/account" {
                        email :: POST "/account/email"  {
                              body :: String,
                              response :: Ok
                        },
                        password :: POST "/account/password" {
                              body :: String,
                              response :: Ok
                        },
                        terminate :: POST "/account/terminate" {
                              body :: NoBody,
                              response :: Ok
                        }
                  }
            },
            recover :: Routes "/recover" {
                  guards :: Guards ("checkAnonymous" : Nil),
                  get :: GET "/?token=<token>" {
                        query :: { token :: Maybe String },
                        response :: Html
                  },
                  post :: POST "/" {
                        body :: RecoverAccount,
                        response :: Ok
                  },
                  reset :: POST "/recover" {
                        body :: ResetPassword,
                        response :: Ok
                  }
            },
            logout :: POST "/logout" {
                  guards :: Guards ("loggedUserID" : Nil),
                  body :: NoBody,
                  response :: Ok
            },
            terms :: GET "/terms" {
                  response :: Html
            },
            privacy :: GET "/privacy" {
                  response :: Html
            },
            help :: GET "/help" {
                  response :: Html
            },
            developmentFiles :: GET "/client/<..path>" {
                  params :: { path :: List String },
                  response :: File
            },

            --404 can only be matched as a catch all route
            notFound :: GET "/<..notFound>" {
                  params :: { notFound :: List String },
                  response :: Html
            }
      }
}
spec = Spec