module Shared.Spec where

import Prelude
import Server.Ok
import Shared.Im.Types
import Shared.Profile.Types
import Shared.User

import Data.List (List)
import Data.Maybe (Maybe)
import Payload.Server.Handlers (File)
import Payload.Spec (type (:), GET, Guards, Nil, POST, Routes, Spec(..))
import Shared.Account (EmailCaptcha, EmailPasswordCaptcha, RegisterTemporary, ResetPassword, EmailPassword)
import Shared.Changelog (Changelog)
import Shared.DateTime (DateTimeWrapper, DateWrapper)
import Shared.Html (Html)
import Shared.Post (Post)
import Shared.Profile.Types (SavedFields)
import Shared.Settings.Types (PrivacySettings)

spec ∷
      Spec
            { guards ∷
                    { loggedUserId ∷ Int
                    , checkAnonymous ∷ Unit
                    }
            , routes ∷
                    { landing ∷
                            GET "/"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , response ∷ Html
                                  }
                    , temporary ∷
                            POST "/temporary"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , response ∷ Ok
                                  }
                    , register ∷
                            POST "/register"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , body ∷ EmailPasswordCaptcha
                                  , response ∷ Ok
                                  }
                    , unsubscribe ∷
                            GET "/unsubscribe?email_id=<emailId>"
                                  { guards ∷ Guards Nil
                                  , query ∷ { emailId ∷ String }
                                  , response ∷ Html
                                  }
                    , login ∷
                            Routes "/login"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ Html
                                                }
                                  ,
                                    --the query parameter next is only used client side
                                    post ∷
                                          POST "/"
                                                { body ∷ EmailPassword
                                                , response ∷ Ok
                                                }
                                  }
                    , posts ∷
                            Routes "/posts"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/?poster=<poster>"
                                                { query ∷ { poster ∷ Int }
                                                , response ∷ Array Post
                                                }
                                  }
                    , im ∷
                            Routes "/im"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ String
                                                }
                                  , subscribe ∷
                                          POST "/subscribe"
                                                { response ∷ Ok
                                                }
                                  , register ∷
                                          POST "/register"
                                                { body ∷ EmailPassword
                                                , response ∷ Ok
                                                }
                                  , contacts ∷
                                          GET "/contacts?skip=<skip>"
                                                { query ∷ { skip ∷ Int }
                                                , response ∷ Array Contact
                                                }
                                  , changelog ∷
                                          Routes "/changelog"
                                                { get ∷
                                                        GET "/?before=<before>"
                                                              { query ∷ { before ∷ Maybe Int }
                                                              , response ∷ Array Changelog
                                                              }
                                                , post ∷
                                                        POST "/"
                                                              { body ∷ { ids ∷ Array Int }
                                                              , response ∷ Ok
                                                              }
                                                }
                                  , contact ∷
                                          GET "/contact?id=<id>"
                                                { query ∷ { id ∷ Int }
                                                , response ∷ Array Contact
                                                }
                                  , history ∷
                                          GET "/history?with=<with>&skip=<skip>"
                                                { query ∷ { skip ∷ Int, with ∷ Int }
                                                , response ∷ Array HistoryMessage
                                                }
                                  , suggestions ∷
                                          GET "/suggestions?skip=<skip>&sg=<sg>"
                                                { query ∷ { skip ∷ Int, sg ∷ SuggestionsFrom }
                                                , response ∷ Array Suggestion
                                                }
                                  , block ∷
                                          POST "/block"
                                                { body ∷ { id ∷ Int }
                                                , response ∷ Ok
                                                }
                                  , delete ∷
                                          POST "/delete"
                                                { body ∷ { userId ∷ Int, messageId ∷ Int }
                                                , response ∷ Ok
                                                }
                                  , missedContacts ∷
                                          GET "/missed?since=<since>&last=<last>"
                                                { query ∷ { since ∷ DateTimeWrapper, last ∷ Maybe Int }
                                                , response ∷ Array Contact
                                                }
                                  , fortune ∷
                                          GET "/fortune"
                                                { response ∷ String
                                                }
                                  , report ∷
                                          POST "/report"
                                                { body ∷ Report
                                                , response ∷ Ok
                                                }

                                  , tutorial ∷
                                          POST "/tutorial"
                                                { response ∷ Ok
                                                }
                                  , greeting ∷
                                          POST "/greeting"
                                                { response ∷ Ok
                                                }
                                  }
                    , profile ∷
                            Routes "/profile"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ String
                                                }
                                  , generated ∷
                                          POST "/generated"
                                                { body ∷ GeneratedInput
                                                , response ∷ String
                                                }
                                  , save ∷
                                          POST "/save"
                                                { body ∷ SavedFields
                                                , response ∷ { avatar ∷ Maybe String }
                                                }
                                  }
                    , settings ∷
                            Routes "/settings"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ String
                                                }
                                  , account ∷
                                          Routes "/account"
                                                { email ∷
                                                        POST "/account/email"
                                                              { body ∷ { email ∷ String }
                                                              , response ∷ Ok
                                                              }
                                                , password ∷
                                                        POST "/account/password"
                                                              { body ∷ { password ∷ String }
                                                              , response ∷ Ok
                                                              }
                                                , terminate ∷
                                                        POST "/account/terminate"
                                                              { response ∷ Ok
                                                              }
                                                , privacy ∷
                                                        POST "/account/privacy"
                                                              { body ∷ PrivacySettings
                                                              , response ∷ Ok
                                                              }
                                                }
                                  }
                    , recover ∷
                            Routes "/recover"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , get ∷
                                          GET "/?token=<token>"
                                                { query ∷ { token ∷ Maybe String }
                                                , response ∷ Html
                                                }
                                  , post ∷
                                          POST "/"
                                                { body ∷ EmailCaptcha
                                                , response ∷ Ok
                                                }
                                  , reset ∷
                                          POST "/recover"
                                                { body ∷ ResetPassword
                                                , response ∷ Ok
                                                }
                                  }
                    , logout ∷
                            POST "/logout"
                                  { guards ∷ Guards ("loggedUserId" : Nil)

                                  , response ∷ Ok
                                  }
                    , leaderboard ∷
                            GET "/leaderboard"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , response ∷ String
                                  }
                    , help ∷
                            GET "/help"
                                  { response ∷ Html
                                  }
                    , feedback ∷
                            Routes "/feedback"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ String
                                                }
                                  , send ∷
                                          POST "/send"
                                                { body ∷ { comments ∷ String, screenshot ∷ Maybe String }
                                                , response ∷ Ok
                                                }
                                  }

                    , internalHelp ∷
                            GET "/inhelp"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , response ∷ String
                                  }
                    , experiments ∷
                            GET "/experiments"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , response ∷ String
                                  }
                    , backer ∷
                            GET "/backer"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , response ∷ Html
                                  }
                    , internalBacker ∷
                            GET "/inbacker"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , response ∷ String
                                  }

                    , banned ∷
                            GET "/banned"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , response ∷ Html
                                  }
                    , admin ∷
                            Routes "/admin"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , ban ∷
                                          POST "/ban?id=<id>&secret=<secret>"
                                                { query ∷ { id ∷ Int, secret ∷ String }
                                                , response ∷ Ok
                                                }
                                  }
                    , topic ∷
                            POST "/topic/<..path>"
                                  { params ∷ { path ∷ List String }
                                  , body ∷ String
                                  , response ∷ Ok
                                  }
                    , sw ∷
                            GET "/sw.js"
                                  { response ∷ File
                                  }
                    , developmentFiles ∷
                            GET "/file/<..path>"
                                  { params ∷ { path ∷ List String }
                                  , response ∷ File
                                  }
                    ,
                      --404 can only be matched as a catch all route
                      notFound ∷
                            GET "/<..notFound>"
                                  { params ∷ { notFound ∷ List String }
                                  , response ∷ Html
                                  }
                    }
            }
spec = Spec