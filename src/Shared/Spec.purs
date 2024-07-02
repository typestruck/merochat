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
import Shared.Account (RecoverAccount, RegisterLogin, ResetPassword, RegisterTemporary)
import Shared.DateTime (DateWrapper)
import Shared.Html (Html)
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
                                  , body ∷ RegisterTemporary
                                  , response ∷ Ok
                                  }
                    , register ∷
                            POST "/register"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , body ∷ RegisterLogin
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
                                                { body ∷ RegisterLogin
                                                , response ∷ Ok
                                                }
                                  }
                    , im ∷
                            Routes "/im"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ String
                                                }
                                  , register ∷
                                          POST "/register"
                                                { body ∷ { email ∷ String, password ∷ String }
                                                , response ∷ Ok
                                                }
                                  , contacts ∷
                                          GET "/contacts?skip=<skip>"
                                                { query ∷ { skip ∷ Int }
                                                , response ∷ Array Contact
                                                }
                                  , contact ∷
                                          GET "/contact?id=<id>&impersonation=<impersonation>"
                                                { query ∷ { id ∷ Int, impersonation ∷ Boolean }
                                                , response ∷ Array Contact
                                                }
                                  , history ∷
                                          GET "/history?with=<with>&skip=<skip>"
                                                { query ∷ { skip ∷ Int, with ∷ Int }
                                                , response ∷ Array HistoryMessage
                                                }
                                  , suggestions ∷
                                          GET "/suggestions?skip=<skip>&avoid=<avoid>"
                                                { query ∷ { skip ∷ Int, avoid ∷ Maybe ArrayPrimaryKey }
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
                                  , missedEvents ∷
                                          GET "/missed?lastSenderId=<lastSenderId>&lastRecipientId=<lastRecipientId>"
                                                { query ∷
                                                        { lastSenderId ∷ Maybe Int
                                                        , lastRecipientId ∷ Maybe Int
                                                        }
                                                , response ∷ MissedEvents
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
                                  , field ∷
                                          Routes "/field"
                                                { generated ∷
                                                        POST "/generated"
                                                              { body ∷ GeneratedInput
                                                              , response ∷ String
                                                              }
                                                , avatar ∷
                                                        POST "/avatar"
                                                              { body ∷ { base64 ∷ Maybe String }
                                                              , response ∷ Ok
                                                              }
                                                , age ∷
                                                        POST "/age"
                                                              { body ∷ { birthday ∷ Maybe DateWrapper }
                                                              , response ∷ Ok
                                                              }
                                                , gender ∷
                                                        POST "/gender"
                                                              { body ∷ { gender ∷ Maybe Gender }
                                                              , response ∷ Ok
                                                              }
                                                , country ∷
                                                        POST "/country"
                                                              { body ∷ { country ∷ Maybe Int }
                                                              , response ∷ Ok
                                                              }
                                                , language ∷
                                                        POST "/language"
                                                              { body ∷ { ids ∷ Maybe (Array Int) }
                                                              , response ∷ Ok
                                                              }
                                                , tag ∷
                                                        POST "/tag"
                                                              { body ∷ { tags ∷ Maybe (Array String) }
                                                              , response ∷ Ok
                                                              }
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
                                                { body ∷ RecoverAccount
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
                    , elsewhere ∷
                            GET "/elsewhere"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , response ∷ Html
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