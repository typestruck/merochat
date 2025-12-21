module Shared.Spec where

import Prelude

import Data.DateTime (DateTime)
import Data.List (List)
import Data.Maybe (Maybe)
import Node.Stream (Read, Stream)
import Payload.ResponseTypes (Empty)
import Payload.Spec (type (:), GET, Guards, Nil, POST, Routes, Spec(..))
import Shared.Account (EmailCaptcha, EmailPassword, EmailPasswordCaptcha, ResetPassword)
import Shared.Ask (Ask)
import Shared.Changelog (Changelog)
import Shared.Experiments.Types (Match, PaperPlane, PaperPlaneStatus, Question)
import Shared.Html (Html)
import Shared.Im.Types (Contact, HistoryMessage, Report, Suggestion, SuggestionsFrom)
import Shared.Post (PostPayload, Post)
import Shared.Profile.Types (GeneratedInput, SavedFields)
import Shared.Settings.Types (PrivacySettings)

-- types other than Html and Empty are serialized as json
spec ∷
      Spec
            { guards ∷
                    { loggedUserId ∷ Int
                    , checkAnonymous ∷ Unit
                    , loggedUserToken ∷ String
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
                                  , response ∷ Empty
                                  }
                    , register ∷
                            POST "/register"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , body ∷ EmailPasswordCaptcha
                                  , response ∷ Empty
                                  }
                    , unsubscribe ∷
                            GET "/unsubscribe?email_id=<token>"
                                  { guards ∷ Guards Nil
                                  , query ∷ { token ∷ String }
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
                                                , response ∷ Empty
                                                }
                                  }
                    , posts ∷
                            Routes "/posts"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/?poster=<poster>"
                                                { query ∷ { poster ∷ Int }
                                                , response ∷ (Array Post)
                                                }
                                  , post ∷
                                          POST "/post"
                                                { body ∷ PostPayload
                                                , response ∷ { id ∷ Int }
                                                }
                                  , seen ∷
                                          POST "/seen"
                                                { body ∷
                                                        { id ∷ Int
                                                        , poster ∷ Int
                                                        }
                                                , response ∷ Empty
                                                }
                                  }
                    , asks ∷
                            Routes "/asks"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                    , get ∷
                                          GET "/?answerer=<answerer>"
                                                { query ∷ { answerer ∷ Int }
                                                , response ∷ Array Ask
                                                }
                                  , post ∷
                                          POST "/post"
                                                { body ∷ { userId ∷ Int, question ∷ String }
                                                , response ∷ { allowed ∷ Boolean }
                                                }
                                  }
                    , im ∷
                            Routes "/im"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ Html
                                                }
                                  , subscribe ∷
                                          POST "/subscribe"
                                                { response ∷ Empty
                                                }
                                  , register ∷
                                          POST "/register"
                                                { body ∷ EmailPassword
                                                , response ∷ Empty
                                                }
                                  , react ∷
                                          POST "/react"
                                                { body ∷ { id ∷ Int, reaction ∷ String }
                                                , response ∷ Empty
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
                                                              , response ∷ Empty
                                                              }
                                                }
                                  , contact ∷
                                          GET "/contact?id=<id>"
                                                { query ∷ { id ∷ Int }
                                                , response ∷ (Array Contact)
                                                }
                                  , history ∷
                                          GET "/history?with=<with>&skip=<skip>"
                                                { query ∷ { skip ∷ Int, with ∷ Int }
                                                , response ∷ (Array HistoryMessage)
                                                }
                                  , suggestions ∷
                                          GET "/suggestions?skip=<skip>&sg=<sg>"
                                                { query ∷ { skip ∷ Int, sg ∷ SuggestionsFrom }
                                                , response ∷ (Array Suggestion)
                                                }
                                  , block ∷
                                          POST "/block"
                                                { body ∷ { id ∷ Int }
                                                , response ∷ Empty
                                                }
                                  , delete ∷
                                          POST "/delete"
                                                { body ∷ { userId ∷ Int, messageId ∷ Int }
                                                , response ∷ Empty
                                                }
                                  , missedContacts ∷
                                          GET "/missed?since=<since>&last=<last>"
                                                { query ∷ { since ∷ DateTime, last ∷ Maybe Int }
                                                , response ∷ (Array Contact)
                                                }
                                  , fortune ∷
                                          GET "/fortune"
                                                { response ∷ String
                                                }
                                  , report ∷
                                          POST "/report"
                                                { body ∷ Report
                                                , response ∷ Empty
                                                }

                                  , tutorial ∷
                                          POST "/tutorial"
                                                { response ∷ Empty
                                                }
                                  , greeting ∷
                                          POST "/greeting"
                                                { response ∷ Empty
                                                }
                                  }
                    , profile ∷
                            Routes "/profile"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , get ∷
                                          GET "/"
                                                { response ∷ Html
                                                }
                                    , ignore ∷
                                          POST "/ignore"
                                                { body ∷ { id ∷  Int }
                                                , response ∷ Empty
                                                }
                                  , asks ∷
                                          GET "/asks?after=<after>"
                                                { query ∷ { after ∷ Maybe Int }
                                                , response ∷ Array Ask
                                                }
                                  , answer ∷
                                          POST "answer"
                                                { body ∷ { id ∷ Int, answer ∷ String }
                                                , response ∷ Empty
                                                }
                                  , posts ∷
                                          GET "/posts?after=<after>"
                                                { query ∷ { after ∷ Maybe Int }
                                                , response ∷ Array Post
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
                                                { response ∷ Html
                                                }
                                  , chat ∷
                                          Routes "/chat"
                                                { background ∷
                                                        POST "background"
                                                              { body ∷ { ownBackground ∷ Boolean, image ∷ Maybe String }
                                                              , response ∷ String
                                                              }
                                                }
                                  , account ∷
                                          Routes "/account"
                                                { email ∷
                                                        POST "/account/email"
                                                              { body ∷ { email ∷ String }
                                                              , response ∷ Empty
                                                              }
                                                , password ∷
                                                        POST "/account/password"
                                                              { body ∷ { password ∷ String }
                                                              , response ∷ Empty
                                                              }
                                                , terminate ∷
                                                        POST "/account/terminate"
                                                              { response ∷ Empty
                                                              }
                                                , privacy ∷
                                                        POST "/account/privacy"
                                                              { body ∷ PrivacySettings
                                                              , response ∷ Empty
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
                                                , response ∷ Empty
                                                }
                                  , reset ∷
                                          POST "/recover"
                                                { body ∷ ResetPassword
                                                , response ∷ Empty
                                                }
                                  }
                    , logout ∷
                            POST "/logout"
                                  { guards ∷ Guards ("loggedUserId" : "loggedUserToken" : Nil)
                                  , response ∷ Empty
                                  }
                    , leaderboard ∷
                            GET "/leaderboard"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , response ∷ Html
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
                                                { response ∷ Html
                                                }
                                  , send ∷
                                          POST "/send"
                                                { body ∷ { comments ∷ String, screenshot ∷ Maybe String }
                                                , response ∷ Empty
                                                }
                                  }

                    , internalHelp ∷
                            GET "/inhelp"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
                                  , response ∷ Html
                                  }
                    , experiments ∷
                            Routes "/experiments"
                                  { get ∷
                                          GET "/"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , response ∷ Html
                                                }
                                  , questions ∷
                                          GET "/questions"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , response ∷ (Array Question)
                                                }
                                  , matches ∷
                                          GET "/matches"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , response ∷ (Array Match)
                                                }
                                  , answer ∷
                                          POST "/answer"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , body ∷ { choice ∷ Int }
                                                , response ∷ Empty
                                                }
                                  , throw ∷
                                          POST "/throw"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , body ∷ { message ∷ String }
                                                , response ∷ { id ∷ Int }
                                                }
                                  , catch ∷
                                          POST "/catch"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , body ∷ { id ∷ Int }
                                                , response ∷ Empty
                                                }
                                  , pass ∷
                                          POST "/pass"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , body ∷ { id ∷ Int }
                                                , response ∷ Empty
                                                }
                                  , flying ∷
                                          GET "/flying"
                                                { guards ∷ Guards ("loggedUserId" : Nil)
                                                , response ∷ Array PaperPlane
                                                }
                                  }
                    , backer ∷
                            GET "/backer"
                                  { guards ∷ Guards ("checkAnonymous" : Nil)
                                  , response ∷ Html
                                  }
                    , internalBacker ∷
                            GET "/inbacker"
                                  { guards ∷ Guards ("loggedUserId" : Nil)
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
                                                , response ∷ Empty
                                                }
                                  }
                    , topic ∷
                            POST "/topic/<..path>"
                                  { params ∷ { path ∷ List String }
                                  , body ∷ String
                                  , response ∷ Empty
                                  }
                    , sw ∷
                            GET "/sw.js"
                                  { response ∷ Stream (read ∷ Read)
                                  }
                    , developmentFiles ∷
                            GET "/file/<..path>"
                                  { params ∷ { path ∷ List String }
                                  , response ∷ Stream (read ∷ Read)
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