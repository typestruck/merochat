{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "melanchat"
, dependencies =
  [ "affjax"
  , "argonaut-generic"
  , "browser-cookies"
  , "console"
  , "debug"
  , "effect"
  , "enums"
  , "exceptions"
  , "flame"
  , "form-urlencoded"
  , "formatters"
  , "http-methods"
  , "httpure"
  , "int-53"
  , "js-date"
  , "node-fs"
  , "node-process"
  , "now"
  , "payload"
  , "postgresql-client"
  , "prelude"
  , "psci-support"
  , "read"
  , "routing-duplex"
  , "run"
  , "simple-jwt"
  , "test-unit"
  , "unordered-collections"
  , "uuid"
  , "web-dom"
  , "web-socket"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
