{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "melanchat"
, dependencies =
  [
  , "argonaut-generic"
  , "console"
  , "debug"
  , "effect"
  , "enums"
  , "exceptions"
  , "flame"
  , "http-methods"
  , "int-53"
  , "js-date"
  , "now"
  , "browser-cookies"
  , "node-process"
  , "simple-jwt"
  , "node-fs"
  , "httpure"
  , "affjax"
  , "postgresql-client"
  , "prelude"
  , "psci-support"
  , "read"
  , "routing-duplex"
  , "run"
  , "test-unit"
  , "unordered-collections"
  , "uuid"
  , "web-dom"
  , "web-socket"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
-- , sources = [ "src/**/*.purs", "test/**/*.purs" ]
, sources = [ "src/**/*.purs" ]
}
