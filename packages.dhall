{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220613/packages.dhall
        sha256:99f976d547980055179de2245e428f00212e36acd55d74144eab8ad8bf8570d8

let overrides = {=}

let additions =
      { browser-cookies =
        { dependencies =
          [  "arrays"
            , "console"
            , "control"
            , "debug"
            , "effect"
            , "either"
            , "foldable-traversable"
            , "js-date"
            , "lists"
            , "maybe"
            , "newtype"
            , "nonempty"
            , "parsing"
            , "prelude"
            , "psci-support"
            , "quickcheck"
            , "strings"
            , "test-unit"
          ]
        , repo = "https://github.com/easafe/purescript-browser-cookies.git"
        , version = "master"
        }
      , simple-jwt =
        { version = "master"
        , repo = "https://github.com/easafe/purescript-simple-jwt"
        , dependencies =
          [ "arrays"
          , "console"
          , "crypto"
          , "effect"
          , "either"
          , "encoding"
          , "maybe"
          , "node-buffer"
          , "prelude"
          , "simple-json"
          , "strings"
          , "test-unit"
          ]
        }
      , crypto =
        { version = "252d31ae33c255cb0b24f9f91050f2746bd8bc1d"
        , repo = "https://github.com/easafe/purescript-crypto"
        , dependencies =
          [ "aff"
          , "effect"
          , "functions"
          , "maybe"
          , "node-buffer"
          , "nullable"
          , "prelude"
          , "test-unit"
          ]
        }
      , uuid =
        { version = "7bb5a90c9b11f6a33ac7610608a650e4d58aeac9"
        , repo = "https://github.com/spicydonuts/purescript-uuid.git"
        , dependencies =
          [ "prelude"
          , "aff"
          , "effect"
          , "either"
          , "foreign-generic"
          , "foreign"
          , "lists"
          , "maybe"
          , "partial"
          , "spec"
          , "strings"
          , "transformers"
          ]
        }
      , foreign-generic =
        { version = "e7fa22dc9fc2351485f2e915fa7d418ca1965c6d"
        , repo =
            "https://github.com/working-group-purescript-es/purescript-foreign-generic"
        , dependencies =
          [ "assert"
          , "console"
          , "effect"
          , "exceptions"
          , "foreign"
          , "foreign-object"
          , "identity"
          , "ordered-collections"
          , "prelude"
          , "psci-support"
          , "record"
          ]
        }
      , simple-json =
        { repo = "https://github.com/justinwoo/purescript-simple-json"
        , version = "master"
        , dependencies =
          [ "arrays"
          , "assert"
          , "bifunctors"
          , "console"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "identity"
          , "lists"
          , "maybe"
          , "nonempty"
          , "nullable"
          , "partial"
          , "prelude"
          , "record"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          ]
        }
      , unordered-collections =
        { version = "master"
        , repo = "https://github.com/fehrenbach/purescript-unordered-collections"
        , dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "functions"
          , "integers"
          , "lists"
          , "maybe"
          , "minibench"
          , "newtype"
          , "nonempty"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "psci-support"
          , "quickcheck"
          , "record"
          , "st"
          , "tuples"
          , "typelevel-prelude"
          , "unfoldable"
          ]
        }
      , payload =
        { version = "master"
        , repo = "https://github.com/easafe/purescript-payload"
        , dependencies =
          [ "aff"
          , "affjax"
          , "affjax-web"
          , "arrays"
          , "bifunctors"
          , "console"
          , "datetime"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "http-methods"
          , "integers"
          , "js-date"
          , "lists"
          , "maybe"
          , "media-types"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "node-fs-aff"
          , "node-http"
          , "node-path"
          , "node-streams"
          , "node-url"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "record"
          , "refs"
          , "simple-json"
          , "strings"
          , "stringutils"
          , "test-unit"
          , "transformers"
          , "tuples"
          , "type-equality"
          , "typelevel-prelude"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        }
      }

in  upstream // overrides // additions
