module Server.Database.Tags where

import Droplet.Language

import Type.Proxy (Proxy(..))

type Tags =
      ( id :: Auto Int
      , name :: String
      )

tags :: Table "tags" Tags
tags = Table

_tags :: Proxy "tags"
_tags = Proxy
