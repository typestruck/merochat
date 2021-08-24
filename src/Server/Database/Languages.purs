module Server.Database.Languages where

import Droplet.Language

import Type.Proxy (Proxy(..))

type Languages = (
      id :: Auto Int,
      name :: String
)

languages :: Table "languages" Languages
languages = Table

_languages :: Proxy "languages"
_languages = Proxy