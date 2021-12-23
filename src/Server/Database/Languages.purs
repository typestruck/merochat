module Server.Database.Languages where

import Droplet.Language

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))

type Languages =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      )

type LanguagesTable = Table "languages" Languages

languages ∷ LanguagesTable
languages = Table

_languages ∷ Proxy "languages"
_languages = Proxy