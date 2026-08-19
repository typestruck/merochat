module Server.Database.DebateStatements where

import Droplet.Language

import Data.DateTime (DateTime)
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

type DebateStatements =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , debate ∷ Int
      , statement ∷ String
      , created ∷ Column DateTime Default
      , who ∷ Int
      )

debate_statements ∷ Table "debate_statements" DebateStatements
debate_statements = Table

_debate ∷ Proxy "debate"
_debate = Proxy

_statement ∷ Proxy "statement"
_statement = Proxy

