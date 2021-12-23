module Server.Database.Countries where

import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))
import Droplet.Language

type Countries =
      ( id ∷ Column Int (PrimaryKey /\ Identity)
      , name ∷ String
      )

type CountriesTable = Table "countries" Countries

countries ∷ CountriesTable
countries = Table
