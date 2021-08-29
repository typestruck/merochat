module Server.Database.Functions where

import Droplet.Language

import Data.DateTime (Date, DateTime)
import Data.Tuple.Nested (type (/\))

date_part_age :: FunctionSignature (String /\ Date) Int
date_part_age = function "date_part_age"

datetime_part_age :: FunctionSignature (String /\ DateTime) Int
datetime_part_age = function "date_part_age"