module Shared.File where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))

fromBase64File ∷ String → Maybe (String /\ String)
fromBase64File input = case DS.split (Pattern ",") input of
      [ mediaType, base64 ] → Just (mediaType /\ base64)
      _ → Nothing