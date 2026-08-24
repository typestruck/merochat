module Shared.Profile.Mode where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as DE
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Payload.Client.QueryParams (class EncodeQueryParam)
import Payload.Server.QueryParams (class DecodeQueryParam, DecodeError)
import Payload.Server.QueryParams as PQ
import Shared.Unsafe as SU

data ProfileMode
      = Edit
      | Preview
      | Praise
      | OwnPosts
      | Asked

derive instance Generic ProfileMode _

derive instance Eq ProfileMode

derive instance Ord ProfileMode

instance Bounded ProfileMode where
      bottom = Edit
      top = Asked

instance BoundedEnum ProfileMode where
      cardinality = Cardinality 1
      fromEnum = case _ of
            Edit → 0
            Preview → 1
            Praise → 2
            OwnPosts → 3
            Asked → 4
      toEnum = case _ of
            0 → Just Edit
            1 → Just Preview
            2 → Just Praise
            3 → Just OwnPosts
            4 → Just Asked
            _ → Nothing

instance Enum ProfileMode where
      succ = case _ of
            Edit → Just Preview
            Preview → Just Praise
            Praise → Just OwnPosts
            OwnPosts → Just Asked
            Asked → Nothing
      pred = case _ of
            Edit → Nothing
            Preview → Just Edit
            Praise → Just Preview
            OwnPosts → Just Praise
            Asked → Just OwnPosts

instance EncodeJson ProfileMode where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson ProfileMode where
      decodeJson = DADGR.genericDecodeJson

instance EncodeQueryParam ProfileMode where
      encodeQueryParam = Just <<< show <<< DE.fromEnum

instance DecodeQueryParam ProfileMode where
      decodeQueryParam query key = map (SU.fromJust <<< DE.toEnum) (PQ.decodeQueryParam query key ∷ Either DecodeError Int)
