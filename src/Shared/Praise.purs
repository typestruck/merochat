module Shared.Praise where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode.Generic as DAEGR
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Shared.Content (Content)
import Shared.DateTime (DateTimeWrapper(..))

type Praise =
      { id ∷ Int
      , date ∷ DateTimeWrapper
      , content ∷ PraisedFor
      }

data PraisedFor
      = Funny
      | Advice
      | Arguing
      | Serious
      | FastReply
      | AlwaysReply
      | LongReply
      | Sarcastic
      | Other String

derive instance Eq PraisedFor
derive instance Ord PraisedFor

derive instance Generic PraisedFor _

instance Bounded PraisedFor where
      bottom = Funny
      top = Other ""

instance BoundedEnum PraisedFor where
      cardinality = Cardinality 1

      fromEnum = case _ of
            Funny → 0
            Advice → 1
            Arguing → 2
            Serious → 3
            FastReply → 4
            AlwaysReply → 5
            LongReply → 6
            Sarcastic → 7
            Other _ → 9

      toEnum = case _ of
            0 → Just Funny
            1 → Just Advice
            2 → Just Arguing
            3 → Just Serious
            4 → Just FastReply
            5 → Just AlwaysReply
            6 → Just LongReply
            7 → Just Sarcastic
            9 → Just $ Other ""
            _ → Nothing

instance Enum PraisedFor where
      succ = case _ of
            Funny → Just Advice
            Advice → Just Arguing
            Arguing → Just Serious
            Serious → Just FastReply
            FastReply → Just AlwaysReply
            AlwaysReply → Just LongReply
            LongReply → Just Sarcastic
            Sarcastic → Just $ Other ""
            Other _ → Nothing

      pred = case _ of
            Funny → Nothing
            Advice → Just Funny
            Arguing → Just Advice
            Serious → Just Advice
            FastReply → Just Serious
            AlwaysReply → Just FastReply
            LongReply → Just AlwaysReply
            Sarcastic → Just LongReply
            Other _ → Just Sarcastic

instance EncodeJson PraisedFor where
      encodeJson = DAEGR.genericEncodeJson

instance DecodeJson PraisedFor where
      decodeJson = DADGR.genericDecodeJson
