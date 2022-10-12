module Shared.KarmaPrivileges.Types where

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic as DADGR
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic as DAEGR
import Shared.User

type LeaderboardUser =
      { position ∷ Int
      , karma ∷ Int
      , avatar ∷ Maybe String
      , name ∷ String
      }

type KarmaPrivilegesModel =
      { top10 ∷ Array LeaderboardUser
      , inBetween10 ∷ Array LeaderboardUser
      , userPosition ∷ Int
      , toggleBoard ∷ ToggleBoard
      , privileges ∷ Array PrivilegeUser
      }

type PrivilegeUser =
      { name ∷ String
      , description ∷ String
      , got ∷ Boolean
      , quantity ∷ Int
      }

data KarmaPrivilegesMessage =
      ToggleBoardDisplay ToggleBoard

data ToggleBoard
      = InBetween10
      | Top10

instance encodeJsonToggleBoard ∷ EncodeJson ToggleBoard where
      encodeJson = DAEGR.genericEncodeJson

instance decodeJsonToggleBoard ∷ DecodeJson ToggleBoard where
      decodeJson = DADGR.genericDecodeJson

derive instance genericToggleBoard ∷ Generic ToggleBoard _

derive instance eqToggleBoard ∷ Eq ToggleBoard