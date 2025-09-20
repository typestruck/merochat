module Server.Posts.Action where

import Debug
import Prelude
import Shared.Im.Types
import Shared.Privilege

import Data.Array ((:))
import Data.Array as DA
import Data.Array.NonEmpty as DAN
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Nullable as DN
import Data.Set (Set)
import Data.Set as DST
import Data.String as DS
import Data.Tuple.Nested (type (/\), (/\))
import Droplet.Driver (Pool)
import Environment (production)
import Run.Except as RE
import Safe.Coerce as SC
import Server.AccountValidation as SA
import Server.Database.Types (Checked(..))
import Server.Effect (BaseEffect, ServerEffect)
import Server.Email (Email(..))
import Server.Email as SE
import Server.File as SF
import Server.Im.Database.Changelog as SIDC
import Server.Im.Database.Execute as SIDE
import Server.Im.Database.Flat (FlatContactHistoryMessage, fromFlatContact, fromFlatMessage)
import Server.Im.Database.Flat as SIF
import Server.Im.Database.Permission as SIDPP
import Server.Im.Database.Present as SIDP
import Server.Im.Database.Suggest as SIDS
import Server.Im.Types (Payload)
import Server.Posts.Database as SPD
import Server.Sanitize as SS
import Server.ThreeK as ST
import Server.Wheel as SW
import Shared.Backer.Contact (backerUser)
import Shared.Backer.Contact as SBC
import Shared.Changelog (Changelog)
import Shared.DateTime (DateTimeWrapper(..))
import Shared.DateTime as SD
import Shared.Markdown (Token(..))
import Shared.Markdown as SM
import Shared.Post (Post)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.ResponseError (ResponseError(..))

posts ∷ Int → Int -> ServerEffect (Array Post)
posts loggedUserId userId = SPD.presentPosts loggedUserId userId

