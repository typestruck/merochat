module Shared.Avatar where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (Html, NodeData)
import Shared.Backer.Contact (backerId)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU
import Web.DOM (Node)

foreign import createImg ∷ Effect Node

foreign import resetImg ∷ ∀ a. Node → a → a → Node

defaultAvatar ∷ String
defaultAvatar = SP.resourcePath (Left Avatar) Svg

fromAvatar ∷ ∀ r. { id ∷ Int, avatar ∷ Maybe String | r } → String
fromAvatar user
      | user.id == backerId = SU.fromJust user.avatar
      | otherwise = DM.fromMaybe defaultAvatar $ map uploaded user.avatar
              where
              uploaded a = SP.resourcePath (Left $ Upload a) Ignore

fromAvatarPath ∷ Maybe String → Maybe String
fromAvatarPath = case _ of
      Just a | DS.contains (Pattern "data:image") a → Just a
      Just aa → Just $ SP.resourcePath (Left $ Upload aa) Ignore
      aaa → aaa

async ∷ ∀ message. NodeData message
async = HA.createAttribute "async" ""

decoding ∷ ∀ (message ∷ Type). String → NodeData message
decoding value = HA.createAttribute "decoding" value

-- avoid lagging pictures when browsing suggestions
avatar ∷ ∀ message. Array (NodeData message) → Html message
avatar attributes = HE.managed { createNode, updateNode } attributes unit
      where
      createNode _ = createImg
      updateNode n o p = pure $ resetImg n o p