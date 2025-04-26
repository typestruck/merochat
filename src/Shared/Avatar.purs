module Shared.Avatar where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Maybe as DM
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element (class ToNode)
import Flame.Html.Element as HE
import Flame.Types (Html, NodeData)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Web.DOM (Node)

foreign import createImg ∷ Effect Node

foreign import resetImg ∷ ∀ a. Node → a → a → Node

defaultAvatar ∷ String
defaultAvatar = SP.resourcePath (Left Avatar) Svg

fromAvatar ∷ Maybe String → String
fromAvatar av = DM.fromMaybe defaultAvatar $ map uploaded av
      where uploaded a = SP.resourcePath (Left $ Upload a) Ignore

async ∷ ∀ message. NodeData message
async = HA.createAttribute "async" ""

decoding ∷ ∀ (message ∷ Type). String → NodeData message
decoding value = HA.createAttribute "decoding" value

-- avoid lagging pictures when browsing suggestions
avatar ∷ ∀ nd34 message35. ToNode nd34 message35 NodeData ⇒ nd34 → Html message35
avatar attributes = HE.managed { createNode, updateNode } attributes unit
      where
      createNode _ = createImg
      updateNode n o p = pure $ resetImg n o p