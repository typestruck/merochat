module Shared.Avatar where

import Prelude

import Data.Maybe (Maybe)
import Data.Maybe as DM
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element (class ToNode)
import Flame.Html.Element as HE
import Flame.Types (Html, NodeData)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SP
import Shared.Unsafe as SU
import Web.DOM (Node)

foreign import createImg ∷ Effect Node

defaultAvatar ∷ String
defaultAvatar = avatarPath 1

differentAvatarImages ∷ Int
differentAvatarImages = 8

avatarPath ∷ Int → String
avatarPath index = SP.mediaPath name Png
      where
      name = case index of
            1 → Avatar1
            2 → Avatar2
            3 → Avatar3
            4 → Avatar4
            5 → Avatar5
            6 → Avatar6
            7 → Avatar7
            _ → Avatar8

avatarForSender ∷ Maybe String → String
avatarForSender = DM.fromMaybe defaultAvatar

avatarForRecipient ∷ Maybe Int → Maybe String → String
avatarForRecipient index = DM.fromMaybe (avatarPath <<< avatarIndex $ SU.fromJust index)

avatarIndex ∷ Int → Int
avatarIndex index = mod index differentAvatarImages + 1

avatarColorClass ∷ Maybe Int → String
avatarColorClass index = className <> show (mod (SU.fromJust index) totalColorClasses + 1)
      where
      className = " avatar-color-"
      totalColorClasses = 4

parseAvatar ∷ Maybe String → Maybe String
parseAvatar av = (\a → SP.mediaPath (Upload a) Included) <$> av

async ∷ ∀ message. NodeData message
async = HA.createAttribute "async" ""

decoding ∷ ∀ (message ∷ Type). String → NodeData message
decoding value = HA.createAttribute "decoding" value

-- avoid lagging pictures when browsing suggestions
avatar ∷ ∀ nd34 message35. ToNode nd34 message35 NodeData ⇒ nd34 → Html message35
avatar attributes = HE.managed { createNode, updateNode } attributes unit
      where
      createNode _ = createImg
      updateNode _ _ _ = createImg