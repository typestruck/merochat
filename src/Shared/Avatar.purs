module Shared.Avatar where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.String (Pattern(..))
import Data.String as DS
import Effect (Effect)
import Flame.Html.Attribute as HA
import Shared.Icon as SI
import Shared.Extra as SE
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
avatar attributes = SE.managed { createNode, updateNode } attributes unit
      where
      createNode _ = createImg
      updateNode :: _ -> _ -> _ -> Effect Node
      updateNode n o p = pure $ resetImg n o p

image ∷ ∀ message r. { avatar ∷ Maybe String | r } → Array (NodeData message) → Html message
image user = imagePath user.avatar

imagePath ∷ ∀ message. Maybe String → Array (NodeData message) → Html message
imagePath path attributes = case path of
      Just _ → HE.img attributes
      Nothing → SI.illustration (attributes <> [ HA.viewBox "0 0 447 400" ])
            [ HE.path' [ HA.d "M130.765 229.722C130.765 240.563 121.958 249.341 111.117 249.341C100.312 249.341 91.4977 240.563 91.4977 229.722C91.4977 218.873 100.312 210.095 111.117 210.095C121.958 210.095 130.765 218.873 130.765 229.722Z", HA.fill "#DB5A5D" ]
            , HE.path' [ HA.d "M414.743 199.996C414.743 310.45 321.991 400 207.52 400C93.0989 400 0.302979 310.45 0.302979 199.996C0.302979 89.543 93.0989 0 207.52 0C321.991 0 414.743 89.543 414.743 199.996Z", HA.fill "#DB5A5D" ]
            , HE.path' [ HA.d "M367.145 257.052C330.365 348.795 227.695 393.956 137.805 357.941C47.8804 321.919 4.81115 218.346 41.54 126.589L367.145 257.052Z", HA.fill "#FBFEFB" ]
            , HE.path' [ HA.d "M122.312 243.852C122.312 250.567 116.88 255.977 110.165 255.977C103.478 255.977 98.0182 250.567 98.0182 243.852C98.0182 237.129 103.478 231.69 110.165 231.69C116.873 231.69 122.312 237.129 122.312 243.852Z", HA.fill "#DB5A5D" ]
            , HE.path' [ HA.d "M176.879 277.003C183.587 277.003 189.025 271.565 189.025 264.856C189.025 258.148 183.587 252.709 176.879 252.709C170.17 252.709 164.732 258.148 164.732 264.856C164.732 271.565 170.17 277.003 176.879 277.003Z", HA.fill "#DB5A5D" ]
            , HE.path' [ HA.d "M248.944 304.009C255.645 304.009 261.076 298.577 261.076 291.876C261.076 285.176 255.645 279.744 248.944 279.744C242.244 279.744 236.812 285.176 236.812 291.876C236.812 298.577 242.244 304.009 248.944 304.009Z", HA.fill "#DB5A5D" ]
            , HE.path' [ HA.d "M132.013 187.907C160.389 187.907 183.392 164.904 183.392 136.529C183.392 108.153 160.389 85.1503 132.013 85.1503C103.638 85.1503 80.6348 108.153 80.6348 136.529C80.6348 164.904 103.638 187.907 132.013 187.907Z", HA.fill "#DB5A5D" ]
            , HE.path' [ HA.d "M246.052 170.271C246.052 198.64 223.057 221.672 194.659 221.672C166.29 221.672 143.295 198.64 143.295 170.271C143.295 141.895 166.29 118.9 194.659 118.9C223.049 118.893 246.052 141.895 246.052 170.271Z", HA.fill "#DB5A5D" ]
            ]
