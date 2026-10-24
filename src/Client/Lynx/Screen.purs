module Client.Lynx.Screen where

import Prelude

import Client.AppId (ClientAppId)
import Flame (AppId, Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Theme as ST

view ∷ ∀ message. AppId ClientAppId message → Array (Html message) → Html message
view appId = HE.scrollView
      [ HA.id id
      , HA.class' "authenticated-route"
      , HA.scrollOrientation "vertical"
      , ST.onLayoutChange id
      ]
      where
      id = show appId
