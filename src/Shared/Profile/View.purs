module Shared.Profile.View where

import Prelude
import Shared.Experiments.Types
import Shared.Im.Types
import Shared.Options.Profile
import Shared.Profile.Types

import Data.Array ((:))
import Data.Array as DA
import Data.Foldable as DF
import Data.HashMap as DH
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Maybe as DM
import Data.Newtype as DN
import Data.String as DS
import Data.String.Read as DSR
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Types (NodeData)
import Prim.Row (class Cons)
import Prim.Symbol (class Append)
import Record as R
import Shared.Avatar as SA
import Shared.DateTime (DateWrapper(..))
import Shared.DateTime as SDT
import Shared.Element (ElementId(..))
import Shared.Im.Svg as SIS
import Shared.Im.View.Profile as SIVP
import Shared.Markdown as SM
import Shared.Network (RequestStatus(..))
import Shared.Privilege (Privilege(..))
import Shared.Intl as SI
import Shared.Privilege as SP
import Shared.Unsafe as SU
import Shared.User (Gender(..))
import Type.Data.Symbol as TDS
import Type.Proxy (Proxy(..))

view ∷ ProfileModel → Html ProfileMessage
view model = HE.div (show ProfileEditionForm)
      [ HE.div [ HA.class' { "profile-edition": true, hidden: not model.visible } ]
              [ HE.div (HA.class' { "loading-over": true, hidden: not model.loading })
                      [ HE.div' (HA.class' "loading")
                      ]
              , HE.div (HA.class' "request-result-message success")
                      [ HE.span (HA.class' { "request-error-message": true, hidden: true }) ""
                      ]
              , HE.div (HA.class' "profile-section")
                      [ HE.div (HA.class' "profile-section-label") "Avatar"
                      , HE.div (HA.onClick SelectAvatar)
                              [ HE.img [ HA.class' "avatar-profile-edition", HA.src $ SA.fromAvatar model.user ]
                              , HE.input [ HA.id "avatar-file-input", HA.type' "file", HA.class' "hidden", HA.accept ".png, .jpg, .jpeg, .tif, .tiff, .bmp" ]
                              , HE.svg [ HA.class' "svg-16", HA.viewBox "0 0 16 16" ]
                                      [ HE.title "Reset profile picture"
                                      , HE.path' [ HA.class' "strokeless", HA.d "M8,0a8,8,0,1,0,8,8A8,8,0,0,0,8,0ZM8,15.5A7.5,7.5,0,1,1,15.5,8,7.5,7.5,0,0,1,8,15.5Z" ]
                                      , HE.path' [ HA.class' "a", HA.d "M11.65,7.38H4.4a.62.62,0,1,0,0,1.24h7.25a.62.62,0,0,0,0-1.24Z" ]
                                      ]
                              ]
                      ]
              , HE.div (HA.class' "profile-section")
                      [ HE.div (HA.class' "profile-section-label") "Display name"
                      , HE.div_ "Leave it blank for a new auto generated name"
                       ,HE.input [HA.class' "modal-input"]
                      ]
              ]
      ]