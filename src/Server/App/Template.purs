module Server.App.Template where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Server.Template (externalDefaultParameters)
import Server.Template as ST
import Shared.Element (ElementId(..))
import Shared.Html (Html(..))
import Shared.Options.Profile (emailMaxCharacters, passwordMaxCharacters, passwordMinCharacters)
import Shared.Resource (Media(..), ResourceType(..))
import Shared.Resource as SR
import Shared.Routes (routesSpec)

template ∷ Effect Html
template = do
      contents ← ST.template externalDefaultParameters
            { content = externalDefaultParameters.content <> content
            , javascript = javascript
            , title = "MeroChat - App"
            }
      Html <$> FRS.render contents
      where
      javascript =
            [ HE.script [ HA.type' "text/javascript" ]
                    [ HE.text
                            """
            if (navigator.userAgent.toLowerCase().indexOf(`android`) == -1)
                  window.location.href = `#other`;
            else
                   window.location.href =  `https://play.google.com/store/apps/details?id=chat.mero.twa`; """
                    ]
            ]
      content =
            [ HE.div [ HA.class' "pastel-area" ]
                    [ HE.div [ HA.id "other", HA.class' "sign-up-form form-up" ]
                            [ HE.h2 [] [ HE.text "iOS" ]
                            , HE.div []
                                    [ HE.text "Apple charges 400 usd per year so MeroChat isn't on the App Store yet. "
                                    , HE.a [ HA.href $ routesSpec.backer {} ] [ HE.text "Wink, wink, donate" ]
                                    ]

                            , HE.b [ HA.class' "app-step" ] [ HE.text "However, you can add the site to your home screen, which works the same as an app! Step by step:" ]
                            , HE.div [ HA.class' "app-step" ]
                                    [ HE.h3 [] [ HE.text "1. Find the share button" ]
                                    , HE.img [ HA.src $ SR.resourcePath (Left AppShare) Png ]
                                    ]
                            , HE.div [ HA.class' "app-step" ]
                                    [ HE.h3 [] [ HE.text """2. Tap "More" """ ]
                                    , HE.img [ HA.src $ SR.resourcePath (Left AppMore) Png ]
                                    ]
                            , HE.div [ HA.class' "app-step" ]
                                    [ HE.h3 [] [ HE.text """3. Tap "Add to Home Screen" """ ]
                                    , HE.img [ HA.src $ SR.resourcePath (Left AppHome) Png ]
                                    ]
                            , HE.div [ HA.class' "app-step" ]
                                    [ HE.h3 [] [ HE.text """4. Tap "Add" """ ]
                                    , HE.img [ HA.src $ SR.resourcePath (Left AppAdd) Png ]
                                    ]
                            , HE.div [ HA.class' "app-step" ]
                                    [ HE.h3 [] [ HE.text """5. Open it from your home screen and tap "Get notified of new messages" to allow notifications""" ]
                                    , HE.img [ HA.src $ SR.resourcePath (Left AppGet) Png ]
                                    ]
                            , HE.div [ HA.class' "app-step" ]
                                    [ HE.h3 [] [ HE.text """Done! Now you can use MeroChat like any other app""" ]
                                    ]
                            ]
                    ]
            ]