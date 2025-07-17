module Shared.Backer.View where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Backer.Types (BackerMessage, BackerModel)

view ∷ BackerModel → Html BackerMessage
view model = HE.div "backer"
      [ HE.div (HA.class' { "backer": true, hidden: not model.visible })
              [ HE.h3 (HA.class' "initial bottom") "MeroChat depends on people like you to keep running"
              , HE.p_ "We will never show ads, charge for subscriptions, sell cosmetics or anything that would stand in the way of being the best place ever to chat"
              , HE.p_ "At the same time: apps have costs. Servers, domains, marketing campaigns, design and development time, etc. quickly become expensive. All the more for a passion project like MeroChat that is not tied to any big corporations or VC money"
              , HE.p_ "So why not pitch in if you can? All proceeds go straight into making this place even better :)"
              , HE.h3_ "Make an one time donation via PayPal"
              , HE.form [ HA.action "https://www.paypal.com/donate", HA.method "post", HA.target "_blank" ]
                      [ HE.input [ HA.type' "hidden", HA.name "business", HA.value "RAH62A4TZZD7L" ]
                      , HE.input [ HA.type' "hidden", HA.name "currency_code", HA.value "USD" ]
                      , HE.input [ HA.type' "image", HA.src "https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif", HA.name "submit", HA.title "PayPal - The safer, easier way to pay online!", HA.alt "Donate with PayPal button" ]
                      , HE.img [ HA.alt "", HA.src "https://www.paypal.com/en_US/i/scr/pixel.gif", HA.width "1", HA.height "1" ]
                      ]
              , HE.h3_ "Make an one time donation with Stripe"
              , HE.a [ HA.href "https://buy.stripe.com/9B67sKa8G1yjgxNdW1d3i00", HA.target "_blank" ]
                      [ HE.svg [ HA.viewBox "0 0 60 25", HA.width "60", HA.height "25", HA.class' "svg-stripe" ]
                              [ HE.path' [ HA.fill "var(--text-color)", HA.d "M59.64 14.28h-8.06c.19 1.93 1.6 2.55 3.2 2.55 1.64 0 2.96-.37 4.05-.95v3.32a8.33 8.33 0 0 1-4.56 1.1c-4.01 0-6.83-2.5-6.83-7.48 0-4.19 2.39-7.52 6.3-7.52 3.92 0 5.96 3.28 5.96 7.5 0 .4-.04 1.26-.06 1.48zm-5.92-5.62c-1.03 0-2.17.73-2.17 2.58h4.25c0-1.85-1.07-2.58-2.08-2.58zM40.95 20.3c-1.44 0-2.32-.6-2.9-1.04l-.02 4.63-4.12.87V5.57h3.76l.08 1.02a4.7 4.7 0 0 1 3.23-1.29c2.9 0 5.62 2.6 5.62 7.4 0 5.23-2.7 7.6-5.65 7.6zM40 8.95c-.95 0-1.54.34-1.97.81l.02 6.12c.4.44.98.78 1.95.78 1.52 0 2.54-1.65 2.54-3.87 0-2.15-1.04-3.84-2.54-3.84zM28.24 5.57h4.13v14.44h-4.13V5.57zm0-4.7L32.37 0v3.36l-4.13.88V.88zm-4.32 9.35v9.79H19.8V5.57h3.7l.12 1.22c1-1.77 3.07-1.41 3.62-1.22v3.79c-.52-.17-2.29-.43-3.32.86zm-8.55 4.72c0 2.43 2.6 1.68 3.12 1.46v3.36c-.55.3-1.54.54-2.89.54a4.15 4.15 0 0 1-4.27-4.24l.01-13.17 4.02-.86v3.54h3.14V9.1h-3.13v5.85zm-4.91.7c0 2.97-2.31 4.66-5.73 4.66a11.2 11.2 0 0 1-4.46-.93v-3.93c1.38.75 3.1 1.31 4.46 1.31.92 0 1.53-.24 1.53-1C6.26 13.77 0 14.51 0 9.95 0 7.04 2.28 5.3 5.62 5.3c1.36 0 2.72.2 4.09.75v3.88a9.23 9.23 0 0 0-4.1-1.06c-.86 0-1.44.25-1.44.9 0 1.85 6.29.97 6.29 5.88z", HA.fillRule "evenodd" ]
                              ]
                      ]
              , HE.h3_ "Become a backer on Liberapay"
              , HE.a [ HA.href "https://liberapay.com/merochat/donate", HA.target "_blank" ] [ HE.img [ HA.alt "Donate using Liberapay", HA.src "https://liberapay.com/assets/widgets/donate.svg" ] ]
              , HE.h3_ "Become a backer on Patreon"
              , HE.a [ HA.href "https://www.patreon.com/bePatron?u=41075080", HA.target "_blank", HA.class' "patreon-button" ]
                      [ HE.svg [ HA.viewBox "0 0 569 546", HA.class' "svg-patreon" ]
                              [ HE.g_
                                      [ HE.circle' [ HA.cx "362.589996", HA.cy "204.589996", HA.r "204.589996" ]
                                      , HE.rect' [ HA.height "545.799988", HA.width "100", HA.x "0", HA.y "0" ]
                                      ]
                              ]
                      , HE.text "Become a Patron"
                      ]
              , HE.h3 ((HA.class' "bottom")) "Other ways to help"
              , HE.p_
                      [ HE.text "Not all contributions have to be financial. Spreading the word, inviting new people to try MeroChat out, reporting bugs or undesired behavior is also a lot of help" ]
              , HE.p_
                      [ HE.text "Likewise, if you have any interest in design or programming, MeroChat is "
                      , HE.a [ HA.href "https://github.com/typestruck/merochat", HA.target "_blank" ] "free and open source software"
                      ]
              ]
      ]