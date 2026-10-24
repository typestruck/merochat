-- | Used by the external and internal help pages
module Shared.Faq where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Extra as SE

faq ∷ ∀ m. Html m
faq =
      HE.div [ HA.class' "terms" ]
            [ SE.ul [ HA.class' "bulleted no-padding" ]
                    [ SE.li [ HA.class' "no-padding" ] [ SE.a [ HA.href "#whatsmerochat", SE.anchor "whatsmerochat" ] [ HE.text "What is MeroChat?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#canifilter", SE.anchor "canifilter" ] [ HE.text "Can I filter suggestions by gender/location/etc?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#dating", SE.anchor "dating" ] [ HE.text "Is MeroChat a dating/hookup app?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#isitfree", SE.anchor "isitfree" ] [ HE.text "Is MeroChat free?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#whatskarma", SE.anchor "whatskarma" ] [ HE.text "What is karma?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#gibberishprofile", SE.anchor "gibberishprofile" ] [ HE.text "What is that gibberish on my profile?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#profileprivate", SE.anchor "profileprivate" ] [ HE.text "Is my profile private?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#groupchats", SE.anchor "groupchats" ] [ HE.text "Are group chats available?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#chatexperiments", SE.anchor "chatexperiments" ] [ HE.text "What are chat experiments?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#mero", SE.anchor "mero" ] [ HE.text "What does Mero in MeroChat stand for?" ] ]
                    , SE.li_ [ SE.a [ HA.href "#canihelp", SE.anchor "canihelp" ] [ HE.text "Wow, I love it. How can I help?" ] ]
                    ]
            , SE.h2 [ HA.id "whatsmerochat" ] [ HE.text "What is MeroChat?" ]
            , SE.p_ [ HE.text "MeroChat is a random chat. That means the app suggests you new people to talk to. You may choose to fill in your profile and voila! Friends" ]
            , SE.h2 [ HA.id "canifilter" ] [ HE.text "Can I filter suggestions by gender/location/etc?" ]
            , SE.p_ [ HE.text "You may skip suggestions, but it is not possible to filter them in any way. MeroChat tries its best to give you quality people to talk to, but the fun is in discovering" ]
            , SE.h2 [ HA.id "dating" ] [ HE.text "Is MeroChat a dating/hookup app?" ]
            , SE.p_ [ HE.text "No. MeroChat is for friendly conversations only, there is already plenty of other places for trying to get laid" ]
            , SE.h2 [ HA.id "isitfree" ] [ HE.text "Is MeroChat free?" ]
            , SE.p_ [ HE.text "Yes! The app is totally free and there is no ads. MeroChat runs on donations from people who like it enough (or might just be after the rewards, who can tell)" ]
            , SE.h2 [ HA.id "whatskarma" ] [ HE.text "What is karma?" ]
            , SE.p_ [ HE.text "Think of it as a score of how much other users trust you. You gain karma points by starting good conversations. As a moderation tool, some privileges (like sending pictures) are locked until you have enough karma" ]
            , SE.h2 [ HA.id "gibberishprofile" ] [ HE.text "What is that gibberish on my profile?" ]
            , SE.p_ [ HE.text "The app automatically fills in some fields (like name, headline or bio) for a new profile. Sometimes it is amusing, sometimes just corny. You may leave as it is (or also generate some new gibberish!) or edit it to your liking" ]
            , SE.h2 [ HA.id "profileprivate" ] [ HE.text "Is my profile private?" ]
            , SE.p_ [ HE.text "Your profile can only be viewed by other users inside of the app, limited by your privacy settings. There is absolutely no need to share personal details on your profile -- you are in control of what to share or not" ]
            , SE.h2 [ HA.id "groupchats" ] [ HE.text "Are group chats available?" ]
            , SE.p_ [ HE.text "No, MeroChat is for one on one, private conversations" ]
            , SE.h2 [ HA.id "chatexperiments" ] [ HE.text "What are chat experiments?" ]
            , SE.p_ [ HE.text "These are gimmicks/games/events for novel chatting. For example: chat in character as a historical figure, find someone to debate your latest hot take, send messages in paper planes, etc. Chat experiments are optional to join and users are randomly invited to join them" ]
            , SE.h2 [ HA.id "mero" ] [ HE.text "What does Mero in MeroChat stand for?" ]
            , SE.p_ [ HE.text "Until someone finds a backronym, it is the Guarani word for (water)melons. Not surprisingly, the app is full of them" ]
            , SE.h2_ [ HE.text "Recommend me some music" ]
            , SE.p [ HA.class' "lynx-inline" ]
                    [ HE.text "Listen to "
                    , SE.a [ HA.href "https://open.spotify.com/track/4klGcqccAwciiLlPL136Kl?si=kYRoPuV6S2utxNCT4j2h_Q", SE.externalLink "https://open.spotify.com/track/4klGcqccAwciiLlPL136Kl?si=kYRoPuV6S2utxNCT4j2h_Q", HA.target "_blank" ] [ HE.text "Metá Metá" ]
                    ]
            , SE.h2 [ HA.id "canihelp" ] [ HE.text "Wow, I love it. How can I help?" ]
            , SE.p [ HA.class' "lynx-inline" ]
                    [ HE.text "Right?! If you can spare some, consider backing MeroChat. The entire "
                    , SE.a [ HA.href "https://github.com/typestruck/merochat", SE.externalLink "https://github.com/typestruck/merochat", HA.target "_blank" ] [ HE.text "source code" ]
                    , HE.text " is also freely avaible in case this is your thing. That being said, just letting others know about MeroChat is already a huge help. Reporting bugs, bad user behavior or other issues is also highly appreciated"
                    ]
            ]
