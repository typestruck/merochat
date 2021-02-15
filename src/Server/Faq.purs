module Server.Faq where

import Prelude

import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Shared.Routes (routes)

faq :: forall m. Html m
faq =
      HE.div (HA.class' "terms") [
            HE.ul (HA.class' "bulleted no-padding") [
                  HE.li (HA.class' "no-padding") $ HE.a (HA.href "#whatsmelanchat") "What is MelanChat?",
                  HE.li_ $ HE.a (HA.href "#canifilter") "Can I filter new people to talk to by gender/location/etc?",
                  HE.li_ $ HE.a (HA.href "#dating") "Is MelanChat a dating/hookup app?",
                  HE.li_ $ HE.a (HA.href "#isitfree") "Is MelanChat free?",
                  HE.li_ $ HE.a (HA.href "#whatskarma") "What is Karma?",
                  HE.li_ $ HE.a (HA.href "#gibberishprofile") "What is that gibberish on my profile?",
                  HE.li_ $ HE.a (HA.href "#profileprivate") "Is my profile private?",
                  HE.li_ $ HE.a (HA.href "#groupchats") "Are group chats available?",
                  HE.li_ $ HE.a (HA.href "#chatexperiments") "What are chat experiments?",
                  HE.li_ $ HE.a (HA.href "#melan") "What does Melan in MelanChat stand for?",
                  HE.li_ $ HE.a (HA.href "#canihelp") "Wow, I love it. How can I help?",
                  HE.li (HA.class' "hide-internal") $ HE.a (HA.href "#other") "I have another question/doubt/something else to say"
            ],
            HE.h2 [HA.id "whatsmelanchat"] "What is MelanChat?",
            HE.p_ "MelanChat is a random chat. That means the app suggests you new people to talk to. You may choose to fill in your profile and voila! Friends.",
            HE.h2 [HA.id "canifilter"] "Can I filter new people to talk to by gender/location/etc?",
            HE.p_ "You may skip suggestions, but it is not possible to filter them in any way. MelanChat tries its best to give you quality people to talk to, but the fun is in discovering.",
            HE.h2 [HA.id "dating"] "Is MelanChat a dating/hookup app?",
            HE.p_ "No. MelanChat is for friendly conversations only, there is already plenty of other places for try getting laid.",
            HE.h2 [HA.id "isitfree"] "Is MelanChat free?",
            HE.p_ [
                  HE.text "Yes! The app is totally free and there is no ads. MelanChat ",
                  HE.a (HA.href $ routes.backer {}) "runs on donations",
                  HE.text " from people who like it enough (or might just be after the rewards, who can tell)."
            ],
            HE.h2 [HA.id "whatskarma"] "What is Karma?",
            HE.p_ "Think of it as a score of how much other users trust you. You gain Karma points by starting good conversations.",
            HE.p_ "As a moderation tool, some features (like sending pictures) might be locked until you have enough Karma.",
            HE.h2 [HA.id "gibberishprofile"] "What is that gibberish on my profile?",
            HE.p_ "The app automatically fills in some fields (like name, headline or bio) for a new profile. Sometimes it is funny, sometimes just totally incoherent. You may leave as it is (or also generate some new gibberish!) or edit it to your liking.",
            HE.h2 [HA.id "profileprivate"] "Is my profile private?",
            HE.p_ "Your profile can only be viewed by other users inside of the app. As for your personal data, you decide how much (or little) you want to share.",
            HE.h2 [HA.id "groupchats"] "Are group chats available?",
            HE.p_ "No, MelanChat is for one on one, private conversations.",
            HE.h2 [HA.id "chatexperiments"] "What are chat experiments?",
            HE.p_ "These are gimmicks/games/events for novel chatting. For example: chat in character as a historical figure, find someone to debate your latest hot take, find patient zero from a network of chats, etc.",
            HE.p_ "Chat experiments are optional to join and users are randomly invited for them.",
            HE.h2 [HA.id "melan"] "What does Melan in MelanChat stand for?",
            HE.p_ "Until someone finds a backronym, it is short for Melancia (watermelon in Portuguese). Not surprisingly, the app is full of them.",
            HE.h2_ "Recommend me some music",
            HE.p_ [
                  HE.text "Listen to ",
                  HE.a (HA.href "https://open.spotify.com/track/4klGcqccAwciiLlPL136Kl?si=kYRoPuV6S2utxNCT4j2h_Q") "Metá Metá",
                  HE.text "."
            ],
            HE.h2 [HA.id "canihelp"] "Wow, I love it. How can I help?",
            HE.p_ [
                  HE.text "Right?! If you can spare some, consider ",
                  HE.a (HA.href $ routes.backer {}) "backing MelanChat",
                  HE.text ". The entire ",
                  HE.a (HA.href "https://github.com/melanchat/melanchat") "source code",
                  HE.text " is also open source, in case this is your thing."
            ],
            HE.p_ "That being said, just letting others know about MelanChat is already a huge help. Reporting bugs, bad user behavior or other issues is also highly appreciated.",
            HE.div (HA.class' "hide-internal") [
                  HE.h2 [HA.id "other"] "I still have questions/doubts/something else to say",
                  HE.p_ "That's easy, send an email to contact@melan.chat"
            ]
      ]