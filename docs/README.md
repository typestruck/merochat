# Software Requirements Specification

Author: @easafe

## Overview

MelanChat is a gamified, self moderated, random chat application for platonic interactions.

Gamification is the technique of using game-like features(point scoring, achievements etc) to boost engagement; it is present in MelanChat via Badges, Trophies and Karma. Karma also doubles as a self moderation tool: features(such as sending images, audio, video etc) are only available after obtaining the necessary amount of Karma. Random, as in random chat, means that chat partners are suggested by the app. Such suggestions may be skipped, but not filtered in any way(such as by age, gender, location etc). Finally, MelanChat is not a dating or hookup app- the focus is amicable chatting.

MelanChat is a web application. While it works correctly in mobile browsers, a mobile app is outside of scope.

## Requirements

Requirements(definitions, business logic and mockups) for MelanChat are grouped as follows:

1.  [User](user/requirements.md)

    General management of accounts on the application

2.  [IM](im/requirements.md)

    The chat flow on the application

3.  [Gamification](gamification/requirements.md)

    Game-like features present on the application

4.  [Help](help/requirements.md)

    Explanations about application features and FAQ

<!--

brain dump

1. **Random**

    c) A bot to chat with, get conversations starters from, redo users profile

2. **Friendly**

    c) Comprehensive help

3. **Self moderated**

    b) Users with high enough karma take part on moderation tasks

6. **Crowdfunded**

    a) Users may donate in a recurring fashion(e.g., patreon)

    b) Users may make an one time donation(e.g., paypal)
-->
