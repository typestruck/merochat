# MelanChat

MelanChat is a friendly random chat.

## Random

* User profiles start off as auto generated, i.e., name, bio, avatar, etc are picked by [bender](https://github.com/easafe/bender) 

* Users to chat with are suggested by the app - besides skipping suggestions, it is not possible to search or filter potential new chats 

* Users may be invited to participate in novel chat experiments or features, similarly to April's Fool Reddit gimmicks

## Friendly

* Self moderation by Karma: ~~fake internet points~~ a numeric score of how much a given user is trusted by the community

* Features (e.g., profile visibility, sending audio or pictures, etc) are unlocked by gaining Karma

* Privacy: users may choose to temporarily suspend, delete or restrict their account at any given time

* Zero tolerance for bots, spammers or peverts

A note on naming: melan is short for _melancia_, the Portuguese word for watermelon. Hence the fruit theming.

## Contribuing

Pull requests are welcome! MelanChat is still in early stages of development, but we do have a [spec](docs/README.md). Issues correspond to spec items, but feel free to contact me (or just fork the thing) if you wish to help.  

MelanChat is written in PureScript, both server-side and client-side. It uses [purescript-flame](https://github.com/easafe/purescript-flame), [httpure](https://github.com/cprussin/purescript-httpure), [purescript-run](https://github.com/natefaubion/purescript-run) and PostgreSQL. To get it running locally:

* Run `npm install && bower install` 

* Configure PostgreSQL and run [index.sql](src/Server/sql/index.sql) (the user "melanchat" and databases "melanchat" and "melanchatTest" are expected) 

* Set configuration.json (see the [example configuration](configuration-example.json))

* Get [bender](https://github.com/easafe/bender) running locally or disable it via `useBender` in configuration.json

* Run `npm run watch` for a hot reloading server at http://localhost:8000
