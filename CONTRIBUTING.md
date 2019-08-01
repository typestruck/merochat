# Contributing

Glad you want to help! Mutual aid is ~~a factor of evolution~~ punk rock as it gets.

You may want to look at 

* [Milestones](https://github.com/easafe/melanchat/milestones)

* [Open issues](https://github.com/easafe/melanchat/issues)

* The [spec](docs/README.md)

Don't hesitate to [contact](https://github.com/easafe) if you need help, or want to discuss something before.

## Setup

MelanChat is written in PureScript, both server-side and client-side. It uses [purescript-flame](https://github.com/easafe/purescript-flame), [httpure](https://github.com/cprussin/purescript-httpure), [purescript-run](https://github.com/natefaubion/purescript-run) and PostgreSQL. To get it running locally:

* Run `npm install && bower install` 

* Configure PostgreSQL and run [index.sql](src/Server/sql/index.sql) (the user "melanchat" and databases "melanchat" and "melanchatTest" are expected) 

* Set configuration.json (see the [example configuration](configuration-example.json))

* Get [bender](https://github.com/easafe/bender) running locally or disable it via `useBender` in configuration.json

* Run `npm run watch` for a hot reloading server at http://localhost:8000

## Pull requests

Unless it is a small change, please include tests and a clear commit message. Bonus points if the code has documentary comments.
