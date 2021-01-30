# Contributing

Glad you want to help! Mutual aid is ~~a factor of evolution~~ punk rock as it gets.

You may want to look at

* [Milestones](https://github.com/easafe/melanchat/milestones)

* [Open issues](https://github.com/easafe/melanchat/issues)

* The [spec](docs/README.md)

Don't hesitate to [contact](https://github.com/easafe) if you need help, or want to discuss something before.

## Setup

MelanChat is written in PureScript, both server-side and client-side. PostgreSQL is used for persistance. To get it running locally:

1. Run `npm install && npm run build`

2. Configure PostgreSQL and run [index.sql](src/Server/sql/index.sql). The user `melanchat`, databases `melanchat` and `melanchat_test` are expected (ident or trust authentication)

3. Run `npm run watch` to start a server at http://localhost:8000

    * [purescript-ide](https://github.com/nwolverson/vscode-ide-purescript) is the fast and easy way to rebuild the project. Alternatively, run `spago build --watch` in a new terminal

    * Refresh the page after any changes :)

4. (Optional) Enable randomized profile creation

    * Set the enviroment variable `RANDOMIZE_PROFILES` (e.g., `export RANDOMIZE_PROFILES=true`)

    * You may need to build the native module ([3000.node](3000.node)) for your system. The source is [here](https://github.com/melanchat/3000). Detailed instructions on how to compile the module can be found [here](https://neon-bindings.com)

## Pull requests

Unless it is a small change, please include tests and a clear commit message. Bonus points if the code has documentary comments.
