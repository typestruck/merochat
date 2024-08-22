# Contributing

Glad you want to help! Mutual aid is ~~a factor of evolution~~ punk rock as it gets.

You may want to look at

* The [spec](docs/README.md)

* [Open issues](https://github.com/typestruck/merochat/issues)

* [Milestones](https://github.com/typestruck/merochat/milestones)

Don't hesitate to [contact](https://github.com/easafe) if you need help, or want to discuss something before.

## Setup

MeroChat is written in PureScript, both server-side and client-side. PostgreSQL is used for persistance.


### Containers

You can run MeroChat locally with podman or docker.

Just run `podman-compose -f containers/composefile.yaml up` to spin up the containers.
Your very own MeroChat is now accessible at http://localhost:8000, and will be rebuilt when you make changes to the code. This isn't instantaneous though. Keep an eye at the container runtime output.

Finally tear the containers down with `ctr+c` followed by `podman-compose -f containers/composefile.yaml down`



### Manual

No containers for you? 

1. Run `npm install && spago build`

2. Configure PostgreSQL and run [index.sql](src/Server/sql/index.sql). The user `merochat`, databases `merochat` and `merochat_test` are expected (ident or trust authentication)

3. Run `npm run watch` to start a server at http://localhost:8000

    * [purescript-ide](https://github.com/nwolverson/vscode-ide-purescript) is the fast and easy way to rebuild the project. Alternatively, run `spago build --watch` in a new terminal

    * Refresh the page after any changes :)


## Pull requests

Unless it is a small change, please include tests and a clear commit message. Bonus points if the code has documentation comments.
