# MelanChat

MelanChat is a random chat with emphasis on friendliness. By random is meant:

* User profiles start off with algorithmically generated info

* Users to chat with are randomly suggested by the app- besides skipping suggestions, it is not possible to search or filter potential new chats 

* Users may be allowed to participate in novel chat features(similar to April's Fool Reddit gimmicks)

and as importantly, friendliness is _enforced_ by:

* Self moderation by Karma, which ~~are fake internet points~~ is a numeric score of how much MelanChat trusts a given user: every feature on the app(e.g., profile visibility, sending audio or pictures, etc) is unlocked by reaching a certain amount of Karma

* Privacy: users may choose to temporarily suspend, delete or restrict their account at any given time

* Zero tolerance for bots, spammers or peverts

A note on naming: melan is short for _melancia_, the Portuguese word for watermelon. Hence why the fruit theming.

## Contribuing

Pull requests are welcome! I am still about to set up a proper workflow, but we do have a [spec](docs/README.md). Issues should be open to account for whats being done.

MelanChat is written in Smalltalk(purely for the irony of it), PostgreSQL, TypeScript and React. CI and Monticello packages are yet TODO, however you can run it on a development machine as follows:

* Get the [Pharo VM](https://pharo.org/) 

* Install the dependencies from [deps.st](deps.st)

* Configuration goes to config.json and config-test.json, which should be placed on the root path of the Pharo VM

* Set up and populate the database(s) with [index.sql](sql/index.sql)

* ```npm install``` will install React and the whole humdrum JavaScript enviroment alongside TypeScript

* By default, the app relies on [Bender](https://github.com/azafeh/bender) running locally: this can be disabled by setting useBender to false in config.json(and config-test.json)

* Evaluate ```Melanchat configure: 'config.json'``` in the Pharo VM 

* ```npm dev``` will build and watch the TypeScript assets
