# Architecture

This is a high level overview of MeroChat sources.

## Project structure

Server side code (to be run with node.js) lies in src/Server. Likewise, client code (to be loaded by a browser) is located in src/Client. There is an extra folder under src namely Shared which contains code used by both server and client side.

### Client side

On client side there is a folder for each entry point. These entry points are bundled with webpack.

#### External pages

External pages are accessed by non logged in users (if a logged in user tries to access an external page, they will be redirected to /im). Each page has their own route, .e.g login is at /login, landing page is at /, password recovery is at /recover, etc, and a Main file that is compiled as the entry point.

External pages might use browsers API for DOM manipulation instead of a framework.

#### Internal pages

Internal pages require login to be accessed. Non logged users are redirected to /login upon trying to access them. However, there is only a single entry point exposed to the user, /im. The IM page lazy loads other entry points (like user settings, karma leaderboard or user profile edition) and renders them without URL changes.

IM uses purescript-flame as web framework. `Client.Im.Main` kickstarts the application, wires document/window events, handles websocket events and call the appropriated methods for each `ImMessage`. Most logical page divisions have its own file to process given `ImMessage`s, e.g., there is a contact list, history, chat etc module.

Lazy loaded pages also have their own `Main` module (e.g. `Client.Profile.Main`) that will handle events and message updates.

#### Miscellaneous

* Resource urls (like CSS or JS files) are not hardcoded, there is a function to set the correct path for production or development

* Most direct DOM manipulation is in Dom.purs and other files under src/Client/Common

* HTTP request are performed by the client generated by purescript-payload. Incorrect paths, missing parameters etc will be type errors

* All pages are server side rendered and only hydrated on client side

* Besides base, each entry point has it own css file (e.g., im.css, settings.css, etc)

### Server Side

The web server framework is purescript-payload. Routes and their query string, request body, response, access level (a.k.a logged in or not guard) are typed with the `Shared.Spec` module. The handler of each route is set at `Server.Handler`. Guards are at `Server.Guard`.

There is a folder for each route (e.g. /im, /landing etc). Each folder always has a `Handler` module for taking care of routes and other HTTP logic, an `Action` module for domain logic, a `Template` module for rendering markup, and sometimes a `Database` module for SQL queries.

#### Miscellaneous

* If a type is used by both server and client side, then it should be located under the folder Shared

* Pages are always rendered server side so `Template` modules use `view`s from the Shared folder

* Sessions are cookie based. The logged in user id is always available to `Handler` modules via `guards :: { loggedUserId :: Int }` in case of internal pages

* There is a server for HTTP requests and another for websockets. The websocket server uses a wrapper for the ws library