#!/bin/bash

npm install &&
rm -rf file/bundle &&
rm -rf output-es &&
spago -x production.dhall build &&
npx webpack --progress --config webpack.production.config.js &&
npx webpack --progress --config webpack.landing.config.js &&
pm2 restart server --update-env

