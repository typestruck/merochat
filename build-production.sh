#!/bin/bash

dist_folder="dist/production"
bundle_folder="bundle"
bucket="ourmelon"

npm install &&
rm -rf dist/production &&
rm -rf output-es &&
spago -x production.dhall build &&
npx webpack --progress --config webpack.production.config.js &&
npx webpack --progress --config webpack.landing.config.js &&
b2-linux sync --keepDays 7 --replaceNewer "$PWD/$dist_folder" "b2://$bucket/$bundle_folder" &&
pm2 restart server --update-env
