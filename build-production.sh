#!/bin/bash

npm install &&
rm -rf file/bundle &&
rm -rf output-es &&
spago -x production.dhall build --purs-args "--censor-codes=MissingTypeDeclaration,UnusedExplicitImport,ImplicitImport,UnusedImport --stash" &&
npx webpack --progress --config webpack.production.config.js &&
npx webpack --progress --config webpack.landing.config.js &&
tar -C /home/eduardo/Documents/merochat/ -c --zstd --exclude=file/upload --exclude=file/nsfw package.json file/ output-es/ loader/server.js | ssh melanchat@46.101.109.141 -t "cd /home/melanchat && rm -rf build/ && mkdir build && cd build && tar -x --zstd" &&
ssh -o ServerAliveInterval=60 melanchat@46.101.109.141

