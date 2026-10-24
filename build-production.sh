#!/bin/bash

npm install &&
rm -rf file/bundle &&
rm -rf output-es &&
spago build &&
npx purs-backend-es build --directives build-directives &&
npm run build &&
tar -C /home/eduardo/Documents/merochat/ -c --zstd --exclude=file/upload --exclude=file/nsfw package.json file/ output-es/ loader/server.js | ssh melanchat@46.101.109.141 -t "cd /home/melanchat && rm -rf build/ && mkdir build && cd build && tar -x --zstd" &&
ssh -o ServerAliveInterval=60 melanchat@46.101.109.141

