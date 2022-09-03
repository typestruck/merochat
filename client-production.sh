#!/bin/bash

dist_folder="dist/production"
bundle_folder="bundle"
bucket="ourmelon"

npm run build-production-client
result=$?

if [ "$result" -gt 0 ]; then
  echo "BUILD FAILED"
  exit 1
fi

b2-linux sync --keepDays 7 --replaceNewer "$PWD/$dist_folder" "b2://$bucket/$bundle_folder"
