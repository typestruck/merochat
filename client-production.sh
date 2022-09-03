#!/bin/bash

dist_folder="dist/production"
bundle_folder="bundle"
bucket="ourmelon"

npm run build-production-client && b2-linux sync --keepDays 7 --replaceNewer "$PWD/$dist_folder" "b2://$bucket/$bundle_folder"
