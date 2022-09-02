#!/bin/bash

dist_folder="dist/production"
copy_folder="distcopy"
environment_file="src/Shared/Environment.purs"
bucket="ourmelon"
css_suffix="CSSHash"
js_suffix='JSHash'

mkdir -p $dist_folder/a
mkdir -p $copy_folder
cp $dist_folder/* $copy_folder

npm run build-production-client
result=$?

if [ "$result" -gt 0 ]; then
  echo "FAILED"
  exit 1
fi

find $dist_folder -size 0 -delete

echo $'module Environment where

foreign import production :: Boolean' > $environment_file

for entry in `ls $dist_folder`
do
    file_name=`echo $entry | perl -nle 'm/^(.*?)\./; print $1'`
    hash=`echo $entry | perl -nle 'm/\.(.+?)\./; print $1'`
    if [ ${entry: -4} == ".css" ]
    then
        echo -en "\n$file_name$css_suffix :: String\n$file_name$css_suffix = \"$hash\"\n" >> $environment_file
    elif [ ${entry: -3} == ".js" ]
    then
        echo -en "\n$file_name$js_suffix :: String\n$file_name$js_suffix = \"$hash\"\n" >> $environment_file
    fi
done

for entry in `ls $dist_folder`
do
    if [ ! -f "$copy_folder/$entry" ]
    then
        echo "New file: $dist_folder/$entry"
        b2-linux upload-file $bucket "$PWD/$dist_folder/$entry" $entry
    fi
done

git add $environment_file
git commit -m 'Update hashs [skip ci]'
git push

rm -rf $copy_folder

sed -i -e 's/development = false/development = true/g' $environment_file