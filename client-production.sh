#!/bin/bash

dist_folder="dist/production"
copy_folder="distcopy"
production_file="production/Environment.purs"
development_file="development/Environment.purs"
bucket="ourmelon"
css_suffix="CSSHash"
js_suffix='JSHash'
#remote_files=`b2-linux ls ourmelon --long`

mkdir -p $copy_folder
cp $dist_folder/* $copy_folder

npm run build-production-client
find $dist_folder -size 0 -delete

echo $'module Environment where\n\ndevelopment :: Boolean\ndevelopment = false\n' > $production_file
echo $'module Environment where\n\ndevelopment :: Boolean\ndevelopment = true\n' > $development_file

for entry in `ls $dist_folder`
do
    file_name=`echo $entry | perl -nle 'm/^(.*?)\./; print $1'`
    hash=`echo $entry | perl -nle 'm/\.(.+?)\./; print $1'`
    if [ ${entry: -4} == ".css" ]
    then
        echo -en "\n$file_name$css_suffix :: String\n$file_name$css_suffix = \"$hash\"\n" >> $production_file
        echo -en "\n$file_name$css_suffix :: String\n$file_name$css_suffix = \"$hash\"\n" >> $development_file
    elif [ ${entry: -3} == ".js" ]
    then
        echo -en "\n$file_name$js_suffix :: String\n$file_name$js_suffix = \"$hash\"\n" >> $production_file
        echo -en "\n$file_name$js_suffix :: String\n$file_name$js_suffix = \"$hash\"\n" >> $development_file
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

# might be better to wait some before deleting older files
# for entry in `ls $copy_folder`
# do
#     if [ ! -f "$dist_folder/$entry" ]
#     then
#         echo "Deleted file: $dist_folder/$entry"
#     fi
# done

#b2-linux

git add $production_file
git add $development_file
git commit -m 'Updated hashs'
git push

rm -rf $copy_folder

