#!/bin/bash

#hack to speed up landing page loading
file="output/Server.Landing.Handler/index.js"
link='\<link rel=\"stylesheet\" type=\"text\/css\" href=\"https\:\/\/static\.melan\.chat\/file\/ourmelon\/base\.*\.css\"\>'
css=$(<dist/production/style.css)
template=$(node -e 'console.log(require("./output/Server.Landing.Template/index.js").template())')
openStyle='<style type=text/css>'
closeStyle='</style>'
payload=$(echo "${template/$link/"$openStyle""$css""$closeStyle"}")
handler="module.exports.landing = function(v) {
    return Control_Applicative.pure(Run.applicativeRun)(require(\"../Shared.ContentType/index.js\").Html('$payload'));
};"

echo $handler >> $file
