#!/bin/bash

#hack to speed up landing page loading
file="output/Server.Landing.Handler/index.js"
link='\<link rel=\"stylesheet\" type=\"text\/css\" href=\"https\:\/\/static\.melan\.chat\/file\/ourmelon\/base\.*\.css\"\>'
css=$(<dist/production/style.css)
template=$(node -e 'import("./output/Server.Landing.Template/index.js").then(m => console.log(m.template()))')
openStyle='<style type=text/css>'
closeStyle='</style>'
payload=$(echo "${template/$link/"$openStyle""$css""$closeStyle"}")
handler="landing = function (v) {
    return Control_Applicative.pure(Run.applicativeRun)(M.Html('$payload'));
};"

sed -i '1s/^/import \* as M from "..\/Shared.ContentType\/index.js";\n/' $file

echo $handler >> $file
