#!/bin/bash

#hack to speed up landing page loading
file="output-es/Server.Landing.Handler/index.js"
link='\<link rel=\"stylesheet\" type=\"text\/css\" href=\"https\:\/\/static\.melan\.chat\/file\/ourmelon\/base\.*\.css\"\>'
css=$(<dist/production/style.css)
template=$(node -e 'import("./output-es/Server.Landing.Template/index.js").then(m => console.log(m.template()))')
openStyle='<style type=text/css>'
closeStyle='</style>'
payload=$(echo "${template/$link/"$openStyle""$css""$closeStyle"}")
handler="function templateHack(v) {
    return M.Html('$payload');
};"

sed -i -e 's/const landing = v => Server$dResponse.serveTemplate(Server$dLanding$dTemplate.template);/const landing = v => Server$dResponse.serveTemplate(templateHack);/' $file

echo $handler >> $file
