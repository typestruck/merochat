#!/bin/bash

#hack to speed up landing page loading
file="output-es/Server.Landing.Handler/index.js"
payload=$(node -e 'import("./output-es/Server.Landing.Template/index.js").then(m => console.log(m.template()))')
handler="function templateHack(v) {
    return M.Html('$payload');
};"

sed -i '1s/^/import \* as M from "..\/Shared.ContentType\/index.js";\n/' $file
sed -i -e 's/const landing = v => Server$dResponse.serveTemplate(Server$dLanding$dTemplate.template);/const landing = v => Server$dResponse.serveTemplate(templateHack);/' $file

echo $handler >> $file
