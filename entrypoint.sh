#!/bin/bash

# docker container entry point script

curl -fsSL https://github.com/purescript/purescript/releases/download/v0.15.6/linux64.tar.gz -o /tmp/purescript-0.15.6.tar.gz
tar zxvf /tmp/purescript-0.15.6.tar.gz -C /tmp
mv /tmp/purescript/purs /bin
chmod +x /bin/purs
npm i -g spago
cd /merochat
npm i

cat > /bin/start-spago <<- EOM
#!/bin/bash
spago build --watch 2>&1 | tee spago.log
EOM

cat > /bin/start-npm <<-EOM
#!/bin/bash
echo 'waiting for spago to finish first build.......'
until tail -f spago.log | grep -m 1 'succeeded'; do : ; done
npm run watch
EOM

chmod +x /bin/start-spago /bin/start-npm

printf '\n\n[+] done. now run:\n'
printf '\tsudo docker exec -w /merochat -i merochat start-spago -- to start spago\n'
printf '\tsudo docker exec -w /merochat -i merochat start-npm   -- to start npm\n\n'
printf 'in two separate terminals\n\n'

tail -F /dev/null