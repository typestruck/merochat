# Feel free to bump versions in this file!

# Check for updates here: https://hub.docker.com/_/node
FROM node:22.17.0

# Install purescript and tooling.
# Check for updates here: https://www.npmjs.com
RUN npm install --global purescript@0.15.15 spago@0.21.0

# Bind mount your project directory here.
RUN mkdir /merochat
WORKDIR /merochat

# The parent image has this set to some script.
ENTRYPOINT []

# watchexec is not available in debian repositories.
# It would be in alpine but purescript fails to build there.
# So let's add this repo which does contain it.
# https://apt.cli.rs/
RUN curl -fsSL https://apt.cli.rs/pubkey.asc >> /usr/share/keyrings/rust-tools.asc
ADD https://apt.cli.rs/rust-tools.list /etc/apt/sources.list.d/rust-tools.list

RUN apt-get update && \
    apt-get install --assume-yes watchexec-cli && \
    rm -rf /var/lib/apt/lists/*

# Start the application and watch for changes.
# https://github.com/purescript/spago?tab=readme-ov-file#watch-mode
CMD watchexec --exts=purs,js,yaml --restart -- "spago build && \
    npx rsbuild build --config rsbuild.development.config.js && \
    spago run --main Server.Main"
