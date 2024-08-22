# Pinning the version for reproducibility.
FROM postgres:16.3@sha256:d0f363f8366fbc3f52d172c6e76bc27151c3d643b870e1062b4e8bfe65baf609

# The container will initialize the db on the first run according to this file.
ADD --chown=postgres:postgres ../src/Server/sql/index.sql /docker-entrypoint-initdb.d/init.sql
