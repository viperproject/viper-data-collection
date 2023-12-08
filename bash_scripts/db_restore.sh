#!/bin/bash

BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"

cat "$BASEDIR"/db/backups/$1 | docker exec -i postgres_db psql -U postgres -d programs
