#!/bin/bash

BASEDIR="$(dirname -- "$(dirname -- "$(realpath -- "$0")")")"

docker exec -t postgres_db pg_dumpall -c -U postgres >"$BASEDIR"/db/backups/dump_$(date +"%Y-%m-%d_%H_%M_%S").sql
