#!/bin/bash
trap 'pkill -P $$; exit' SIGINT SIGTERM

docker-compose up &
bash_scripts/run_scala_class.sh "webAPI.Routes" &

wait
