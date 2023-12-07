#!/bin/bash
trap 'pkill -P $$; exit' SIGINT SIGTERM

docker-compose up &
bash_scripts/run_scala_class.sh "webAPI.Routes" &
bash_scripts/process_submission.sh &

wait
