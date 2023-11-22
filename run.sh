#!/bin/bash
trap 'pkill -P $$; exit' SIGINT SIGTERM

docker-compose up &
bash_scripts/webAPI.sh &

wait
