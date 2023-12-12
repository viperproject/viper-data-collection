#!/bin/bash
trap 'pkill -P $$; exit' SIGINT SIGTERM

mkdir -p tmp

CP_FILE="vdc_classpath.txt"
if [ ! -f "$CP_FILE" ]; then
  (
    sbt "export runtime:dependencyClasspath" | tail -n1 >"$CP_FILE"
  )
fi

docker-compose up &
bash_scripts/run_scala_class.sh "webAPI.Routes" &
bash_scripts/process_submission.sh &

wait
